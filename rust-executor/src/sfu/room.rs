//! Room and session management for the SFU.
//!
//! A room maps 1:1 to a call session within a neighbourhood.
//! Rooms are keyed by (neighbourhood URL, room ID).
//!
//! A participant is one session: an agent's DID on one device. The same
//! agent on another device is another participant; a join from the same
//! device (another browser tab) replaces the earlier session.

use std::collections::HashMap;
use std::fmt;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Instant;
use std::time::SystemTime;

/// Unique identifier for a participant in the SFU.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParticipantId(pub u64);

impl ParticipantId {
    pub fn next() -> Self {
        static COUNTER: AtomicU64 = AtomicU64::new(1);
        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }

    /// Read back the `Display` form (`P{n}`) a join returned.
    pub fn parse(s: &str) -> Option<Self> {
        s.strip_prefix('P')?.parse().ok().map(Self)
    }
}

/// Most sessions one DID holds in one room: one per device.
pub const MAX_DEVICES_PER_DID: usize = 4;

impl fmt::Display for ParticipantId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "P{}", self.0)
    }
}

/// Unique identifier for a room, composed of neighbourhood URL and room name.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RoomId {
    pub neighbourhood_url: String,
    pub room_name: String,
}

impl RoomId {
    pub fn new(neighbourhood_url: impl Into<String>, room_name: impl Into<String>) -> Self {
        Self {
            neighbourhood_url: neighbourhood_url.into(),
            room_name: room_name.into(),
        }
    }

    /// Parse a `RoomId` from its `Display` format
    /// (`{neighbourhood_url}:{room_name}`).
    ///
    /// Neighbourhood URLs contain their own `://`, so the split uses
    /// the LAST colon.  When no colon appears, the entire string
    /// becomes the neighbourhood URL and the room name defaults to
    /// `"default"`.
    pub fn parse(s: &str) -> Self {
        let (nh_url, room_name) = s.rsplit_once(':').unwrap_or((s, "default"));
        Self::new(nh_url, room_name)
    }
}

impl fmt::Display for RoomId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.neighbourhood_url, self.room_name)
    }
}

/// Information about a participant in a room.
#[derive(Debug, Clone)]
pub struct ParticipantInfo {
    pub id: ParticipantId,
    pub agent_did: String,
    /// The device the session joined from, as the client named it.
    pub device_id: String,
    pub joined_at: Instant,
    pub has_audio: bool,
    pub has_video: bool,
    pub is_active_speaker: bool,
    /// Joined with `acceptRedirect`: it can reach other cascade nodes, so a
    /// rebalance may move it. Other clients reach only this node.
    pub accepts_redirect: bool,
}

/// A call room managed by the SFU.
#[derive(Debug)]
pub struct SfuRoom {
    pub id: RoomId,
    pub participants: HashMap<ParticipantId, ParticipantInfo>,
    /// Epoch millis when the room was created.
    pub created_at: u64,
    pub max_participants: Option<usize>,
}

impl SfuRoom {
    pub fn new(id: RoomId, max_participants: Option<usize>) -> Self {
        Self {
            id,
            participants: HashMap::new(),
            created_at: SystemTime::now()
                .duration_since(SystemTime::UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis() as u64,
            max_participants,
        }
    }

    /// Add the session `pid` of `agent_did` on `device_id`. A session of the
    /// same DID on the same device leaves the room for it, and its id is
    /// returned: the caller drops its peer and tells it so. It does not count
    /// against the room's capacity or the DID's devices.
    pub fn add_participant(
        &mut self,
        pid: ParticipantId,
        agent_did: String,
        device_id: String,
    ) -> Result<Option<ParticipantId>, RoomError> {
        let replaced = self
            .participants
            .values()
            .find(|p| p.agent_did == agent_did && p.device_id == device_id)
            .map(|p| p.id.clone());
        let staying = |p: &&ParticipantInfo| Some(&p.id) != replaced.as_ref();

        if let Some(max) = self.max_participants {
            if self.participants.values().filter(staying).count() >= max {
                return Err(RoomError::RoomFull);
            }
        }
        let devices = self
            .participants
            .values()
            .filter(staying)
            .filter(|p| p.agent_did == agent_did)
            .count();
        if devices >= MAX_DEVICES_PER_DID {
            return Err(RoomError::TooManyDevices);
        }

        if let Some(old) = &replaced {
            self.participants.remove(old);
        }
        self.participants.insert(
            pid.clone(),
            ParticipantInfo {
                id: pid,
                agent_did,
                device_id,
                joined_at: Instant::now(),
                has_audio: false,
                has_video: false,
                is_active_speaker: false,
                accepts_redirect: false,
            },
        );

        Ok(replaced)
    }

    /// Mark a participant as able to follow a redirect to another node.
    pub fn mark_redirectable(&mut self, pid: &ParticipantId) {
        if let Some(p) = self.participants.get_mut(pid) {
            p.accepts_redirect = true;
        }
    }

    /// Remove a participant from the room. Returns true if the room is now empty.
    pub fn remove_participant(&mut self, pid: &ParticipantId) -> bool {
        self.participants.remove(pid);
        self.participants.is_empty()
    }

    pub fn participant_count(&self) -> usize {
        self.participants.len()
    }

    pub fn is_empty(&self) -> bool {
        self.participants.is_empty()
    }

    /// The session `participant_id` names, when it is in the room and is
    /// `did`'s: a caller acts on its own sessions only, never on another
    /// session of its DID by accident or on someone else's.
    pub fn session_of(&self, participant_id: &str, did: &str) -> Option<ParticipantId> {
        let pid = ParticipantId::parse(participant_id)?;
        self.participants
            .get(&pid)
            .filter(|p| p.agent_did == did)
            .map(|p| p.id.clone())
    }

    /// Whether `did` is in the room, on any device.
    pub fn has_did(&self, did: &str) -> bool {
        self.participants.values().any(|p| p.agent_did == did)
    }
}

/// Room management errors.
#[derive(Debug, Clone)]
pub enum RoomError {
    RoomFull,
    /// The DID is in the room from [`MAX_DEVICES_PER_DID`] other devices.
    TooManyDevices,
    NotFound,
}

impl fmt::Display for RoomError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RoomError::RoomFull => write!(f, "Room is full"),
            RoomError::TooManyDevices => write!(
                f,
                "Already in this call from {} devices: leave on one of them first",
                MAX_DEVICES_PER_DID
            ),
            RoomError::NotFound => write!(f, "Room not found"),
        }
    }
}

impl std::error::Error for RoomError {}

/// Manages all active rooms.
#[derive(Debug, Default)]
pub struct RoomManager {
    rooms: HashMap<RoomId, SfuRoom>,
}

impl RoomManager {
    pub fn new() -> Self {
        Self {
            rooms: HashMap::new(),
        }
    }

    /// Create a new room. Returns error if room already exists.
    pub fn create_room(
        &mut self,
        id: RoomId,
        max_participants: Option<usize>,
    ) -> Result<&SfuRoom, RoomError> {
        if self.rooms.contains_key(&id) {
            // Room already exists — return it (idempotent)
            return Ok(&self.rooms[&id]);
        }
        self.rooms
            .insert(id.clone(), SfuRoom::new(id.clone(), max_participants));
        Ok(&self.rooms[&id])
    }

    /// Destroy a room, removing all participants.
    pub fn destroy_room(&mut self, id: &RoomId) -> Result<Vec<ParticipantId>, RoomError> {
        match self.rooms.remove(id) {
            Some(room) => Ok(room.participants.keys().cloned().collect()),
            None => Err(RoomError::NotFound),
        }
    }

    /// Get a room by ID.
    pub fn get_room(&self, id: &RoomId) -> Option<&SfuRoom> {
        self.rooms.get(id)
    }

    /// Get a mutable room by ID.
    pub fn get_room_mut(&mut self, id: &RoomId) -> Option<&mut SfuRoom> {
        self.rooms.get_mut(id)
    }

    /// List all active rooms.
    pub fn list_rooms(&self) -> Vec<&SfuRoom> {
        self.rooms.values().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn join(
        room: &mut SfuRoom,
        did: &str,
        device: &str,
    ) -> Result<(ParticipantId, Option<ParticipantId>), RoomError> {
        let pid = ParticipantId::next();
        let replaced = room.add_participant(pid.clone(), did.to_string(), device.to_string())?;
        Ok((pid, replaced))
    }

    #[test]
    fn test_room_lifecycle() {
        let mut mgr = RoomManager::new();
        let room_id = RoomId::new("neighbourhood://test", "room1");

        // Create room
        mgr.create_room(room_id.clone(), Some(4)).unwrap();
        assert_eq!(mgr.list_rooms().len(), 1);

        let room = mgr.get_room_mut(&room_id).unwrap();
        let (p1, _) = join(room, "did:key:z6Mk1", "laptop").unwrap();
        let (p2, _) = join(room, "did:key:z6Mk2", "laptop").unwrap();
        assert_eq!(room.participant_count(), 2);

        // Remove participant
        room.remove_participant(&p1);
        assert_eq!(room.participant_count(), 1);

        // Remove last participant — room still exists (manager handles cleanup)
        let room = mgr.get_room_mut(&room_id).unwrap();
        room.remove_participant(&p2);
        assert!(room.is_empty());
    }

    #[test]
    fn test_room_capacity() {
        let mut room = SfuRoom::new(RoomId::new("neighbourhood://test", "small"), Some(2));
        join(&mut room, "did:key:z6MkA", "laptop").unwrap();
        join(&mut room, "did:key:z6MkB", "laptop").unwrap();

        assert!(matches!(
            join(&mut room, "did:key:z6MkC", "laptop"),
            Err(RoomError::RoomFull)
        ));
        // A join that replaces a session takes its place: a full room still takes it.
        let (_, replaced) = join(&mut room, "did:key:z6MkA", "laptop").unwrap();
        assert!(replaced.is_some());
        assert_eq!(room.participant_count(), 2);
    }

    /// Another tab of one browser: the newest join wins.
    #[test]
    fn the_same_did_on_the_same_device_replaces_its_session() {
        let mut room = SfuRoom::new(RoomId::new("n", "r"), Some(8));
        let (first, _) = join(&mut room, "did:a", "laptop").unwrap();
        let (second, replaced) = join(&mut room, "did:a", "laptop").unwrap();

        assert_eq!(replaced, Some(first.clone()));
        assert_eq!(room.participant_count(), 1);
        assert!(!room.participants.contains_key(&first));
        assert_eq!(room.participants[&second].device_id, "laptop");
    }

    /// A phone and a laptop: both are in the call.
    #[test]
    fn the_same_did_on_another_device_is_another_participant() {
        let mut room = SfuRoom::new(RoomId::new("n", "r"), Some(8));
        let (laptop, _) = join(&mut room, "did:a", "laptop").unwrap();
        let (phone, replaced) = join(&mut room, "did:a", "phone").unwrap();

        assert_eq!(replaced, None);
        assert_eq!(room.participant_count(), 2);
        assert!(room.participants.contains_key(&laptop));
        assert!(room.participants.contains_key(&phone));
        // Another DID on a device of the same name is someone else entirely.
        let (_, replaced) = join(&mut room, "did:b", "laptop").unwrap();
        assert_eq!(replaced, None);
        assert_eq!(room.participant_count(), 3);
    }

    #[test]
    fn a_did_holds_a_bounded_number_of_devices_in_a_room() {
        let mut room = SfuRoom::new(RoomId::new("n", "r"), None);
        for device in 0..MAX_DEVICES_PER_DID {
            join(&mut room, "did:a", &format!("device-{device}")).unwrap();
        }
        assert!(matches!(
            join(&mut room, "did:a", "one-too-many"),
            Err(RoomError::TooManyDevices)
        ));
        // Rejoining from a device already in the call still works, and others still join.
        assert!(join(&mut room, "did:a", "device-0").unwrap().1.is_some());
        join(&mut room, "did:b", "device-0").unwrap();
        assert_eq!(room.participant_count(), MAX_DEVICES_PER_DID + 1);
    }

    /// A call about a session acts only on that session, and only for its own DID.
    #[test]
    fn a_session_is_found_only_by_its_own_did() {
        let mut room = SfuRoom::new(RoomId::new("n", "r"), None);
        let (laptop, _) = join(&mut room, "did:a", "laptop").unwrap();
        let (phone, _) = join(&mut room, "did:a", "phone").unwrap();
        let (bobs, _) = join(&mut room, "did:b", "laptop").unwrap();

        assert_eq!(
            room.session_of(&laptop.to_string(), "did:a"),
            Some(laptop.clone())
        );
        assert_eq!(room.session_of(&phone.to_string(), "did:a"), Some(phone));
        assert_eq!(room.session_of(&bobs.to_string(), "did:a"), None);
        assert_eq!(room.session_of(&laptop.to_string(), "did:b"), None);
        assert_eq!(room.session_of("P999999999", "did:a"), None);
        assert_eq!(room.session_of("", "did:a"), None);
        assert_eq!(room.session_of(&laptop.0.to_string(), "did:a"), None);
    }

    #[test]
    fn a_participant_id_reads_back_from_its_display_form() {
        let pid = ParticipantId::next();
        assert_eq!(ParticipantId::parse(&pid.to_string()), Some(pid));
        assert_eq!(ParticipantId::parse("P"), None);
        assert_eq!(ParticipantId::parse("Q1"), None);
        assert_eq!(ParticipantId::parse("P-1"), None);
    }
}
