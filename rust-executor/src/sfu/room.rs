//! Room and session management for the SFU.
//!
//! A room maps 1:1 to a call session within a neighbourhood.
//! Rooms are keyed by (neighbourhood URL, room ID).

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
}

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
    pub joined_at: Instant,
    pub has_audio: bool,
    pub has_video: bool,
    pub is_active_speaker: bool,
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

    /// Add a participant to the room. Returns error if room is full or DID already present.
    pub fn add_participant(
        &mut self,
        pid: ParticipantId,
        agent_did: String,
    ) -> Result<(), RoomError> {
        // Check capacity
        if let Some(max) = self.max_participants {
            if self.participants.len() >= max {
                return Err(RoomError::RoomFull);
            }
        }

        // Check for duplicate DID
        if self.participants.values().any(|p| p.agent_did == agent_did) {
            return Err(RoomError::AlreadyJoined);
        }

        self.participants.insert(
            pid.clone(),
            ParticipantInfo {
                id: pid,
                agent_did,
                joined_at: Instant::now(),
                has_audio: false,
                has_video: false,
                is_active_speaker: false,
            },
        );

        Ok(())
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

    /// Find a participant's ID by their agent DID.
    pub fn participant_id_for_did(&self, did: &str) -> Option<ParticipantId> {
        self.participants
            .iter()
            .find(|(_, p)| p.agent_did == did)
            .map(|(pid, _)| pid.clone())
    }
}

/// Room management errors.
#[derive(Debug, Clone)]
pub enum RoomError {
    RoomFull,
    AlreadyJoined,
    NotFound,
}

impl fmt::Display for RoomError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RoomError::RoomFull => write!(f, "Room is full"),
            RoomError::AlreadyJoined => write!(f, "Agent already joined this room"),
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

    #[test]
    fn test_room_lifecycle() {
        let mut mgr = RoomManager::new();
        let room_id = RoomId::new("neighbourhood://test", "room1");

        // Create room
        mgr.create_room(room_id.clone(), Some(4)).unwrap();
        assert_eq!(mgr.list_rooms().len(), 1);

        // Add participants
        let p1 = ParticipantId::next();
        let p2 = ParticipantId::next();

        let room = mgr.get_room_mut(&room_id).unwrap();
        room.add_participant(p1.clone(), "did:key:z6Mk1".to_string())
            .unwrap();
        room.add_participant(p2.clone(), "did:key:z6Mk2".to_string())
            .unwrap();
        assert_eq!(room.participant_count(), 2);

        // Duplicate DID rejected
        let p3 = ParticipantId::next();
        assert!(matches!(
            room.add_participant(p3, "did:key:z6Mk1".to_string()),
            Err(RoomError::AlreadyJoined)
        ));

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
        let mut mgr = RoomManager::new();
        let room_id = RoomId::new("neighbourhood://test", "small");
        mgr.create_room(room_id.clone(), Some(2)).unwrap();

        let room = mgr.get_room_mut(&room_id).unwrap();
        let p1 = ParticipantId::next();
        let p2 = ParticipantId::next();
        let p3 = ParticipantId::next();

        room.add_participant(p1, "did:key:z6MkA".to_string())
            .unwrap();
        room.add_participant(p2, "did:key:z6MkB".to_string())
            .unwrap();

        assert!(matches!(
            room.add_participant(p3, "did:key:z6MkC".to_string()),
            Err(RoomError::RoomFull)
        ));
    }
}
