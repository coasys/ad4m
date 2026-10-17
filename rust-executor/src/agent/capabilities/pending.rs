//! A grant in its `Requested` state: an app's capability request for one agent, from
//! `agent.requestCapability` until the app redeems its code (`agent.generateJwt`), runs out of
//! code attempts, or expires.
//!
//! Requests live in memory only. Anyone may request a capability, anonymous callers included,
//! so a request must give them no write path to disk; a restart forgets the pending ones and
//! their apps ask again. Only redemption writes: it mints the grant ([`crate::agent::grants`]).

use super::types::AuthInfo;
use crate::agent::AgentContext;
use crate::secret::Secret;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

/// The arrival order of the next request.
static NEXT_SEQ: AtomicU64 = AtomicU64::new(0);

/// Wrong codes a request may receive before it is deleted. The code has six digits, so
/// without a limit anyone who learned a request id could try every code.
pub const MAX_CODE_ATTEMPTS: u8 = 5;

/// Seconds a request for the main agent waits: from the request to the approval, and again
/// from the approval to the redemption. Its operator is usually at the executor's launcher.
pub const MAIN_AGENT_REQUEST_TTL_SECONDS: u64 = 10 * 60;

/// Seconds a request for a managed agent waits (D15): its user may approve from an email.
pub const MANAGED_AGENT_REQUEST_TTL_SECONDS: u64 = 24 * 60 * 60;

/// Requests held at most. Anyone may request a capability, so past this a new request evicts
/// an unapproved one instead of growing memory ([`PendingRequests::request`]): first one for
/// no agent, then one for the new request's own agent; an approved one never.
pub const MAX_PENDING_REQUESTS: usize = 1024;

/// Requests held at most for one target (an agent, or all emails without one together). Past
/// this a new request evicts the oldest unapproved one for the same target, so a flood aimed at
/// one agent stays within its share and leaves other agents' requests alone.
pub const MAX_PENDING_REQUESTS_PER_TARGET: usize = 64;

/// The agent a capability request is for (D16: an agent is its DID).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum RequestTarget {
    /// An agent on this executor: the main agent, or a managed agent by its DID.
    Agent(AgentContext),
    /// An email that no user of this executor has, so no agent. The request is taken all the
    /// same, so that the answer does not tell the caller which emails have accounts. Its
    /// approval request reaches nobody, only the operator may approve it, its code redeems
    /// nothing, and it expires like a request for a managed agent.
    NoAgent,
}

impl RequestTarget {
    /// Whether the request is for the main agent.
    pub fn is_main_agent(&self) -> bool {
        matches!(self, RequestTarget::Agent(agent) if agent.is_main_agent())
    }

    /// The agent the request is for; `None` for an email without one.
    pub fn agent(&self) -> Option<&AgentContext> {
        match self {
            RequestTarget::Agent(agent) => Some(agent),
            RequestTarget::NoAgent => None,
        }
    }
}

impl From<AgentContext> for RequestTarget {
    fn from(agent: AgentContext) -> Self {
        RequestTarget::Agent(agent)
    }
}

/// How long a request for `target` waits, from the request and again from the approval.
pub fn request_ttl(target: &RequestTarget) -> u64 {
    if target.is_main_agent() {
        MAIN_AGENT_REQUEST_TTL_SECONDS
    } else {
        MANAGED_AGENT_REQUEST_TTL_SECONDS
    }
}

/// Whether `code` has the form of an approval code: exactly six ASCII digits. Only an owner's
/// approval sets a code, and it is always six digits; no request is ever approved with an
/// empty or otherwise shaped code, and none redeems with one.
pub fn is_code(code: &str) -> bool {
    code.len() == 6 && code.bytes().all(|b| b.is_ascii_digit())
}

/// One capability request.
struct PendingRequest {
    /// What the app asked for; after the approval, what the approver granted.
    auth: AuthInfo,
    /// The agent the grant will act as. Fixed at the request: an approval cannot move it.
    target: RequestTarget,
    /// Set by the approval; goes to the approver only. Always six digits ([`is_code`]).
    code: Option<Secret>,
    attempts: u8,
    /// Order of arrival: a request with a smaller one is older.
    seq: u64,
    /// Unix seconds.
    expires_at: u64,
}

/// Why a code was not redeemed.
#[derive(Debug, PartialEq, Eq)]
pub enum RedeemError {
    /// No request has this id, or no one has approved it yet. Records nothing.
    NotPermitted,
    /// The request expired; it is gone.
    Expired,
    /// The code is wrong; the request counts the attempt.
    WrongCode,
    /// The last allowed wrong code; the request is gone.
    TooManyWrongCodes,
    /// The code is right but the grant could not be minted (a locked wallet, a database that
    /// did not take the grant). The request stays approved, and the same code redeems later.
    Mint(String),
}

impl std::fmt::Display for RedeemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RedeemError::NotPermitted | RedeemError::WrongCode => {
                f.write_str("Can't find permitted request")
            }
            RedeemError::Expired => f.write_str("This request expired; request access again"),
            RedeemError::TooManyWrongCodes => {
                f.write_str("Too many wrong codes for this request; request access again")
            }
            RedeemError::Mint(e) => f.write_str(e),
        }
    }
}

/// Pending requests by request id. Every request, approval and redemption also deletes the
/// expired ones, so no separate sweeper runs.
#[derive(Default)]
pub struct PendingRequests(Mutex<HashMap<String, PendingRequest>>);

lazy_static! {
    static ref PENDING: PendingRequests = PendingRequests::default();
}

impl PendingRequests {
    /// The executor's requests.
    pub fn global() -> &'static PendingRequests {
        &PENDING
    }

    fn lock(&self) -> std::sync::MutexGuard<'_, HashMap<String, PendingRequest>> {
        self.0
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    fn lock_swept(&self, now: u64) -> std::sync::MutexGuard<'_, HashMap<String, PendingRequest>> {
        let mut pending = self.lock();
        pending.retain(|_, request| request.expires_at > now);
        pending
    }

    /// Records a new request of the app `auth` for the agent `target`. When the target already
    /// has [`MAX_PENDING_REQUESTS_PER_TARGET`] requests, or all targets together
    /// [`MAX_PENDING_REQUESTS`], an unapproved one goes, oldest first: one for the same target,
    /// or, when all are full, one for no agent, then one for the same target, then (for an
    /// agent below its share) one of the target holding the most unapproved requests, so that
    /// filling the store for a few agents does not refuse every other one. Approved requests
    /// stay, so `Err` when nothing may go. A request for no agent that may take nothing is
    /// dropped and answered `Ok` while an agent's request would be, so the two look alike.
    pub fn request(
        &self,
        request_id: String,
        auth: AuthInfo,
        target: impl Into<RequestTarget>,
        now: u64,
    ) -> Result<(), String> {
        let target = target.into();
        let mut pending = self.lock_swept(now);
        let oldest_unapproved =
            |pending: &HashMap<String, PendingRequest>, of: &dyn Fn(&RequestTarget) -> bool| {
                pending
                    .iter()
                    .filter(|(_, request)| request.code.is_none() && of(&request.target))
                    .min_by_key(|(_, request)| request.seq)
                    .map(|(id, _)| id.clone())
            };
        let same_target = |t: &RequestTarget| *t == target;
        let for_target = pending.values().filter(|r| same_target(&r.target)).count();
        if for_target >= MAX_PENDING_REQUESTS_PER_TARGET {
            if let Some(id) = oldest_unapproved(&pending, &same_target) {
                pending.remove(&id);
            }
        }
        if pending.len() >= MAX_PENDING_REQUESTS {
            let below_share = for_target < MAX_PENDING_REQUESTS_PER_TARGET;
            let fullest = || {
                if target == RequestTarget::NoAgent || !below_share {
                    return None;
                }
                let mut unapproved: HashMap<&RequestTarget, usize> = HashMap::new();
                for request in pending.values().filter(|r| r.code.is_none()) {
                    *unapproved.entry(&request.target).or_default() += 1;
                }
                let own = unapproved.get(&target).copied().unwrap_or(0);
                let most = unapproved
                    .values()
                    .copied()
                    .max()
                    .filter(|most| *most > own)?;
                oldest_unapproved(&pending, &|t| unapproved.get(t) == Some(&most))
            };
            let evicted = oldest_unapproved(&pending, &|t| *t == RequestTarget::NoAgent)
                .or_else(|| oldest_unapproved(&pending, &same_target))
                .or_else(fullest);
            match evicted {
                Some(id) => {
                    pending.remove(&id);
                }
                // A request for no agent gets the answer a request for an agent with nothing
                // waiting gets, which takes one from the fullest target while any request is
                // unapproved: refusing it alone would tell the caller that the email has no
                // account (D16). It reaches no one, so it is dropped rather than stored, and
                // redeeming its id answers as for an unapproved one.
                None if target == RequestTarget::NoAgent
                    && pending.values().any(|r| r.code.is_none()) =>
                {
                    return Ok(());
                }
                None => {
                    return Err(
                        "Too many capability requests are waiting; try again later".to_string()
                    )
                }
            }
        }
        let expires_at = now + request_ttl(&target);
        pending.insert(
            request_id,
            PendingRequest {
                auth,
                target,
                code: None,
                attempts: 0,
                seq: NEXT_SEQ.fetch_add(1, Ordering::Relaxed),
                expires_at,
            },
        );
        Ok(())
    }

    /// The agent that the unexpired request `request_id` is for: whoever approves it must own
    /// that agent.
    pub fn target(&self, request_id: &str, now: u64) -> Option<RequestTarget> {
        self.lock_swept(now)
            .get(request_id)
            .map(|request| request.target.clone())
    }

    /// Approves the request with `auth`, which may grant less than the app asked for, and
    /// `code`. A second approval replaces the code and the auth. The app then has the
    /// request's lifetime again to redeem it. The request's target stays as requested.
    /// `code` must be six digits ([`is_code`]): the empty code approves nothing.
    pub fn approve(
        &self,
        request_id: &str,
        auth: AuthInfo,
        code: Secret,
        now: u64,
    ) -> Result<(), String> {
        if !is_code(code.expose()) {
            return Err("An approval code has six digits".to_string());
        }
        let mut pending = self.lock_swept(now);
        let request = pending.get_mut(request_id).ok_or(
            "No pending request has this id; it may have expired. The app must request access \
             again",
        )?;
        request.auth = auth;
        request.code = Some(code);
        request.attempts = 0;
        request.expires_at = now + request_ttl(&request.target);
        Ok(())
    }

    /// Redeems the request when `code` is its code, in one step under the lock: `mint` gets
    /// the request's target and approved auth, and the request goes only when `mint`
    /// succeeds, so a code redeems once and a failed mint leaves it redeemable. A wrong code
    /// counts an attempt, and the [`MAX_CODE_ATTEMPTS`]th deletes the request. A code that is
    /// not six digits ([`is_code`]) is refused before the lookup and counts nothing.
    ///
    /// `mint` runs with the lock held, so it must not touch these requests.
    pub fn redeem<T>(
        &self,
        request_id: &str,
        code: &str,
        now: u64,
        mint: impl FnOnce(&AgentContext, &AuthInfo) -> Result<T, String>,
    ) -> Result<T, RedeemError> {
        // Only a six-digit code can redeem. Checked before any lookup or comparison, so the
        // empty code (or any other malformed one) never matches and touches no request.
        if !is_code(code) {
            return Err(RedeemError::NotPermitted);
        }
        let mut pending = self.lock();
        let result = match pending.get_mut(request_id) {
            None => Err(RedeemError::NotPermitted),
            Some(request) if request.expires_at <= now => {
                pending.remove(request_id);
                Err(RedeemError::Expired)
            }
            Some(request) => match &request.code {
                // Before the approval there is no code to guess, so nothing is counted.
                None => Err(RedeemError::NotPermitted),
                Some(expected) if expected.expose_for_compare(code) => match &request.target {
                    RequestTarget::Agent(target) => match mint(target, &request.auth) {
                        Ok(minted) => {
                            pending.remove(request_id);
                            Ok(minted)
                        }
                        Err(e) => Err(RedeemError::Mint(e)),
                    },
                    RequestTarget::NoAgent => Err(RedeemError::Mint(
                        "No agent on this executor has the name this request gave".to_string(),
                    )),
                },
                Some(_) => {
                    request.attempts += 1;
                    if request.attempts >= MAX_CODE_ATTEMPTS {
                        pending.remove(request_id);
                        Err(RedeemError::TooManyWrongCodes)
                    } else {
                        Err(RedeemError::WrongCode)
                    }
                }
            },
        };
        pending.retain(|_, request| request.expires_at > now);
        result
    }

    #[cfg(test)]
    pub fn contains(&self, request_id: &str) -> bool {
        self.lock().contains_key(request_id)
    }

    #[cfg(test)]
    pub fn has_code(&self, request_id: &str, code: &str) -> bool {
        self.lock().get(request_id).is_some_and(|request| {
            request
                .code
                .as_ref()
                .is_some_and(|c| c.expose_for_compare(code))
        })
    }

    /// Whether someone approved the request.
    #[cfg(test)]
    pub fn is_approved(&self, request_id: &str) -> bool {
        self.lock()
            .get(request_id)
            .is_some_and(|request| request.code.is_some())
    }

    /// The request's expiry (Unix seconds).
    #[cfg(test)]
    pub fn expires_at(&self, request_id: &str) -> Option<u64> {
        self.lock()
            .get(request_id)
            .map(|request| request.expires_at)
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.lock().len()
    }

    /// Sets the requests aside and fills the store with [`MAX_PENDING_REQUESTS`] approved
    /// requests for no agent, which nothing evicts, so the next request is refused. The
    /// requests set aside return when the guard drops. Requests other tests make meanwhile
    /// are refused too, so a test holds the guard only around its own call.
    #[cfg(test)]
    pub fn fill_for_test(&'static self) -> FullStoreGuard {
        let filled = (0..MAX_PENDING_REQUESTS)
            .map(|i| {
                let request = PendingRequest {
                    auth: AuthInfo::default(),
                    target: RequestTarget::NoAgent,
                    code: Some(Secret::new("123456")),
                    attempts: 0,
                    seq: NEXT_SEQ.fetch_add(1, Ordering::Relaxed),
                    expires_at: u64::MAX,
                };
                (format!("filled-{i}"), request)
            })
            .collect();
        let aside = std::mem::replace(&mut *self.lock(), filled);
        FullStoreGuard { store: self, aside }
    }
}

/// See [`PendingRequests::fill_for_test`].
#[cfg(test)]
pub struct FullStoreGuard {
    store: &'static PendingRequests,
    aside: HashMap<String, PendingRequest>,
}

#[cfg(test)]
impl Drop for FullStoreGuard {
    fn drop(&mut self) {
        *self.store.lock() = std::mem::take(&mut self.aside);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TTL: u64 = MAIN_AGENT_REQUEST_TTL_SECONDS;

    fn main() -> AgentContext {
        AgentContext::main_agent()
    }

    fn approved(pending: &PendingRequests, id: &str, now: u64) {
        pending
            .request(id.to_string(), AuthInfo::default(), main(), now)
            .unwrap();
        pending
            .approve(id, AuthInfo::default(), "123456".into(), now)
            .unwrap();
    }

    fn minted(_: &AgentContext, auth: &AuthInfo) -> Result<AuthInfo, String> {
        Ok(auth.clone())
    }

    fn redeem(
        pending: &PendingRequests,
        id: &str,
        code: &str,
        now: u64,
    ) -> Result<(), RedeemError> {
        pending.redeem(id, code, now, minted).map(|_| ())
    }

    #[test]
    fn the_right_code_redeems_the_request_once() {
        let pending = PendingRequests::default();
        approved(&pending, "a", 0);
        assert!(redeem(&pending, "a", "123456", 1).is_ok());
        assert_eq!(
            redeem(&pending, "a", "123456", 1).err(),
            Some(RedeemError::NotPermitted)
        );
    }

    #[test]
    fn redeeming_mints_on_the_requested_target_with_the_approved_auth() {
        let pending = PendingRequests::default();
        let alice = AgentContext::managed("did:key:z6MkAlice".into());
        let asked = AuthInfo {
            app_name: "asked".into(),
            ..AuthInfo::default()
        };
        pending
            .request("a".into(), asked, alice.clone(), 0)
            .unwrap();
        let granted = AuthInfo {
            app_name: "granted".into(),
            // An approval cannot move the request to another agent.
            user_email: Some("mallory@example.org".into()),
            ..AuthInfo::default()
        };
        pending.approve("a", granted, "123456".into(), 0).unwrap();
        let (target, auth) = pending
            .redeem("a", "123456", 1, |target, auth| {
                Ok((target.clone(), auth.clone()))
            })
            .unwrap();
        assert_eq!(target, alice);
        assert_eq!(auth.app_name, "granted");
    }

    // The old flow took the request out, then put it back when the token could not be issued:
    // two steps another redemption could slip between. Now the request goes only once the
    // grant exists.
    #[test]
    fn a_failed_mint_keeps_the_request_approved_and_redeemable() {
        let pending = PendingRequests::default();
        approved(&pending, "a", 0);
        let failed = pending.redeem("a", "123456", 1, |_, _| -> Result<(), String> {
            Err("Executor is locked".into())
        });
        assert_eq!(failed, Err(RedeemError::Mint("Executor is locked".into())));
        assert!(pending.has_code("a", "123456"));
        assert!(redeem(&pending, "a", "123456", 2).is_ok());
        assert!(!pending.contains("a"));
    }

    #[test]
    fn an_expired_request_is_refused_and_gone() {
        let pending = PendingRequests::default();
        approved(&pending, "a", 0);
        assert_eq!(
            redeem(&pending, "a", "123456", TTL).err(),
            Some(RedeemError::Expired)
        );
        assert!(!pending.contains("a"));
    }

    // D15: 10 minutes for the main agent, 24 hours for a managed agent (its user may answer
    // an email).
    #[test]
    fn a_request_for_a_managed_agent_waits_a_day_and_one_for_the_main_agent_ten_minutes() {
        let pending = PendingRequests::default();
        let alice = AgentContext::managed("did:key:z6MkAlice".into());
        pending
            .request("main".into(), AuthInfo::default(), main(), 0)
            .unwrap();
        pending
            .request("managed".into(), AuthInfo::default(), alice, 0)
            .unwrap();
        assert_eq!(pending.expires_at("main"), Some(10 * 60));
        assert_eq!(pending.expires_at("managed"), Some(24 * 60 * 60));

        // Past ten minutes the main agent's request is gone; the managed agent's waits.
        assert!(pending.target("main", 10 * 60).is_none());
        assert!(pending.target("managed", 10 * 60).is_some());
        pending
            .approve(
                "managed",
                AuthInfo::default(),
                "123456".into(),
                23 * 60 * 60,
            )
            .unwrap();
        assert_eq!(pending.expires_at("managed"), Some(47 * 60 * 60));
        assert!(redeem(&pending, "managed", "123456", 47 * 60 * 60 - 1).is_ok());
    }

    // An email no user has: the request waits as one for a managed agent would, so neither
    // its answer nor its lifetime says the email has no account, but its code mints nothing.
    #[test]
    fn a_request_for_no_agent_waits_like_a_managed_one_and_redeems_nothing() {
        let pending = PendingRequests::default();
        pending
            .request(
                "nobody".into(),
                AuthInfo::default(),
                RequestTarget::NoAgent,
                0,
            )
            .unwrap();
        assert_eq!(pending.expires_at("nobody"), Some(24 * 60 * 60));
        assert_eq!(pending.target("nobody", 0), Some(RequestTarget::NoAgent));

        pending
            .approve("nobody", AuthInfo::default(), "123456".into(), 0)
            .unwrap();
        let mut minted = false;
        let result = pending.redeem("nobody", "123456", 1, |_, _| {
            minted = true;
            Ok(())
        });
        assert!(matches!(result, Err(RedeemError::Mint(_))), "{result:?}");
        assert!(!minted, "nothing is minted for no agent");
    }

    #[test]
    fn the_approval_restarts_the_expiry() {
        let pending = PendingRequests::default();
        pending
            .request("a".into(), AuthInfo::default(), main(), 0)
            .unwrap();
        pending
            .approve("a", AuthInfo::default(), "123456".into(), TTL - 1)
            .unwrap();
        assert!(redeem(&pending, "a", "123456", 2 * TTL - 2).is_ok());
    }

    #[test]
    fn an_expired_request_cannot_be_approved() {
        let pending = PendingRequests::default();
        pending
            .request("a".into(), AuthInfo::default(), main(), 0)
            .unwrap();
        assert!(pending
            .approve("a", AuthInfo::default(), "123456".into(), TTL)
            .is_err());
        assert!(pending
            .approve("never-requested", AuthInfo::default(), "123456".into(), 0)
            .is_err());
    }

    #[test]
    fn every_access_sweeps_the_expired_requests() {
        let pending = PendingRequests::default();
        pending
            .request("old".into(), AuthInfo::default(), main(), 0)
            .unwrap();
        pending
            .request("new".into(), AuthInfo::default(), main(), 100)
            .unwrap();
        let _ = redeem(&pending, "unrelated", "000000", TTL + 50);
        assert!(!pending.contains("old"));
        assert!(pending.contains("new"));
        pending
            .request("newest".into(), AuthInfo::default(), main(), TTL + 100)
            .unwrap();
        assert!(!pending.contains("new"));
        assert_eq!(pending.len(), 1);
    }

    // A flood aimed at one agent evicts that agent's oldest unapproved requests, never an
    // approved one, and stays within the agent's share.
    #[test]
    fn a_flood_for_one_agent_evicts_its_oldest_unapproved_request() {
        let pending = PendingRequests::default();
        approved(&pending, "approved", 0);
        // All inside one expiry period, so the sweep removes none; r1 is the oldest.
        let max = MAX_PENDING_REQUESTS as u64;
        for i in 1..=max {
            pending
                .request(
                    format!("r{i}"),
                    AuthInfo::default(),
                    main(),
                    i * (TTL - 1) / max,
                )
                .unwrap();
        }
        assert_eq!(pending.len(), MAX_PENDING_REQUESTS_PER_TARGET);
        assert!(pending.contains("approved"));
        assert!(!pending.contains("r1"));
        let newest = MAX_PENDING_REQUESTS - MAX_PENDING_REQUESTS_PER_TARGET + 2;
        assert!(!pending.contains(&format!("r{}", newest - 1)));
        assert!(pending.contains(&format!("r{newest}")));
    }

    // Requests for emails without an agent are silent and live a day; a flood of them used to
    // evict the main agent's request (the nearest expiry), and the user's approval then found
    // nothing.
    #[test]
    fn a_silent_flood_keeps_a_waiting_request() {
        let pending = PendingRequests::default();
        let alice = AgentContext::managed("did:key:z6MkAlice".into());
        pending
            .request("legit".into(), AuthInfo::default(), main(), 0)
            .unwrap();
        pending
            .request("alice".into(), AuthInfo::default(), alice, 0)
            .unwrap();
        for i in 0..MAX_PENDING_REQUESTS {
            pending
                .request(
                    format!("f{i}"),
                    AuthInfo::default(),
                    RequestTarget::NoAgent,
                    1,
                )
                .unwrap();
        }
        assert!(
            pending.contains("legit"),
            "requests for no agent evicted a real one"
        );
        assert!(pending.contains("alice"));
        assert!(pending.contains(&format!("f{}", MAX_PENDING_REQUESTS - 1)));
    }

    // A full store of requests for 16 agents (64 each, the per-target share) does not refuse a
    // 17th agent: its request takes one from the agents that hold the most, so a flood aimed at
    // a few known agents cannot block every other user's approvals for a day.
    #[test]
    fn a_full_store_does_not_lock_out_an_agent_below_its_share() {
        let pending = PendingRequests::default();
        let agent = |a: usize| AgentContext::managed(format!("did:key:z6MkFlooded{a}"));
        for a in 0..MAX_PENDING_REQUESTS / MAX_PENDING_REQUESTS_PER_TARGET {
            for i in 0..MAX_PENDING_REQUESTS_PER_TARGET {
                pending
                    .request(format!("f{a}-{i}"), AuthInfo::default(), agent(a), 1)
                    .unwrap();
            }
        }
        assert_eq!(pending.len(), MAX_PENDING_REQUESTS);
        let bob = AgentContext::managed("did:key:z6MkBob".into());
        pending
            .request("bob".into(), AuthInfo::default(), bob.clone(), 2)
            .expect("an agent with no request waiting is not refused");
        assert!(pending.contains("bob"));
        assert!(
            !pending.contains("f0-0"),
            "the oldest of the fullest target goes"
        );
        assert_eq!(pending.len(), MAX_PENDING_REQUESTS);

        // The next agent takes from a target that now holds the most (f0 holds 63, the others
        // 64), never Bob's single request.
        let carol = AgentContext::managed("did:key:z6MkCarol".into());
        pending
            .request("carol".into(), AuthInfo::default(), carol, 2)
            .unwrap();
        assert!(pending.contains("bob"));
        assert!(pending.contains("f0-1"));
        assert!(!pending.contains("f1-0"));

        // A request for no agent still takes nothing from an agent.
        assert!(pending
            .request(
                "none".into(),
                AuthInfo::default(),
                RequestTarget::NoAgent,
                2
            )
            .is_ok());
        assert!(!pending.contains("none"));
        assert!(pending.contains("bob") && pending.contains("carol"));
        assert_eq!(pending.len(), MAX_PENDING_REQUESTS);
    }

    // With the store full of requests for agents, a request for an email without an account
    // gets the answer one for an email with an account gets, so the answer does not tell an
    // anonymous caller which emails have accounts (D16). Only a store where nothing may go
    // refuses both.
    #[test]
    fn a_full_store_answers_an_email_without_an_account_like_one_with() {
        let pending = PendingRequests::default();
        let agent = |a: usize| AgentContext::managed(format!("did:key:z6MkKnown{a}"));
        for a in 0..MAX_PENDING_REQUESTS / MAX_PENDING_REQUESTS_PER_TARGET {
            for i in 0..MAX_PENDING_REQUESTS_PER_TARGET {
                pending
                    .request(format!("k{a}-{i}"), AuthInfo::default(), agent(a), 1)
                    .unwrap();
            }
        }
        let with_account = AgentContext::managed("did:key:z6MkWithAccount".into());
        let answers = |pending: &PendingRequests| {
            (
                pending
                    .request(
                        "no-account".into(),
                        AuthInfo::default(),
                        RequestTarget::NoAgent,
                        2,
                    )
                    .is_ok(),
                pending
                    .request(
                        "account".into(),
                        AuthInfo::default(),
                        with_account.clone(),
                        2,
                    )
                    .is_ok(),
            )
        };
        assert_eq!(answers(&pending), (true, true));
        assert!(!pending.contains("no-account"));

        let approved_only = PendingRequests::default();
        for a in 0..MAX_PENDING_REQUESTS / MAX_PENDING_REQUESTS_PER_TARGET {
            for i in 0..MAX_PENDING_REQUESTS_PER_TARGET {
                let id = format!("k{a}-{i}");
                approved_only
                    .request(id.clone(), AuthInfo::default(), agent(a), 1)
                    .unwrap();
                approved_only
                    .approve(&id, AuthInfo::default(), "123456".into(), 1)
                    .unwrap();
            }
        }
        assert_eq!(answers(&approved_only), (false, false));
    }

    // When every target is full, a new request evicts one for no agent first, then the oldest
    // unapproved one for its own agent; never another agent's or an approved one.
    #[test]
    fn a_full_store_evicts_no_agent_then_the_same_agent_and_else_refuses() {
        let pending = PendingRequests::default();
        let agents = MAX_PENDING_REQUESTS / MAX_PENDING_REQUESTS_PER_TARGET;
        let agent = |a: usize| AgentContext::managed(format!("did:key:z6Mk{a}"));
        pending
            .request(
                "nobody".into(),
                AuthInfo::default(),
                RequestTarget::NoAgent,
                0,
            )
            .unwrap();
        for a in 0..agents {
            for i in 0..MAX_PENDING_REQUESTS_PER_TARGET {
                let _ = pending.request(format!("a{a}-{i}"), AuthInfo::default(), agent(a), 1);
            }
        }
        assert_eq!(pending.len(), MAX_PENDING_REQUESTS);
        // The last agent's last request evicted the one for no agent.
        assert!(!pending.contains("nobody"));
        assert!(pending.contains("a0-0"));

        pending
            .request("a0-new".into(), AuthInfo::default(), agent(0), 2)
            .unwrap();
        assert!(!pending.contains("a0-0"));
        assert!(pending.contains("a1-0"));

        // A new agent takes the oldest unapproved request of the target holding the most
        // (all hold 64; a0-1 is the oldest of them). No agent: nothing it may evict, so it is
        // answered as stored and dropped.
        let new_agent = AgentContext::managed("did:key:z6MkNew".into());
        pending
            .request("new".into(), AuthInfo::default(), new_agent, 2)
            .unwrap();
        assert!(!pending.contains("a0-1"));
        assert!(pending.contains("a1-0"));
        assert!(pending
            .request(
                "none".into(),
                AuthInfo::default(),
                RequestTarget::NoAgent,
                2
            )
            .is_ok());
        assert!(!pending.contains("none"));
        assert!(pending.contains("a1-0"));
        assert_eq!(pending.len(), MAX_PENDING_REQUESTS);

        // An approved request is never evicted.
        for i in 1..MAX_PENDING_REQUESTS_PER_TARGET {
            pending
                .approve(&format!("a1-{i}"), AuthInfo::default(), "123456".into(), 2)
                .unwrap();
        }
        pending
            .request("a1-new".into(), AuthInfo::default(), agent(1), 3)
            .unwrap();
        assert!(!pending.contains("a1-0"));
        pending
            .request("a1-newer".into(), AuthInfo::default(), agent(1), 3)
            .unwrap();
        assert!(!pending.contains("a1-new"));
        pending
            .approve("a1-newer", AuthInfo::default(), "123456".into(), 3)
            .unwrap();
        assert!(pending
            .request("a1-last".into(), AuthInfo::default(), agent(1), 3)
            .is_err());
        assert!(pending.is_approved("a1-newer"));
        assert!(
            (1..MAX_PENDING_REQUESTS_PER_TARGET).all(|i| pending.is_approved(&format!("a1-{i}")))
        );
    }

    #[test]
    fn five_wrong_codes_delete_the_request() {
        let pending = PendingRequests::default();
        approved(&pending, "a", 0);
        for _ in 0..MAX_CODE_ATTEMPTS - 1 {
            assert_eq!(
                redeem(&pending, "a", "000000", 0).err(),
                Some(RedeemError::WrongCode)
            );
        }
        assert_eq!(
            redeem(&pending, "a", "000000", 0).err(),
            Some(RedeemError::TooManyWrongCodes)
        );
        assert!(!pending.contains("a"));
        assert_eq!(
            redeem(&pending, "a", "123456", 0).err(),
            Some(RedeemError::NotPermitted)
        );
    }

    // Before the approval there is no code, so guesses then use up no attempts.
    #[test]
    fn codes_sent_before_the_approval_count_no_attempt() {
        let pending = PendingRequests::default();
        pending
            .request("a".into(), AuthInfo::default(), main(), 0)
            .unwrap();
        for _ in 0..MAX_CODE_ATTEMPTS {
            assert_eq!(
                redeem(&pending, "a", "000000", 0).err(),
                Some(RedeemError::NotPermitted)
            );
        }
        pending
            .approve("a", AuthInfo::default(), "123456".into(), 0)
            .unwrap();
        assert!(redeem(&pending, "a", "123456", 0).is_ok());
    }

    // No request is ever approved with an empty or malformed code, and no such code redeems
    // one: the empty code or any code that is not six digits is refused before the comparison
    // and uses up no attempt.
    #[test]
    fn only_a_six_digit_code_approves_or_redeems() {
        let pending = PendingRequests::default();
        pending
            .request("a".into(), AuthInfo::default(), main(), 0)
            .unwrap();
        let fullwidth = "\u{FF11}\u{FF12}\u{FF13}\u{FF14}\u{FF15}\u{FF16}";
        for code in ["", "12345", "1234567", "12345a", " 23456", fullwidth] {
            assert!(
                pending
                    .approve("a", AuthInfo::default(), Secret::new(code), 0)
                    .is_err(),
                "{code:?} approves nothing"
            );
        }
        assert!(!pending.is_approved("a"));
        assert_eq!(
            redeem(&pending, "a", "", 0).err(),
            Some(RedeemError::NotPermitted)
        );

        pending
            .approve("a", AuthInfo::default(), "123456".into(), 0)
            .unwrap();
        for _ in 0..2 * MAX_CODE_ATTEMPTS {
            for code in ["", "12345", "1234567", "12345a"] {
                assert_eq!(
                    redeem(&pending, "a", code, 0).err(),
                    Some(RedeemError::NotPermitted),
                    "{code:?}"
                );
            }
        }
        assert!(redeem(&pending, "a", "123456", 0).is_ok());
    }
}
