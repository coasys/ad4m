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
use std::sync::Mutex;

/// Wrong codes a request may receive before it is deleted. The code has six digits, so
/// without a limit anyone who learned a request id could try every code.
pub const MAX_CODE_ATTEMPTS: u8 = 5;

/// Seconds a request for the main agent waits: from the request to the approval, and again
/// from the approval to the redemption. Its operator is usually at the executor's launcher.
pub const MAIN_AGENT_REQUEST_TTL_SECONDS: u64 = 10 * 60;

/// Seconds a request for a managed agent waits (D15): its user may approve from an email.
pub const MANAGED_AGENT_REQUEST_TTL_SECONDS: u64 = 24 * 60 * 60;

/// Requests held at most. Anyone may request a capability, so past this a new request evicts
/// the unapproved one nearest its expiry instead of growing memory. Only an agent's owner
/// approves, so a flood of requests evicts no approved one while unapproved ones remain.
pub const MAX_PENDING_REQUESTS: usize = 1024;

/// The agent a capability request is for (D16: an agent is its DID).
#[derive(Clone, Debug, PartialEq, Eq)]
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

    /// Records a new request of the app `auth` for the agent `target`.
    pub fn request(
        &self,
        request_id: String,
        auth: AuthInfo,
        target: impl Into<RequestTarget>,
        now: u64,
    ) {
        let target = target.into();
        let mut pending = self.lock_swept(now);
        if pending.len() >= MAX_PENDING_REQUESTS {
            let nearest = pending
                .iter()
                .min_by_key(|(_, request)| (request.code.is_some(), request.expires_at))
                .map(|(id, _)| id.clone());
            if let Some(id) = nearest {
                pending.remove(&id);
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
                expires_at,
            },
        );
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
}

#[cfg(test)]
mod tests {
    use super::*;

    const TTL: u64 = MAIN_AGENT_REQUEST_TTL_SECONDS;

    fn main() -> AgentContext {
        AgentContext::main_agent()
    }

    fn approved(pending: &PendingRequests, id: &str, now: u64) {
        pending.request(id.to_string(), AuthInfo::default(), main(), now);
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
        pending.request("a".into(), asked, alice.clone(), 0);
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
        pending.request("main".into(), AuthInfo::default(), main(), 0);
        pending.request("managed".into(), AuthInfo::default(), alice, 0);
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
        pending.request(
            "nobody".into(),
            AuthInfo::default(),
            RequestTarget::NoAgent,
            0,
        );
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
        pending.request("a".into(), AuthInfo::default(), main(), 0);
        pending
            .approve("a", AuthInfo::default(), "123456".into(), TTL - 1)
            .unwrap();
        assert!(redeem(&pending, "a", "123456", 2 * TTL - 2).is_ok());
    }

    #[test]
    fn an_expired_request_cannot_be_approved() {
        let pending = PendingRequests::default();
        pending.request("a".into(), AuthInfo::default(), main(), 0);
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
        pending.request("old".into(), AuthInfo::default(), main(), 0);
        pending.request("new".into(), AuthInfo::default(), main(), 100);
        let _ = redeem(&pending, "unrelated", "000000", TTL + 50);
        assert!(!pending.contains("old"));
        assert!(pending.contains("new"));
        pending.request("newest".into(), AuthInfo::default(), main(), TTL + 100);
        assert!(!pending.contains("new"));
        assert_eq!(pending.len(), 1);
    }

    #[test]
    fn a_flood_of_requests_evicts_the_unapproved_one_nearest_its_expiry() {
        let pending = PendingRequests::default();
        approved(&pending, "approved", 0);
        // All inside one expiry period, so the sweep removes none; r1 is the oldest.
        let max = MAX_PENDING_REQUESTS as u64;
        for i in 1..=max {
            pending.request(
                format!("r{i}"),
                AuthInfo::default(),
                main(),
                i * (TTL - 1) / max,
            );
        }
        assert_eq!(pending.len(), MAX_PENDING_REQUESTS);
        assert!(pending.contains("approved"));
        assert!(!pending.contains("r1"));
        assert!(pending.contains("r2"));
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
        pending.request("a".into(), AuthInfo::default(), main(), 0);
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
        pending.request("a".into(), AuthInfo::default(), main(), 0);
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
