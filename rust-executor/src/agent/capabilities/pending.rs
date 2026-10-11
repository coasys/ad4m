//! [`PendingApproval`]: an app's capability request, from `agent.requestCapability` until the
//! app redeems its code (`agent.generateJwt`), runs out of code attempts, or expires.

use super::types::AuthInfo;
use crate::secret::Secret;
use std::collections::HashMap;
use std::sync::Mutex;

/// Wrong codes a request may receive before it is deleted. The code has six digits, so
/// without a limit anyone who learned a request id could try every code.
pub const MAX_CODE_ATTEMPTS: u8 = 5;

/// Seconds a request waits: from the request to the approval, and again from the approval
/// to the redemption.
pub const PENDING_APPROVAL_TTL_SECONDS: u64 = 10 * 60;

/// Requests held at most. Anyone may request a capability, so past this a new request evicts
/// the unapproved one nearest its expiry instead of growing memory. Only the operator
/// approves, so a flood of requests evicts no approved one while unapproved ones remain.
pub const MAX_PENDING_APPROVALS: usize = 1024;

/// One capability request.
pub struct PendingApproval {
    auth: AuthInfo,
    /// Set when the operator approves; goes to the operator only.
    code: Option<Secret>,
    attempts: u8,
    /// Unix seconds.
    expires_at: u64,
}

impl PendingApproval {
    pub fn auth(&self) -> &AuthInfo {
        &self.auth
    }
}

/// Why a code was not redeemed.
#[derive(Debug, PartialEq, Eq)]
pub enum RedeemError {
    /// No request has this id, or the operator has not approved it yet. Records nothing.
    NotPermitted,
    /// The request expired; it is gone.
    Expired,
    /// The code is wrong; the request counts the attempt.
    WrongCode,
    /// The last allowed wrong code; the request is gone.
    TooManyWrongCodes,
}

impl std::fmt::Display for RedeemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            RedeemError::NotPermitted | RedeemError::WrongCode => "Can't find permitted request",
            RedeemError::Expired => "This request expired; request access again",
            RedeemError::TooManyWrongCodes => {
                "Too many wrong codes for this request; request access again"
            }
        })
    }
}

/// Pending requests by request id. Every request, approval and redemption also deletes the
/// expired ones, so no separate sweeper runs.
#[derive(Default)]
pub struct PendingApprovals(Mutex<HashMap<String, PendingApproval>>);

lazy_static! {
    static ref PENDING: PendingApprovals = PendingApprovals::default();
}

impl PendingApprovals {
    /// The executor's requests.
    pub fn global() -> &'static PendingApprovals {
        &PENDING
    }

    fn lock_swept(&self, now: u64) -> std::sync::MutexGuard<'_, HashMap<String, PendingApproval>> {
        let mut pending = self
            .0
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        pending.retain(|_, approval| approval.expires_at > now);
        pending
    }

    /// Records a new request.
    pub fn request(&self, request_id: String, auth: AuthInfo, now: u64) {
        let mut pending = self.lock_swept(now);
        if pending.len() >= MAX_PENDING_APPROVALS {
            let nearest = pending
                .iter()
                .min_by_key(|(_, approval)| (approval.code.is_some(), approval.expires_at))
                .map(|(id, _)| id.clone());
            if let Some(id) = nearest {
                pending.remove(&id);
            }
        }
        let approval = PendingApproval {
            auth,
            code: None,
            attempts: 0,
            expires_at: now + PENDING_APPROVAL_TTL_SECONDS,
        };
        pending.insert(request_id, approval);
    }

    /// The operator approves the request with `auth`, which may differ from what the app
    /// asked for, and `code`. A second approval replaces the code. The app then has
    /// [`PENDING_APPROVAL_TTL_SECONDS`] to redeem it.
    pub fn approve(
        &self,
        request_id: &str,
        auth: AuthInfo,
        code: String,
        now: u64,
    ) -> Result<(), String> {
        let mut pending = self.lock_swept(now);
        let approval = pending.get_mut(request_id).ok_or(
            "No pending request has this id; it may have expired. The app must request access \
             again",
        )?;
        approval.auth = auth;
        approval.code = Some(Secret::new(code));
        approval.expires_at = now + PENDING_APPROVAL_TTL_SECONDS;
        Ok(())
    }

    /// Takes the request out when `code` is its code. A wrong code counts an attempt, and
    /// the [`MAX_CODE_ATTEMPTS`]th deletes the request.
    pub fn redeem(
        &self,
        request_id: &str,
        code: &str,
        now: u64,
    ) -> Result<PendingApproval, RedeemError> {
        let mut pending = self
            .0
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let result = match pending.get_mut(request_id) {
            None => Err(RedeemError::NotPermitted),
            Some(approval) if approval.expires_at <= now => {
                pending.remove(request_id);
                Err(RedeemError::Expired)
            }
            Some(approval) => match &approval.code {
                // Before the approval there is no code to guess, so nothing is counted.
                None => Err(RedeemError::NotPermitted),
                Some(expected) if expected.expose_for_compare(code) => {
                    Ok(pending.remove(request_id).expect("the request is present"))
                }
                Some(_) => {
                    approval.attempts += 1;
                    if approval.attempts >= MAX_CODE_ATTEMPTS {
                        pending.remove(request_id);
                        Err(RedeemError::TooManyWrongCodes)
                    } else {
                        Err(RedeemError::WrongCode)
                    }
                }
            },
        };
        pending.retain(|_, approval| approval.expires_at > now);
        result
    }

    /// Puts back a redeemed request whose token could not be issued (a locked wallet) or
    /// registered (the registry file did not take it), so the app can retry with the same
    /// code.
    pub fn restore(&self, request_id: String, approval: PendingApproval) {
        let mut pending = self
            .0
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        pending.insert(request_id, approval);
    }

    #[cfg(test)]
    pub fn contains(&self, request_id: &str) -> bool {
        self.0.lock().unwrap().contains_key(request_id)
    }

    #[cfg(test)]
    pub fn has_code(&self, request_id: &str, code: &str) -> bool {
        self.0
            .lock()
            .unwrap()
            .get(request_id)
            .is_some_and(|approval| {
                approval
                    .code
                    .as_ref()
                    .is_some_and(|c| c.expose_for_compare(code))
            })
    }

    #[cfg(test)]
    fn len(&self) -> usize {
        self.0.lock().unwrap().len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const TTL: u64 = PENDING_APPROVAL_TTL_SECONDS;

    fn approved(pending: &PendingApprovals, id: &str, now: u64) {
        pending.request(id.to_string(), AuthInfo::default(), now);
        pending
            .approve(id, AuthInfo::default(), "123456".into(), now)
            .unwrap();
    }

    #[test]
    fn the_right_code_redeems_the_request_once() {
        let pending = PendingApprovals::default();
        approved(&pending, "a", 0);
        assert!(pending.redeem("a", "123456", 1).is_ok());
        assert_eq!(
            pending.redeem("a", "123456", 1).err(),
            Some(RedeemError::NotPermitted)
        );
    }

    #[test]
    fn an_expired_request_is_refused_and_gone() {
        let pending = PendingApprovals::default();
        approved(&pending, "a", 0);
        assert_eq!(
            pending.redeem("a", "123456", TTL).err(),
            Some(RedeemError::Expired)
        );
        assert!(!pending.contains("a"));
    }

    #[test]
    fn the_approval_restarts_the_expiry() {
        let pending = PendingApprovals::default();
        pending.request("a".into(), AuthInfo::default(), 0);
        pending
            .approve("a", AuthInfo::default(), "123456".into(), TTL - 1)
            .unwrap();
        assert!(pending.redeem("a", "123456", 2 * TTL - 2).is_ok());
    }

    #[test]
    fn an_expired_request_cannot_be_approved() {
        let pending = PendingApprovals::default();
        pending.request("a".into(), AuthInfo::default(), 0);
        assert!(pending
            .approve("a", AuthInfo::default(), "123456".into(), TTL)
            .is_err());
        assert!(pending
            .approve("never-requested", AuthInfo::default(), "123456".into(), 0)
            .is_err());
    }

    #[test]
    fn every_access_sweeps_the_expired_requests() {
        let pending = PendingApprovals::default();
        pending.request("old".into(), AuthInfo::default(), 0);
        pending.request("new".into(), AuthInfo::default(), 100);
        let _ = pending.redeem("unrelated", "000000", TTL + 50);
        assert!(!pending.contains("old"));
        assert!(pending.contains("new"));
        pending.request("newest".into(), AuthInfo::default(), TTL + 100);
        assert!(!pending.contains("new"));
        assert_eq!(pending.len(), 1);
    }

    #[test]
    fn a_flood_of_requests_evicts_the_unapproved_one_nearest_its_expiry() {
        let pending = PendingApprovals::default();
        approved(&pending, "approved", 0);
        // All inside one expiry period, so the sweep removes none; r1 is the oldest.
        let max = MAX_PENDING_APPROVALS as u64;
        for i in 1..=max {
            pending.request(format!("r{i}"), AuthInfo::default(), i * (TTL - 1) / max);
        }
        assert_eq!(pending.len(), MAX_PENDING_APPROVALS);
        assert!(pending.contains("approved"));
        assert!(!pending.contains("r1"));
        assert!(pending.contains("r2"));
    }

    // Before the approval there is no code, so guesses then use up no attempts.
    #[test]
    fn codes_sent_before_the_approval_count_no_attempt() {
        let pending = PendingApprovals::default();
        pending.request("a".into(), AuthInfo::default(), 0);
        for _ in 0..MAX_CODE_ATTEMPTS {
            assert_eq!(
                pending.redeem("a", "000000", 0).err(),
                Some(RedeemError::NotPermitted)
            );
        }
        pending
            .approve("a", AuthInfo::default(), "123456".into(), 0)
            .unwrap();
        assert!(pending.redeem("a", "123456", 0).is_ok());
    }

    #[test]
    fn a_restored_request_redeems_again() {
        let pending = PendingApprovals::default();
        approved(&pending, "a", 0);
        let approval = pending.redeem("a", "123456", 0).ok().unwrap();
        pending.restore("a".into(), approval);
        assert!(pending.redeem("a", "123456", 0).is_ok());
    }
}
