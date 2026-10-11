//! Open connections by token id, so a revocation closes them at once.
//!
//! Each socket that serves an app or a user session (the RPC socket, the events socket and
//! the realtime socket) registers while it is open. [`super::revoked::revoke`] ends every
//! connection of the revoked id: the connection's own close path then closes it with 1008,
//! as it does when a check fails. Without the registry an open socket closed only on its
//! next request or event.
//!
//! Ending a connection only cancels its token. The connection's task alone writes the close
//! frame, so a revoke from another task never races the socket's own writes, and a
//! connection that ends for two reasons at once closes once.

use super::session::{AuthError, Principal, Session, TokenId};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, MutexGuard};
use tokio_util::sync::CancellationToken;

lazy_static! {
    static ref SESSIONS: SessionRegistry = SessionRegistry::default();
}

/// One open connection.
struct Registration {
    id: u64,
    /// The email of the user the session names; `None` for an app.
    user: Option<String>,
    /// Ends the connection.
    end: CancellationToken,
}

/// The open connections of app and user sessions, by token id.
#[derive(Default)]
pub struct SessionRegistry {
    next_id: AtomicU64,
    connections: Mutex<HashMap<TokenId, Vec<Registration>>>,
}

/// Keeps a connection registered until it drops.
pub struct Registered {
    registry: &'static SessionRegistry,
    token_id: TokenId,
    id: u64,
}

impl Drop for Registered {
    fn drop(&mut self) {
        let mut connections = self.registry.lock();
        if let Some(registrations) = connections.get_mut(&self.token_id) {
            registrations.retain(|r| r.id != self.id);
            if registrations.is_empty() {
                connections.remove(&self.token_id);
            }
        }
    }
}

impl SessionRegistry {
    /// The executor's registry: every revocation ends the connections in it.
    pub fn global() -> &'static SessionRegistry {
        &SESSIONS
    }

    fn lock(&self) -> MutexGuard<'_, HashMap<TokenId, Vec<Registration>>> {
        self.connections
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }

    /// Registers a connection of `session` until the returned guard drops. `end` fires when
    /// the session's token is revoked. The operator and anonymous callers have no token to
    /// revoke: they get `None`.
    ///
    /// Checks the session after it registers: a revoke that ran between the connection's
    /// own check and the registration found nothing to end, so this check ends it. An
    /// expired token is left to the connection's next check, which answers 401.
    pub fn register(
        &'static self,
        session: &Session,
        end: CancellationToken,
    ) -> Option<Registered> {
        let (token_id, user) = match &session.principal {
            Principal::App { token_id, .. } => (token_id.clone(), None),
            Principal::User {
                token_id, email, ..
            } => (token_id.clone(), Some(email.clone())),
            Principal::Operator | Principal::Anonymous { .. } => return None,
        };
        let id = self.next_id.fetch_add(1, Ordering::Relaxed);
        self.lock()
            .entry(token_id.clone())
            .or_default()
            .push(Registration {
                id,
                user,
                end: end.clone(),
            });
        if session.check() == Err(AuthError::Revoked) {
            end.cancel();
        }
        Some(Registered {
            registry: self,
            token_id,
            id,
        })
    }

    /// Ends every open connection of `token_id`. Nothing happens when it has none.
    pub fn end(&self, token_id: &TokenId) {
        let ends: Vec<CancellationToken> = self
            .lock()
            .get(token_id)
            .map(|registrations| registrations.iter().map(|r| r.end.clone()).collect())
            .unwrap_or_default();
        ends.iter().for_each(CancellationToken::cancel);
    }

    /// Ends every open connection of the user `email`, whatever its token.
    pub fn end_user(&self, email: &str) {
        let ends: Vec<CancellationToken> = self
            .lock()
            .values()
            .flatten()
            .filter(|r| r.user.as_deref() == Some(email))
            .map(|r| r.end.clone())
            .collect();
        ends.iter().for_each(CancellationToken::cancel);
    }

    /// How many connections of `token_id` are open.
    #[cfg(test)]
    pub(crate) fn connections_of(&self, token_id: &TokenId) -> usize {
        self.lock().get(token_id).map_or(0, Vec::len)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::capabilities::ListenerReach;

    fn registry() -> &'static SessionRegistry {
        Box::leak(Box::default())
    }

    fn id(s: &str) -> TokenId {
        TokenId(s.to_string())
    }

    fn app(token_id: &str) -> Session {
        Session::for_test(
            Principal::App {
                token_id: id(token_id),
                caps: vec![],
            },
            ListenerReach::Network,
        )
    }

    fn user(token_id: &str, email: &str) -> Session {
        Session::for_test(
            Principal::User {
                token_id: id(token_id),
                email: email.to_string(),
                did: format!("did:key:{email}"),
                caps: vec![],
            },
            ListenerReach::Network,
        )
    }

    #[test]
    fn ending_a_token_ends_each_of_its_connections_and_no_other() {
        let registry = registry();
        let (first, second, other) = (
            CancellationToken::new(),
            CancellationToken::new(),
            CancellationToken::new(),
        );
        let _a = registry.register(&app("a"), first.clone()).unwrap();
        let _b = registry.register(&app("a"), second.clone()).unwrap();
        let _c = registry.register(&app("c"), other.clone()).unwrap();
        assert_eq!(registry.connections_of(&id("a")), 2);

        registry.end(&id("a"));
        assert!(first.is_cancelled() && second.is_cancelled());
        assert!(!other.is_cancelled());
    }

    #[test]
    fn a_connection_leaves_the_registry_when_its_guard_drops() {
        let registry = registry();
        let first = registry.register(&app("a"), CancellationToken::new());
        let second = registry.register(&app("a"), CancellationToken::new());
        drop(first);
        assert_eq!(registry.connections_of(&id("a")), 1);
        drop(second);
        assert_eq!(registry.connections_of(&id("a")), 0);
        assert!(registry.lock().is_empty(), "no empty entry stays behind");
    }

    #[test]
    fn ending_a_token_without_connections_does_nothing() {
        let registry = registry();
        registry.end(&id("never-connected"));
        assert!(registry.lock().is_empty());
    }

    #[test]
    fn the_operator_and_anonymous_callers_do_not_register() {
        let registry = registry();
        for principal in [Principal::Operator, Principal::Anonymous { caps: vec![] }] {
            let session = Session::for_test(principal, ListenerReach::Network);
            assert!(registry
                .register(&session, CancellationToken::new())
                .is_none());
        }
        assert!(registry.lock().is_empty());
    }

    #[test]
    fn ending_a_user_ends_every_token_of_that_user_only() {
        let registry = registry();
        let (laptop, phone, other) = (
            CancellationToken::new(),
            CancellationToken::new(),
            CancellationToken::new(),
        );
        let _l = registry.register(&user("l", "a@example.org"), laptop.clone());
        let _p = registry.register(&user("p", "a@example.org"), phone.clone());
        let _o = registry.register(&user("o", "b@example.org"), other.clone());

        registry.end_user("a@example.org");
        assert!(laptop.is_cancelled() && phone.is_cancelled());
        assert!(!other.is_cancelled());
    }

    // The revoke ran before the connection registered: the registry's check ends it.
    #[test]
    fn a_connection_that_registers_after_its_revocation_ends_at_once() {
        let registry = registry();
        let revoked = format!("revoked-{}", uuid::Uuid::new_v4());
        crate::agent::revoked::RevokedSet::global().revoke(id(&revoked), u64::MAX);
        let end = CancellationToken::new();
        let _registered = registry.register(&app(&revoked), end.clone());
        assert!(end.is_cancelled());
    }
}
