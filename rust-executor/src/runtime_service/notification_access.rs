//! Access rules for notifications.
//!
//! A notification posts trigger matches from the perspectives it lists to a
//! webhook URL. It belongs to the DID in `owner_did`: a managed user's, or the
//! main agent's for the operator's notifications. Every rule compares DIDs;
//! the operator's sessions act as the main agent.
//!
//! - Grant: a main-agent notification fires only after the operator approves
//!   it with `runtime.grantNotification`, which only the admin credential may
//!   call (the launcher shows the request). An app cannot approve its own.
//!   Managed users cannot call that RPC, so their notifications are granted
//!   when created. That is safe because creation refuses every perspective the
//!   user does not own: the grant covers only the user's own data.
//! - Update: an update never raises the grant. The operator approved the old
//!   trigger, perspectives and webhook, so an update drops the grant of a
//!   main-agent notification. A user's update of their own notification passes
//!   the same perspective check as creation and keeps the stored grant. An
//!   operator's update of a user's notification drops it.
//! - Delivery: a notification fires for a perspective only when it is granted
//!   and its owner may read that perspective at that moment. This also covers
//!   rows that never passed the checks above (older executors, DB import) and
//!   ownership that changed after the grant.

use crate::types::{Notification, PerspectiveHandle};

/// True when `owner_did` may read `perspective`. The main agent also reads
/// unowned perspectives.
pub(crate) fn owner_may_read(
    owner_did: &str,
    main_agent_did: &str,
    perspective: &PerspectiveHandle,
) -> bool {
    perspective.is_owned_by(owner_did) || (owner_did == main_agent_did && perspective.is_unowned())
}

/// True when `caller_did` may update or delete `notification`: a managed user
/// only their own, the operator (the main agent) any.
pub(crate) fn caller_may_manage(
    notification: &Notification,
    caller_did: &str,
    main_agent_did: &str,
) -> bool {
    caller_did == main_agent_did || notification.owner_did == caller_did
}

/// The grant `stored` keeps when `caller_did` updates it.
pub(crate) fn granted_after_update(
    stored: &Notification,
    caller_did: &str,
    main_agent_did: &str,
) -> bool {
    stored.granted && stored.owner_did != main_agent_did && stored.owner_did == caller_did
}

/// The notifications that fire for a change in `perspective`.
pub(crate) fn notifications_to_fire(
    notifications: Vec<Notification>,
    perspective: &PerspectiveHandle,
    main_agent_did: &str,
) -> Vec<Notification> {
    notifications
        .into_iter()
        .filter(|n| n.granted && n.perspective_ids.contains(&perspective.uuid))
        .filter(|n| owner_may_read(&n.owner_did, main_agent_did, perspective))
        .collect()
}
