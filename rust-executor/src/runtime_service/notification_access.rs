//! Access rules for notifications.
//!
//! A notification posts trigger matches from the perspectives it lists to a
//! webhook URL. It belongs to the managed user in `user_email`, or to the main
//! agent when that is `None`.
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

use std::collections::HashMap;

use crate::agent::AgentService;
use crate::helpers::can_access_perspective_with_did;
use crate::types::{Notification, PerspectiveHandle};

/// True when the caller may update or delete `notification`: a managed user
/// only their own, an operator (`caller_email` is `None`) any.
pub(crate) fn caller_may_manage(
    notification: &Notification,
    caller_email: &Option<String>,
) -> bool {
    caller_email.is_none() || notification.user_email == *caller_email
}

/// The grant `stored` keeps when the caller in `caller_email` updates it.
pub(crate) fn granted_after_update(stored: &Notification, caller_email: &Option<String>) -> bool {
    stored.granted && stored.user_email.is_some() && stored.user_email == *caller_email
}

/// The notifications that fire for a change in `perspective`.
pub(crate) fn notifications_to_fire(
    notifications: Vec<Notification>,
    perspective: &PerspectiveHandle,
) -> Vec<Notification> {
    // One wallet lookup per user, not one per notification.
    let mut user_dids: HashMap<String, Option<String>> = HashMap::new();
    notifications
        .into_iter()
        .filter(|n| n.granted && n.perspective_ids.contains(&perspective.uuid))
        .filter(|n| match &n.user_email {
            None => can_access_perspective_with_did(&None, perspective),
            // A user without a key (deleted) reads nothing.
            Some(email) => user_dids
                .entry(email.clone())
                .or_insert_with(|| AgentService::get_user_did_by_email(email).ok())
                .as_ref()
                .is_some_and(|did| perspective.is_owned_by(did)),
        })
        .collect()
}
