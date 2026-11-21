//! The host's settings (AR-05 4.3): who may join, the cap, the label, and the fields the
//! config file fixes. Kept as one JSON row `host_policy` of the `settings` table.

use serde::{Deserialize, Serialize};
use ts_rs::TS;

use crate::db_backend::db_backend;

use super::HostRefusal;

/// The `settings` row that holds the policy.
const POLICY_KEY: &str = "host_policy";

/// Who may join through `host.join`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "snake_case")]
#[ts(export)]
pub enum SignupPolicy {
    Anyone,
    ByInvite,
    /// A new executor's value, until its operator chooses.
    NoOne,
}

/// A field the config file fixes, so `host.setPolicy` refuses it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "snake_case")]
#[ts(export)]
pub enum PinnedField {
    Signup,
}

/// What the chrome shows of the host.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "snake_case")]
#[ts(export)]
pub struct HostDisplay {
    pub name: String,
    /// A label only: the hosting index decides which hosts are run by Coasys.
    pub run_by_coasys: bool,
}

impl Default for HostDisplay {
    fn default() -> Self {
        HostDisplay {
            name: "This executor".into(),
            run_by_coasys: false,
        }
    }
}

/// The host's settings. The config file's `host` section has this shape.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "snake_case", deny_unknown_fields)]
#[ts(export)]
pub struct HostPolicy {
    pub signup: SignupPolicy,
    /// The cap on `Active` plus `Pending` tenants; `None` for none.
    pub max_tenants: Option<u32>,
    pub display: HostDisplay,
    pub pinned: Vec<PinnedField>,
}

impl Default for HostPolicy {
    fn default() -> Self {
        HostPolicy {
            signup: SignupPolicy::NoOne,
            max_tenants: None,
            display: HostDisplay::default(),
            pinned: Vec::new(),
        }
    }
}

/// The `host` section of the config file: every field optional, as the file sets some.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct HostConfigSection {
    pub signup: Option<SignupPolicy>,
    pub max_tenants: Option<u32>,
    pub display: Option<HostDisplay>,
    pub pinned: Option<Vec<PinnedField>>,
    /// Withdrawn fields (no allowlist, no per-tenant limits): read so a file that carries
    /// them loads, and logged once at start.
    #[serde(default)]
    pub language_policy: Option<serde_json::Value>,
    #[serde(default)]
    pub default_limits: Option<serde_json::Value>,
}

/// `host.policy` and `host.setPolicy`.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostPolicyView {
    pub signup: SignupPolicy,
    pub max_tenants: Option<u32>,
    pub display: HostDisplay,
    pub pinned: Vec<PinnedField>,
    /// True while no policy was ever saved: the operator has not chosen (the notice
    /// "No one - choose who may join this device").
    pub choice_needed: bool,
}

/// What `host.setPolicy` changes. A field left out stays.
#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export)]
pub struct HostPolicyPatch {
    pub signup: Option<SignupPolicy>,
    /// The cap; 0 takes the cap away.
    pub max_tenants: Option<u32>,
    pub display_name: Option<String>,
}

/// The policy row, `None` before the operator or the config file chose.
fn stored() -> Result<Option<HostPolicy>, HostRefusal> {
    let Some(text) = db_backend()
        .get_setting(POLICY_KEY)
        .map_err(|e| HostRefusal::internal(e.to_string()))?
    else {
        return Ok(None);
    };
    serde_json::from_str(&text)
        .map(Some)
        .map_err(|e| HostRefusal::internal(format!("the stored host policy is unreadable: {e}")))
}

fn save(policy: &HostPolicy) -> Result<(), HostRefusal> {
    let text = serde_json::to_string(policy).map_err(|e| HostRefusal::internal(e.to_string()))?;
    db_backend()
        .set_setting(POLICY_KEY, &text)
        .map_err(|e| HostRefusal::internal(e.to_string()))
}

/// The policy in force: the stored one, else the start value (`NoOne`).
pub fn current() -> Result<HostPolicy, HostRefusal> {
    Ok(stored()?.unwrap_or_default())
}

/// [`current`] with whether the operator still has to choose.
pub fn view() -> Result<HostPolicyView, HostRefusal> {
    let choice_needed = stored()?.is_none();
    let policy = current()?;
    Ok(HostPolicyView {
        signup: policy.signup,
        max_tenants: policy.max_tenants,
        display: policy.display,
        pinned: policy.pinned,
        choice_needed,
    })
}

/// Applies `patch`; `pinned-by-config` for a pinned field, and nothing is written then.
pub fn set(patch: HostPolicyPatch) -> Result<HostPolicyView, HostRefusal> {
    let mut policy = current()?;
    if patch.signup.is_some() && policy.pinned.contains(&PinnedField::Signup) {
        return Err(HostRefusal::PinnedByConfig);
    }
    if let Some(signup) = patch.signup {
        policy.signup = signup;
    }
    if let Some(cap) = patch.max_tenants {
        policy.max_tenants = (cap > 0).then_some(cap);
    }
    if let Some(name) = patch.display_name {
        policy.display.name = name;
    }
    save(&policy)?;
    view()
}

/// Writes every field the config file's `host` section sets into the stored policy (at
/// start). A file that sets `signup` counts as the operator's choice.
pub fn seed_from_config(section: &HostConfigSection) -> Result<(), HostRefusal> {
    if section.language_policy.is_some() || section.default_limits.is_some() {
        log::info!("host config: language_policy and default_limits have no effect");
    }
    if section.signup.is_none()
        && section.max_tenants.is_none()
        && section.display.is_none()
        && section.pinned.is_none()
    {
        return Ok(());
    }
    let mut policy = current()?;
    if let Some(signup) = section.signup {
        policy.signup = signup;
    }
    if section.max_tenants.is_some() {
        policy.max_tenants = section.max_tenants;
    }
    if let Some(display) = &section.display {
        policy.display = display.clone();
    }
    if let Some(pinned) = &section.pinned {
        policy.pinned = pinned.clone();
    }
    save(&policy)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::perspectives::interpretation_test_support::ensure_db_init;

    fn reset() {
        ensure_db_init();
        crate::db::Ad4mDb::with_global_instance(|db| {
            db.clear_setting_for_test(POLICY_KEY).unwrap()
        });
    }

    #[test]
    fn no_row_is_no_one_with_a_choice_needed() {
        reset();
        let view = view().unwrap();
        assert_eq!(view.signup, SignupPolicy::NoOne);
        assert!(view.choice_needed);
    }

    #[test]
    fn saving_no_one_ends_the_notice() {
        reset();
        let view = set(HostPolicyPatch {
            signup: Some(SignupPolicy::NoOne),
            ..Default::default()
        })
        .unwrap();
        assert!(!view.choice_needed);
    }

    #[test]
    fn a_pinned_field_refuses_and_keeps_its_value() {
        reset();
        seed_from_config(&HostConfigSection {
            signup: Some(SignupPolicy::ByInvite),
            pinned: Some(vec![PinnedField::Signup]),
            ..Default::default()
        })
        .unwrap();
        let refused = set(HostPolicyPatch {
            signup: Some(SignupPolicy::Anyone),
            ..Default::default()
        });
        assert_eq!(refused.unwrap_err(), HostRefusal::PinnedByConfig);
        assert_eq!(current().unwrap().signup, SignupPolicy::ByInvite);
        // An unpinned field still changes.
        let view = set(HostPolicyPatch {
            max_tenants: Some(5),
            ..Default::default()
        })
        .unwrap();
        assert_eq!(view.max_tenants, Some(5));
        assert!(!view.choice_needed, "a file that sets signup is a choice");
    }

    #[test]
    fn a_zero_cap_takes_the_cap_away() {
        reset();
        set(HostPolicyPatch {
            max_tenants: Some(3),
            ..Default::default()
        })
        .unwrap();
        let view = set(HostPolicyPatch {
            max_tenants: Some(0),
            ..Default::default()
        })
        .unwrap();
        assert_eq!(view.max_tenants, None);
    }
}
