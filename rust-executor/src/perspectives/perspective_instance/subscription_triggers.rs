//! Which writes re-run a model subscription (#1237).

#[cfg(test)]
pub(super) mod reruns {
    //! Test hook: how many times each subscription was re-run.
    use std::collections::HashMap;
    use std::sync::{LazyLock, Mutex};

    static RERUNS: LazyLock<Mutex<HashMap<String, usize>>> = LazyLock::new(Default::default);

    pub(in crate::perspectives) fn note(subscription_id: &str) {
        *RERUNS
            .lock()
            .unwrap()
            .entry(subscription_id.to_string())
            .or_default() += 1;
    }

    pub(in crate::perspectives) fn count(subscription_id: &str) -> usize {
        RERUNS
            .lock()
            .unwrap()
            .get(subscription_id)
            .copied()
            .unwrap_or(0)
    }
}

#[cfg(test)]
mod tests;
