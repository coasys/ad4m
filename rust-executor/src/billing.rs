use crate::db::Ad4mDb;
use crate::pubsub::mark_credits_dirty;

const DEFAULT_LINK_WRITE_RATE: f64 = 0.25; // credits per link write

/// Look up the link write rate from the host_rates DB table, falling back to the default.
pub fn get_link_write_rate() -> f64 {
    Ad4mDb::with_global_instance(|db| db.get_host_rate("link write"))
        .ok()
        .flatten()
        .unwrap_or(DEFAULT_LINK_WRITE_RATE)
}

/// Read-only credit check. Returns Ok(()) if the user can afford compute.
/// Used as a pre-check before link operations; the actual deduction happens
/// after the operation via bill_compute with the exact cost.
/// No-ops (allows) if free hosting is enabled or user has free access.
pub fn check_compute_credits(email: &str) -> Result<(), anyhow::Error> {
    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    if global_free {
        return Ok(());
    }
    let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))?;
    if free {
        return Ok(());
    }
    let credits = Ad4mDb::with_global_instance(|db| db.get_user_credits(email))?;
    if credits <= 0.0 {
        return Err(anyhow::anyhow!("Insufficient compute credits"));
    }
    Ok(())
}

/// Deduct credits and log a compute event for the given user email.
/// No-ops if free hosting is enabled globally or the user has free access.
///
/// This is the shared billing function callable from anywhere in the executor
/// (e.g. AI service async tasks, perspective instance methods) without needing
/// a GraphQL RequestContext or auth token.
pub fn bill_compute(
    email: &str,
    amount: f64,
    operation: &str,
    summary: Option<&str>,
) -> Result<(), anyhow::Error> {
    let global_free =
        Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled()).unwrap_or(true);
    if global_free {
        return Ok(());
    }
    let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))?;
    if free {
        return Ok(());
    }
    let (row_id, credits_after) = Ad4mDb::with_global_instance(|db| {
        db.deduct_credits_and_log(email, amount, operation, summary)
    })?;
    crate::pubsub::push_compute_log_entry(row_id, email, operation, summary, amount, credits_after);
    mark_credits_dirty(email);
    Ok(())
}
