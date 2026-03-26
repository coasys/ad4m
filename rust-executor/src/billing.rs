use crate::db::Ad4mDb;
use crate::pubsub::mark_credits_dirty;

#[derive(Debug, thiserror::Error)]
pub enum BillingError {
    #[error("Insufficient compute credits")]
    InsufficientCredits,
    #[error("User not found: {0}")]
    UserNotFound(String),
    #[error("{0}")]
    Other(#[from] anyhow::Error),
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
) -> Result<(), BillingError> {
    let global_free = Ad4mDb::with_global_instance(|db| db.get_free_hosting_enabled())?;
    if global_free {
        return Ok(());
    }
    let free = Ad4mDb::with_global_instance(|db| db.get_user_free_access(email))?;
    if free {
        return Ok(());
    }
    let result = Ad4mDb::with_global_instance(|db| {
        db.deduct_credits_and_log(email, amount, operation, summary)
    });

    match result {
        Ok((row_id, credits_after)) => {
            crate::pubsub::push_compute_log_entry(
                row_id,
                email,
                operation,
                summary,
                amount,
                credits_after,
            );
            mark_credits_dirty(email);
            Ok(())
        }
        Err(e) => {
            let msg = e.to_string();
            if msg.contains("Insufficient compute credits") {
                Err(BillingError::InsufficientCredits)
            } else if msg.starts_with("User not found") {
                Err(BillingError::UserNotFound(email.to_string()))
            } else {
                Err(BillingError::Other(e))
            }
        }
    }
}
