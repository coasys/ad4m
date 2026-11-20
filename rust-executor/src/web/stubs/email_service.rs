//! `crate::email_service` on wasm32: no SMTP in the web executor. Sending fails with
//! [`NotAvailable`](crate::web::stubs::NotAvailable); test mode stays off.

use crate::config::SmtpConfig;
use anyhow::Error as AnyError;
use std::sync::{Arc, Mutex};

lazy_static! {
    /// Never set on wasm32: there is no `enable_test_mode`.
    pub static ref EMAIL_TEST_MODE: Arc<Mutex<bool>> = Arc::new(Mutex::new(false));
}

pub struct EmailService;

impl EmailService {
    pub fn new(_smtp_config: SmtpConfig) -> Self {
        EmailService
    }

    pub async fn send_verification_email(
        &self,
        _email: &str,
        _code: &str,
        _verification_type: &str,
        _app_name: Option<&str>,
        _app_icon: Option<&str>,
    ) -> Result<(), AnyError> {
        Err(crate::web::stubs::not_available("email"))
    }
}
