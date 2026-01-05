use crate::config::SmtpConfig;
use deno_core::error::AnyError;
use lettre::transport::smtp::authentication::Credentials;
use lettre::{Message, SmtpTransport, Transport};
use std::sync::{Arc, Mutex};
use lazy_static::lazy_static;

lazy_static! {
    /// Test mode: captures sent emails instead of actually sending them
    /// Map of email address -> verification code
    pub static ref TEST_MODE_EMAILS: Arc<Mutex<std::collections::HashMap<String, String>>> =
        Arc::new(Mutex::new(std::collections::HashMap::new()));

    /// Flag to enable test mode (set via environment variable or config)
    pub static ref EMAIL_TEST_MODE: Arc<Mutex<bool>> = Arc::new(Mutex::new(false));
}

/// Enable test mode for email service (captures codes instead of sending)
pub fn enable_test_mode() {
    if let Ok(mut test_mode) = EMAIL_TEST_MODE.lock() {
        *test_mode = true;
        log::info!("Email service test mode ENABLED - emails will be captured, not sent");
    }
}

/// Disable test mode
pub fn disable_test_mode() {
    if let Ok(mut test_mode) = EMAIL_TEST_MODE.lock() {
        *test_mode = false;
        log::info!("Email service test mode DISABLED - emails will be sent normally");
    }
}

/// Get captured email code for testing
pub fn get_test_code(email: &str) -> Option<String> {
    TEST_MODE_EMAILS.lock().ok()
        .and_then(|codes| codes.get(email).cloned())
}

/// Clear all captured test codes
pub fn clear_test_codes() {
    if let Ok(mut codes) = TEST_MODE_EMAILS.lock() {
        codes.clear();
    }
}

pub struct EmailService {
    smtp_config: SmtpConfig,
}

impl EmailService {
    pub fn new(smtp_config: SmtpConfig) -> Self {
        Self { smtp_config }
    }

    /// Sends a verification email with a 6-digit code
    pub async fn send_verification_email(
        &self,
        email: &str,
        code: &str,
        verification_type: &str,
    ) -> Result<(), AnyError> {
        // Check if we're in test mode
        let test_mode = EMAIL_TEST_MODE.lock().ok()
            .map(|mode| *mode)
            .unwrap_or(false);

        if test_mode {
            // In test mode: capture the code instead of sending
            if let Ok(mut codes) = TEST_MODE_EMAILS.lock() {
                codes.insert(email.to_string(), code.to_string());
                log::info!("📧 TEST MODE: Captured verification code for {}: {}", email, code);
            }
            return Ok(());
        }

        // Normal mode: actually send the email
        let (html_body, text_body) = self.render_verification_email(code, verification_type);

        let subject = match verification_type {
            "signup" => "Complete Your AD4M Account Registration",
            "login" => "Your AD4M Login Verification Code",
            _ => "Your AD4M Verification Code",
        };

        self.send_email(email, subject, &html_body, &text_body).await
    }

    /// Sends a test email to verify SMTP configuration
    pub async fn send_test_email(&self, to: &str) -> Result<(), AnyError> {
        let html_body = r#"
            <!DOCTYPE html>
            <html>
            <head>
                <meta charset="utf-8">
                <style>
                    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; line-height: 1.6; color: #333; }
                    .container { max-width: 600px; margin: 0 auto; padding: 20px; }
                    .header { background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; padding: 30px; text-align: center; border-radius: 8px 8px 0 0; }
                    .content { background: #f9f9f9; padding: 30px; border-radius: 0 0 8px 8px; }
                    h1 { margin: 0; font-size: 24px; }
                    .success { background: #10b981; color: white; padding: 15px; border-radius: 6px; text-align: center; font-weight: bold; }
                </style>
            </head>
            <body>
                <div class="container">
                    <div class="header">
                        <h1>AD4M SMTP Test</h1>
                    </div>
                    <div class="content">
                        <div class="success">✓ SMTP Configuration Successful!</div>
                        <p>Your SMTP settings are working correctly. You can now send verification emails to users.</p>
                        <p style="color: #666; font-size: 14px; margin-top: 30px;">
                            This is a test email from your AD4M executor instance.
                        </p>
                    </div>
                </div>
            </body>
            </html>
        "#;

        let text_body = "AD4M SMTP Test\n\n✓ SMTP Configuration Successful!\n\nYour SMTP settings are working correctly. You can now send verification emails to users.\n\nThis is a test email from your AD4M executor instance.";

        self.send_email(to, "AD4M SMTP Configuration Test", html_body, text_body)
            .await
    }

    /// Renders the HTML and plain text versions of a verification email
    fn render_verification_email(&self, code: &str, verification_type: &str) -> (String, String) {
        let action = match verification_type {
            "signup" => "complete your account registration",
            "login" => "log in to your account",
            _ => "verify your email",
        };

        let html_body = format!(
            r#"
<!DOCTYPE html>
<html>
<head>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <style>
        body {{
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif;
            line-height: 1.6;
            color: #333;
            margin: 0;
            padding: 0;
            background-color: #f5f5f5;
        }}
        .container {{
            max-width: 600px;
            margin: 40px auto;
            background-color: white;
            border-radius: 12px;
            overflow: hidden;
            box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }}
        .header {{
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 40px 30px;
            text-align: center;
        }}
        .header h1 {{
            margin: 0;
            font-size: 28px;
            font-weight: 600;
        }}
        .content {{
            padding: 40px 30px;
        }}
        .code-container {{
            background: #f9fafb;
            border: 2px solid #e5e7eb;
            border-radius: 8px;
            padding: 30px;
            margin: 30px 0;
            text-align: center;
        }}
        .code {{
            font-size: 48px;
            font-weight: bold;
            letter-spacing: 8px;
            color: #667eea;
            font-family: 'Courier New', monospace;
            margin: 10px 0;
        }}
        .code-label {{
            font-size: 14px;
            color: #6b7280;
            text-transform: uppercase;
            letter-spacing: 1px;
            margin-bottom: 10px;
        }}
        .expiry {{
            background: #fef3c7;
            border-left: 4px solid #f59e0b;
            padding: 15px;
            margin: 20px 0;
            border-radius: 4px;
        }}
        .expiry-text {{
            margin: 0;
            color: #92400e;
            font-size: 14px;
        }}
        .footer {{
            background: #f9fafb;
            padding: 20px 30px;
            text-align: center;
            color: #6b7280;
            font-size: 13px;
        }}
        .instructions {{
            background: #eff6ff;
            border-left: 4px solid #3b82f6;
            padding: 15px;
            margin: 20px 0;
            border-radius: 4px;
        }}
        .instructions p {{
            margin: 0;
            color: #1e40af;
        }}
    </style>
</head>
<body>
    <div class="container">
        <div class="header">
            <h1>🔐 AD4M Verification</h1>
        </div>
        <div class="content">
            <h2 style="margin-top: 0; color: #111827;">Email Verification Required</h2>
            <p>Enter this verification code in your application to {action}:</p>

            <div class="code-container">
                <div class="code-label">Your Verification Code</div>
                <div class="code">{code}</div>
            </div>

            <div class="instructions">
                <p><strong>How to use this code:</strong></p>
                <p>1. Return to the application where you requested this code</p>
                <p>2. Enter the 6-digit code shown above</p>
                <p>3. Your verification will be completed automatically</p>
            </div>

            <div class="expiry">
                <p class="expiry-text">⏰ <strong>Important:</strong> This code will expire in 15 minutes for security reasons.</p>
            </div>

            <p style="margin-top: 30px; color: #6b7280; font-size: 14px;">
                If you didn't request this code, you can safely ignore this email. Someone may have entered your email address by mistake.
            </p>
        </div>
        <div class="footer">
            <p>This is an automated message from your AD4M instance.</p>
            <p style="margin-top: 10px;">Please do not reply to this email.</p>
        </div>
    </div>
</body>
</html>
            "#,
            action = action,
            code = code
        );

        let text_body = format!(
            r#"
AD4M EMAIL VERIFICATION
========================

Enter this verification code in your application to {action}:

    {code}

HOW TO USE THIS CODE:
1. Return to the application where you requested this code
2. Enter the 6-digit code shown above
3. Your verification will be completed automatically

IMPORTANT: This code will expire in 15 minutes for security reasons.

If you didn't request this code, you can safely ignore this email.
Someone may have entered your email address by mistake.

---
This is an automated message from your AD4M instance.
Please do not reply to this email.
            "#,
            action = action,
            code = code
        );

        (html_body, text_body)
    }

    /// Internal method to send an email via SMTP
    async fn send_email(
        &self,
        to: &str,
        subject: &str,
        html_body: &str,
        text_body: &str,
    ) -> Result<(), AnyError> {
        let email = Message::builder()
            .from(self.smtp_config.from_address.parse()?)
            .to(to.parse()?)
            .subject(subject)
            .multipart(
                lettre::message::MultiPart::alternative()
                    .singlepart(
                        lettre::message::SinglePart::builder()
                            .header(lettre::message::header::ContentType::TEXT_PLAIN)
                            .body(text_body.to_string()),
                    )
                    .singlepart(
                        lettre::message::SinglePart::builder()
                            .header(lettre::message::header::ContentType::TEXT_HTML)
                            .body(html_body.to_string()),
                    ),
            )?;

        let creds = Credentials::new(
            self.smtp_config.username.clone(),
            self.smtp_config.password.clone(),
        );

        // Build SMTP transport with STARTTLS
        let mailer = SmtpTransport::relay(&self.smtp_config.host)?
            .credentials(creds)
            .port(self.smtp_config.port)
            .build();

        // Send the email
        tokio::task::spawn_blocking(move || mailer.send(&email))
            .await?
            .map_err(|e| anyhow::anyhow!("Failed to send email: {}", e))?;

        Ok(())
    }
}
