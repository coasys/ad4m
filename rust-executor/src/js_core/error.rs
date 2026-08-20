use anyhow::Error as AnyhowError;
use std::any::Any;
use std::borrow::Cow;

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct AnyhowWrapperError {
    inner: AnyhowError,
}

// Manual JsErrorClass impl because deno_error::JsError derive-macro path
// stopped covering `anyhow::Error`-transparent-wrapper types in deno 2.9's
// deno_error 0.5.x. Being explicit here maps every wrapped error to the
// generic JavaScript `Error` class with the anyhow chain's Display as the
// message. AD4M's op2 sites all convert to this wrapper type, so a single
// impl here unblocks all 44 op2-macro E0277 sites in one commit.
impl deno_error::JsErrorClass for AnyhowWrapperError {
    fn get_class(&self) -> Cow<'static, str> {
        Cow::Borrowed(deno_error::builtin_classes::GENERIC_ERROR)
    }
    fn get_message(&self) -> Cow<'static, str> {
        Cow::Owned(self.inner.to_string())
    }
    fn get_additional_properties(&self) -> Vec<(Cow<'static, str>, Cow<'static, str>)> {
        Vec::new()
    }
    fn as_any(&self) -> &dyn Any {
        self
    }
}

impl From<AnyhowError> for AnyhowWrapperError {
    fn from(error: AnyhowError) -> Self {
        AnyhowWrapperError { inner: error }
    }
}

impl From<serde_json::Error> for AnyhowWrapperError {
    fn from(error: serde_json::Error) -> Self {
        AnyhowWrapperError {
            inner: error.into(),
        }
    }
}

// impl std::fmt::Display for AnyhowWrapperError {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         write!(f, "{}", self.inner)
//     }
// }

// impl std::error::Error for AnyhowWrapperError {
//     fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
//         Some(&*self.inner)
//     }
// }
