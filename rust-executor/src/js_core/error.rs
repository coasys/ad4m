use anyhow::Error as AnyhowError;
use std::borrow::Cow;

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct AnyhowWrapperError {
    inner: AnyhowError,
}

// Manual JsErrorClass impl for deno_error 0.7.x (deno v2.9). Trait shape:
//   fn get_class(&self) -> Cow<'static, str>;
//   fn get_message(&self) -> Cow<'static, str>;
//   fn get_additional_properties(&self) -> AdditionalProperties;
//   fn get_ref(&self) -> &(dyn std::error::Error + Send + Sync + 'static);
// (as_any was removed between 0.5 → 0.7; get_ref replaces the downcast
// escape hatch that used it.)
//
// Being explicit here maps every wrapped error to the generic JavaScript
// `Error` class with the anyhow chain's Display as the message. AD4M's op2
// sites all convert to this wrapper type, so a single impl here unblocks
// the 44 op2-macro E0277 sites in one commit.
impl deno_error::JsErrorClass for AnyhowWrapperError {
    fn get_class(&self) -> Cow<'static, str> {
        Cow::Borrowed(deno_error::builtin_classes::GENERIC_ERROR)
    }
    fn get_message(&self) -> Cow<'static, str> {
        Cow::Owned(self.inner.to_string())
    }
    fn get_additional_properties(&self) -> deno_error::AdditionalProperties {
        // deno_error 0.7.1 typedef: Box<dyn Iterator<Item = (Cow<'static, str>,
        // PropertyValue)>>. Empty iterator = no extra JS-side properties.
        Box::new(std::iter::empty())
    }
    fn get_ref(&self) -> &(dyn std::error::Error + Send + Sync + 'static) {
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
