use anyhow::Error as AnyhowError;

#[derive(Debug, thiserror::Error, deno_error::JsError)]
//#[property("code" = 10)]
#[class(generic)]
#[error(transparent)]
//#[js_error(name = "AnyhowWrapperError")]
pub struct AnyhowWrapperError {
    inner: AnyhowError,
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
