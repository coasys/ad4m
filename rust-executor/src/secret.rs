//! [`Secret`]: a credential value that cannot leave the executor by accident.
//!
//! A `Secret` has no `Serialize` and no `Display`, and its `Debug` prints
//! `Secret(<redacted>)`. So a secret inside a struct that is logged, sent as an
//! RPC result or published as an event either fails to compile or shows as
//! redacted. Code reads the value only through three named doors:
//!
//! | Method | Use |
//! |---|---|
//! | [`Secret::expose_for_compare`] | check a presented value, in constant time |
//! | [`Secret::expose_once`] | hand the value to its one rightful receiver; consumes the secret |
//! | [`Secret::expose`] | read the value in place, where it must reach a receiver and the secret stays; every call site says why |

use serde::{Deserialize, Deserializer, Serializer};
use std::fmt;

/// Holds a credential. See the module documentation.
///
/// `Clone` exists because the configs that hold a secret (`Ad4mConfig`,
/// `AppState`, `McpContext`) are cloned into every service that needs them.
/// A clone is another `Secret`, never a plain `String`.
#[derive(Clone)]
pub struct Secret(String);

impl Secret {
    pub fn new(value: impl Into<String>) -> Self {
        Secret(value.into())
    }

    /// `None` for an empty value: an empty credential is no credential.
    pub fn non_empty(value: Option<String>) -> Option<Secret> {
        value.filter(|value| !value.is_empty()).map(Secret)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Whether `other` equals the secret, in constant time.
    pub fn expose_for_compare(&self, other: &str) -> bool {
        crate::utils::constant_time_eq(&self.0, other)
    }

    /// The value, for its one rightful receiver. Consumes the secret.
    pub fn expose_once(self) -> String {
        self.0
    }

    /// The value, read in place. Every caller documents why it must read it.
    pub fn expose(&self) -> &str {
        &self.0
    }
}

impl fmt::Debug for Secret {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("Secret(<redacted>)")
    }
}

impl From<String> for Secret {
    fn from(value: String) -> Self {
        Secret(value)
    }
}

impl From<&str> for Secret {
    fn from(value: &str) -> Self {
        Secret(value.to_string())
    }
}

/// Reading a secret in (a config, a request) is safe; only writing it out is not.
impl<'de> Deserialize<'de> for Secret {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        String::deserialize(deserializer).map(Secret)
    }
}

/// For `#[serde(serialize_with = "crate::secret::serialize_redacted")]` on an
/// `Option<Secret>` field of a serialisable struct: a set secret serialises as
/// [`crate::config_file::REDACTED`], an unset one as `null`. The value never
/// reaches the output.
pub fn serialize_redacted<S: Serializer>(
    value: &Option<Secret>,
    serializer: S,
) -> Result<S::Ok, S::Error> {
    match value {
        Some(_) => serializer.serialize_str(crate::config_file::REDACTED),
        None => serializer.serialize_none(),
    }
}

/// Compile-time proof that `Secret` implements neither `Serialize` nor
/// `Display`. `static_assertions::assert_not_impl_any!` does the same, but
/// that crate is not a direct dependency; this is its technique, inlined.
///
/// If `Secret` implemented the trait, both impls of `AmbiguousIfImpl` below
/// would apply to it, the `_` in the call could not be inferred, and the
/// crate would not compile. Without the trait only the blanket impl applies.
macro_rules! assert_not_impl {
    ($ty:ty: $($tr:tt)+) => {
        const _: fn() = || {
            trait AmbiguousIfImpl<A> {
                fn some_item() {}
            }
            impl<T: ?Sized> AmbiguousIfImpl<()> for T {}
            #[allow(dead_code)]
            struct Invalid;
            impl<T: ?Sized + $($tr)+> AmbiguousIfImpl<Invalid> for T {}
            let _ = <$ty as AmbiguousIfImpl<_>>::some_item;
        };
    };
}

assert_not_impl!(Secret: serde::Serialize);
assert_not_impl!(Secret: std::fmt::Display);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_never_prints_the_value() {
        let secret = Secret::new("hunter2");
        assert_eq!(format!("{secret:?}"), "Secret(<redacted>)");
        assert_eq!(
            format!("{:#?}", Some(secret)),
            "Some(\n    Secret(<redacted>),\n)"
        );
    }

    // The length check covers every byte of the length: the value followed by 65536 NUL
    // bytes differed from it only above the low 16 bits, which went unchecked.
    #[test]
    fn compare_refuses_the_value_with_padding_of_any_length() {
        let secret = Secret::new("s3cret");
        for pad in [1usize, 256, 65536, 65536 * 2] {
            assert!(!secret.expose_for_compare(&format!("s3cret{}", "\0".repeat(pad))));
        }
    }

    #[test]
    fn compare_matches_only_the_exact_value() {
        let secret = Secret::new("hunter2");
        assert!(secret.expose_for_compare("hunter2"));
        for other in ["", "hunter", "hunter22", "Hunter2", "hunter2 "] {
            assert!(!secret.expose_for_compare(other), "{other:?}");
        }
    }

    #[test]
    fn non_empty_drops_only_the_empty_value() {
        assert!(Secret::non_empty(Some(String::new())).is_none());
        assert!(Secret::non_empty(None).is_none());
        assert!(Secret::non_empty(Some(" ".into()))
            .unwrap()
            .expose_for_compare(" "));
    }

    #[test]
    fn the_value_comes_out_only_through_the_named_doors() {
        let secret = Secret::new("hunter2");
        assert_eq!(secret.expose(), "hunter2");
        assert_eq!(secret.expose_once(), "hunter2");
    }

    #[test]
    fn deserialises_from_a_string() {
        let secret: Secret = serde_json::from_str("\"hunter2\"").unwrap();
        assert!(secret.expose_for_compare("hunter2"));
    }

    #[test]
    fn serialize_redacted_writes_the_marker_or_null() {
        #[derive(serde::Serialize)]
        struct Holder {
            #[serde(serialize_with = "serialize_redacted")]
            secret: Option<Secret>,
        }
        let set = serde_json::to_string(&Holder {
            secret: Some("hunter2".into()),
        })
        .unwrap();
        assert_eq!(set, r#"{"secret":"<redacted>"}"#);
        let unset = serde_json::to_string(&Holder { secret: None }).unwrap();
        assert_eq!(unset, r#"{"secret":null}"#);
    }
}
