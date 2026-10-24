//! [`Secret`]: a secret value that zeroes itself, never prints, never serialises and never
//! clones. A secret leaves the client only into a vault's own storage call.

use std::fmt;

use zeroize::{Zeroize, Zeroizing};

pub struct Secret<T: Zeroize>(Zeroizing<T>);

impl<T: Zeroize> Secret<T> {
    pub fn new(value: T) -> Self {
        Secret(Zeroizing::new(value))
    }

    /// The value, for the one call that needs it.
    pub fn expose(&self) -> &T {
        &self.0
    }
}

impl<T: Zeroize> fmt::Debug for Secret<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("<redacted>")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_secret_never_prints() {
        let secret = Secret::new([7u8; 32]);
        assert_eq!(format!("{secret:?}"), "<redacted>");
        assert_eq!(secret.expose()[0], 7);
    }
}
