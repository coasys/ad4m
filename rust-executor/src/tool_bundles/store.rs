//! Where tool bundles are kept: one file per bundle, named by its content address.
//!
//! Apart from Languages on purpose. Every `bundle.js` under the languages directory is loaded as a
//! Language at boot, and a tool bundle is not one, so it lives in a sibling directory and is only
//! ever run in a [`BundleSandbox`](super::BundleSandbox).
//!
//! Addressed by content with the same function a Language's hash comes from
//! ([`content_address`]), so a bundle's name is a statement about its bytes: the same source is
//! stored once, and a file changed on disk no longer reads as the bundle it was stored as.

use crate::js_core::utils::content_address;
use crate::utils::languages_directory;
use std::fs;
use std::io;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct ToolBundleStore {
    dir: PathBuf,
}

impl ToolBundleStore {
    pub fn new(dir: impl Into<PathBuf>) -> Self {
        Self { dir: dir.into() }
    }

    /// `tool_bundles/`, beside the languages directory, so it follows the same data path.
    pub fn default_location() -> Self {
        let languages = languages_directory();
        let parent = languages.parent().map(PathBuf::from).unwrap_or(languages);
        Self::new(parent.join("tool_bundles"))
    }

    /// Store a bundle's source and answer with its address. Storing the same source again
    /// changes nothing.
    pub fn put(&self, source: &str) -> io::Result<String> {
        let address = content_address(source);
        let path = self.path_of(&address)?;
        if !path.exists() {
            fs::create_dir_all(&self.dir)?;
            // Written beside and renamed into place, so a reader never sees half a bundle.
            let partial = path.with_extension("js.partial");
            fs::write(&partial, source)?;
            fs::rename(&partial, &path)?;
        }
        Ok(address)
    }

    /// The source stored under `address`, or None when there is none. A file whose contents no
    /// longer match its address is an error rather than a bundle.
    pub fn get(&self, address: &str) -> io::Result<Option<String>> {
        let path = self.path_of(address)?;
        let source = match fs::read_to_string(&path) {
            Ok(source) => source,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(e) => return Err(e),
        };
        if content_address(&source) != address {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("tool bundle {address} no longer matches its address"),
            ));
        }
        Ok(Some(source))
    }

    /// Every stored bundle's address, sorted.
    pub fn list(&self) -> io::Result<Vec<String>> {
        let entries = match fs::read_dir(&self.dir) {
            Ok(entries) => entries,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Vec::new()),
            Err(e) => return Err(e),
        };
        let mut addresses = Vec::new();
        for entry in entries {
            let name = entry?.file_name();
            if let Some(address) = name.to_str().and_then(|n| n.strip_suffix(".js")) {
                if is_address(address) {
                    addresses.push(address.to_string());
                }
            }
        }
        addresses.sort();
        Ok(addresses)
    }

    /// Remove a stored bundle. Answers whether there was one.
    pub fn remove(&self, address: &str) -> io::Result<bool> {
        match fs::remove_file(self.path_of(address)?) {
            Ok(()) => Ok(true),
            Err(e) if e.kind() == io::ErrorKind::NotFound => Ok(false),
            Err(e) => Err(e),
        }
    }

    /// The file an address is kept in. Refuses anything that is not an address, so a caller's
    /// string can never name a path outside the store.
    fn path_of(&self, address: &str) -> io::Result<PathBuf> {
        if !is_address(address) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("not a tool bundle address: {address:?}"),
            ));
        }
        Ok(self.dir.join(format!("{address}.js")))
    }
}

/// What [`content_address`] writes: `Qm`, then base58.
fn is_address(text: &str) -> bool {
    text.len() > 2 && text.starts_with("Qm") && text.chars().all(|c| c.is_ascii_alphanumeric())
}
