use dirs::home_dir;
use portpicker;
use std::path::PathBuf;
use std::sync::OnceLock;

static LANGUAGES_DIR: OnceLock<PathBuf> = OnceLock::new();

pub(crate) fn ad4m_data_directory() -> PathBuf {
    home_dir().unwrap().join(".ad4m")
}

/// Set the languages directory based on the app data path.
/// Must be called once during initialization before any language operations.
/// The languages directory will be `{app_data_path}/ad4m/languages/`.
pub fn set_languages_directory(app_data_path: &str) {
    let dir = PathBuf::from(app_data_path).join("ad4m").join("languages");
    let _ = LANGUAGES_DIR.set(dir);
}

pub fn languages_directory() -> PathBuf {
    LANGUAGES_DIR
        .get()
        .cloned()
        .unwrap_or_else(|| ad4m_data_directory().join("languages"))
}

pub fn language_storage_directory(language_address: &str) -> PathBuf {
    languages_directory().join(language_address)
}

pub fn find_port(start_port: u16, end_port: u16) -> Result<u16, String> {
    for x in start_port..end_port {
        if portpicker::is_free(x) {
            return Ok(x);
        }
    }

    Err(format!(
        "No open port found between: [{:?}, {:?}]",
        start_port, end_port
    ))
}

/// Constant-time comparison for security-sensitive values (prevents timing attacks)
/// This function always performs the same number of operations regardless of where
/// the first difference occurs, preventing timing-based side-channel attacks.
pub fn constant_time_eq(a: &str, b: &str) -> bool {
    let a_bytes = a.as_bytes();
    let b_bytes = b.as_bytes();

    // Always compare the maximum length to ensure constant-time behavior
    let max_len = a_bytes.len().max(b_bytes.len());
    let mut diff = 0u8;

    // Compare all bytes up to the maximum length
    // For indices beyond a string's length, XOR with 0 (no-op for padding)
    for i in 0..max_len {
        let a_byte = if i < a_bytes.len() { a_bytes[i] } else { 0 };
        let b_byte = if i < b_bytes.len() { b_bytes[i] } else { 0 };
        diff |= a_byte ^ b_byte;
    }

    // Also XOR the length difference to ensure different lengths are detected
    // Note: This only works if lengths fit in u8, but for tokens/hashes this is fine
    // For longer strings, we'd need a more sophisticated approach
    if a_bytes.len() <= 255 && b_bytes.len() <= 255 {
        diff |= (a_bytes.len() as u8) ^ (b_bytes.len() as u8);
    } else {
        // For very long strings, compare length bytes directly
        let len_diff = a_bytes.len() ^ b_bytes.len();
        diff |= (len_diff & 0xFF) as u8;
        diff |= ((len_diff >> 8) & 0xFF) as u8;
    }

    // Return true only if all bytes match and lengths are equal
    diff == 0
}

/// Rewrite the legacy double-slash `literal://<kind>:<value>` form (still
/// minted by Flux's TypeScript `Literal`, and still arriving via sync from
/// peers on pre-normalisation executors — see issue #1014) to the
/// single-colon `literal:<kind>:<value>` form everything else in this
/// executor speaks.
///
/// `literal://string:x` is *not* a parseable IRI — `string:x` reads as
/// `host:port` with a non-numeric port, so oxigraph's SPARQL parser rejects
/// `<literal://string:x>` outright and every query that inlines the value
/// fails. Normalising means an agent that passes the legacy spelling gets
/// the same node as one that passes the current spelling, instead of a hard
/// SPARQL error or an unmatchable `NamedNode::new_unchecked` target.
///
/// Lives here (not in `mcp`) because both the MCP tool layer and the flow
/// engine's role/tombstone matching need it: any comparison against a link
/// target that ignores the legacy spelling silently misses links written in
/// it.
pub(crate) fn normalize_legacy_literal(value: &str) -> std::borrow::Cow<'_, str> {
    match value.strip_prefix("literal://") {
        // Only the `literal://<kind>:…` shape; `literal://` alone is not one.
        Some(rest) if rest.contains(':') => std::borrow::Cow::Owned(format!("literal:{rest}")),
        _ => std::borrow::Cow::Borrowed(value),
    }
}

/// The *other* spelling of a `literal:` URI, or `None` if there isn't one.
///
/// The two spellings are mutually derivable — `literal://<kind>:<v>` ⇄
/// `literal:<kind>:<v>` — and a store that was written across the
/// normalisation boundary holds both for the same node, so a filter has to be
/// tried in both directions. Anything that is not a two-part `literal:` URI
/// (`ad4m://obj/…`, `did:key:…`, a bare `literal://`) has no counterpart and
/// yields `None`, so no second query is spent on it.
pub(crate) fn other_literal_spelling(value: &str) -> Option<String> {
    if let Some(rest) = value.strip_prefix("literal://") {
        // Mirrors `normalize_legacy_literal`: `literal://` alone is not the
        // `literal://<kind>:<value>` shape.
        return rest.contains(':').then(|| format!("literal:{rest}"));
    }
    let rest = value.strip_prefix("literal:")?;
    rest.contains(':').then(|| format!("literal://{rest}"))
}
