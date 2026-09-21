//! Does a language bundle actually use Holochain?
//!
//! Every route a Language v1 bundle has to the Holochain conductor leaves a
//! textual trace in the bundle source, because the host surfaces Holochain
//! only through named, unminifiable entry points:
//!
//! 1. **`ad4m:host` imports** — `holochainRegisterDnas`, `holochainCall`,
//!    `holochainCallAsync` (`rust-executor/src/js_core/host.js`). Both
//!    authoring paths keep the import statement verbatim in the bundle: the
//!    JS ALDK marks `ad4m:host` as external in esbuild, and wasm-bindgen
//!    emits it from `#[wasm_bindgen(module = "ad4m:host")]`
//!    (`ad4m-ldk/rust/src/imports.rs`). Export names in an import clause
//!    cannot be minified away — only the local binding can be renamed —
//!    so the clause is always recognisable.
//!
//!    Presence of the import alone is NOT usage: the ALDK's `imports`
//!    module re-exports the whole host surface, so every ALDK-built bundle
//!    imports all three `holochain*` names whether it calls them or not
//!    (the mainnet seed's language-language does, and never touches
//!    Holochain). Usage means the *local binding* of a `holochain*` import
//!    is referenced somewhere outside the import clauses.
//!
//! 2. **Runtime globals** — `__holochainDelegate__` (the per-language
//!    delegate `host.js` routes through) and `HOLOCHAIN_SERVICE` (the raw
//!    service global the delegate itself uses). A bundle touching either
//!    directly bypasses `ad4m:host` but still names the global, and esbuild
//!    does not rename globals or property names.
//!
//! 3. **Legacy constructor style** — `context.Holochain` from the pre-v1
//!    executor. Such bundles fail to load in the current runtime anyway
//!    (no `init()` export), but deferring them costs nothing and keeps the
//!    classifier honest about what the marker set covers.
//!
//! `handleHolochainSignal` is deliberately NOT a marker: the ALDK's
//! `defineLanguage` projection helper mentions it in every bundle it builds,
//! and exporting a signal handler never blocks on the conductor by itself —
//! signals only flow after `holochainRegisterDnas`, which marker 1 catches.
//!
//! **Fail safe:** anything the classifier cannot positively reason about is
//! treated as Holochain-using and deferred until the conductor is up —
//! an unreadable bundle, or an `ad4m:host` reference outside a recognised
//! named-import clause (namespace import, dynamic import). Deferral merely
//! restores the pre-detection behaviour for that language; a false "does
//! not use Holochain" would instead make its load block on the conductor
//! mid-startup, so the asymmetry is deliberate.

use log::warn;
use regex::Regex;
use std::path::Path;

/// Named-import clause from `ad4m:host`, spacing-agnostic so minified
/// bundles (`import{holochainCall as e}from"ad4m:host"`) match too.
/// Group 1 captures the names between the braces.
const AD4M_HOST_IMPORT: &str = r#"import\s*\{([^}]*)\}\s*from\s*["']ad4m:host["']"#;

/// Classify a bundle on disk. Unreadable ⇒ Holochain-using (fail safe).
pub fn bundle_file_uses_holochain(bundle_path: &Path) -> bool {
    match std::fs::read_to_string(bundle_path) {
        Ok(source) => bundle_uses_holochain(&source),
        Err(e) => {
            warn!(
                "Could not read language bundle {:?} for Holochain-usage classification: {} — treating it as Holochain-using (deferring load until the conductor is up)",
                bundle_path, e
            );
            true
        }
    }
}

/// Classify bundle source text. See the module doc for the marker set and
/// why each marker is reliable.
pub fn bundle_uses_holochain(source: &str) -> bool {
    let import_clause = Regex::new(AD4M_HOST_IMPORT).expect("static regex");

    // Local bindings of holochain* imports, and the source with all
    // recognised import clauses removed.
    let mut holochain_bindings: Vec<String> = Vec::new();
    for clause in import_clause.captures_iter(source) {
        let names = clause.get(1).map(|m| m.as_str()).unwrap_or("");
        for entry in names.split(',') {
            let mut parts = entry.split_whitespace();
            let (exported, local) = match (parts.next(), parts.next(), parts.next()) {
                (Some(name), None, _) => (name, name),
                (Some(name), Some("as"), Some(local)) => (name, local),
                _ => continue,
            };
            if exported.starts_with("holochain") {
                holochain_bindings.push(local.to_string());
            }
        }
    }
    let remaining = import_clause.replace_all(source, "");

    // `ad4m:host` referenced outside a recognised named-import clause:
    // namespace import, dynamic import, … — can't tell what it reaches,
    // so fail safe.
    if remaining.contains("ad4m:host") {
        return true;
    }

    // A holochain* import binding referenced outside the import clauses.
    for binding in &holochain_bindings {
        let word = Regex::new(&format!(r"\b{}\b", regex::escape(binding))).expect("escaped regex");
        if word.is_match(&remaining) {
            return true;
        }
    }

    // Direct globals and the legacy `context.Holochain` property.
    if remaining.contains("__holochainDelegate__") || remaining.contains("HOLOCHAIN_SERVICE") {
        return true;
    }
    Regex::new(r"\.Holochain\b")
        .expect("static regex")
        .is_match(&remaining)
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The full-surface import every ALDK-built bundle carries, used or not.
    const FULL_ALDK_IMPORT: &str = r#"import {
  agentDid,
  agentCreateSignedExpression,
  holochainRegisterDnas,
  holochainCall,
  holochainCallAsync,
  httpFetch,
  languageSettings,
  storageGet,
  storagePut
} from "ad4m:host";
"#;

    #[test]
    fn aldk_bundle_that_calls_holochain_is_detected() {
        let source = format!(
            r#"{FULL_ALDK_IMPORT}
export async function init() {{
    await holochainRegisterDnas([{{ nick: "main", bundle: BUNDLE }}]);
}}
export async function perspectiveSyncSync() {{
    return await holochainCall("main", "zome", "sync", null);
}}"#
        );
        assert!(bundle_uses_holochain(&source));
    }

    #[test]
    fn aldk_bundle_that_only_imports_holochain_is_not_detected() {
        // Mirrors the mainnet seed's language-language: the ALDK import
        // pulls in all holochain* names, but the body never uses them.
        let source = format!(
            r#"{FULL_ALDK_IMPORT}
export async function init() {{
    const settings = languageSettings();
    await httpFetch(settings.url, "GET", "", "");
}}"#
        );
        assert!(!bundle_uses_holochain(&source));
    }

    #[test]
    fn renamed_holochain_import_is_detected() {
        let source = r#"import { holochainCall as hcCall } from "ad4m:host";
export async function init() {}
export async function expressionGet(addr) { return await hcCall("dna", "zome", "get", addr); }"#;
        assert!(bundle_uses_holochain(source));
    }

    #[test]
    fn minified_import_and_call_site_is_detected() {
        let source = r#"import{holochainCall as e}from"ad4m:host";export async function init(){await e("d","z","f",null)}"#;
        assert!(bundle_uses_holochain(source));
    }

    #[test]
    fn namespace_import_of_ad4m_host_fails_safe() {
        let source = r#"import * as host from "ad4m:host";
export async function init() { host.storagePut("k", "v"); }"#;
        assert!(bundle_uses_holochain(source));
    }

    #[test]
    fn legacy_context_holochain_is_detected() {
        let source = r#"async function create(context) {
    const Holochain = context.Holochain;
    await Holochain.registerDNAs([{ file: BUNDLE, nick: "main" }]);
}"#;
        assert!(bundle_uses_holochain(source));
    }

    #[test]
    fn direct_delegate_and_service_globals_are_detected() {
        assert!(bundle_uses_holochain(
            r#"export async function init() { await globalThis.__holochainDelegate__.call("d","z","f",null); }"#
        ));
        assert!(bundle_uses_holochain(
            r#"export async function init() { await HOLOCHAIN_SERVICE.callZomeFunction("a","d","z","f",null); }"#
        ));
    }

    #[test]
    fn aldk_define_language_helper_alone_is_not_detected() {
        // Verbatim from the ALDK's defineLanguage projection, present in
        // every ALDK-built bundle whether or not the language uses Holochain.
        let source = r#"export async function init() {}
function project(spec) {
  const out = {};
  if (spec.handleHolochainSignal) {
    out.handleHolochainSignal = spec.handleHolochainSignal.bind(spec);
  }
  return out;
}"#;
        assert!(!bundle_uses_holochain(source));
    }

    #[test]
    fn bundle_without_any_markers_is_not_detected() {
        assert!(!bundle_uses_holochain(
            r#"export async function init() {}
export function expressionGet(addr) { return null; }"#
        ));
    }

    #[test]
    fn unreadable_bundle_file_fails_safe() {
        assert!(bundle_file_uses_holochain(Path::new(
            "/nonexistent/languages/QmDoesNotExist/bundle.js"
        )));
    }

    #[test]
    fn readable_bundle_file_is_classified_by_content() {
        let dir = tempfile::tempdir().expect("tempdir");
        let path = dir.path().join("bundle.js");
        std::fs::write(&path, "export async function init() {}").expect("write");
        assert!(!bundle_file_uses_holochain(&path));
    }

    /// The real mainnet seed's language-language bundle imports the full
    /// ALDK host surface but never calls Holochain — the exact false
    /// positive this classifier exists to avoid.
    #[test]
    fn mainnet_seed_language_language_is_not_holochain_using() {
        let seed: serde_json::Value =
            serde_json::from_str(include_str!("../mainnet_seed.json")).expect("parse seed");
        let bundle = seed["languageLanguageBundle"]
            .as_str()
            .expect("seed has languageLanguageBundle");
        assert!(
            bundle.contains("holochainRegisterDnas"),
            "seed bundle should carry the full ALDK import (test premise)"
        );
        assert!(!bundle_uses_holochain(bundle));
    }
}
