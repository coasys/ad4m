//! The data local AI decides by, embedded in the executor at build time:
//!
//! - `manifest.json` (beside this file): the runtime builds and model files the executor may
//!   download, each pinned by URL, size and sha256.
//! - `ai-wind-tunnel/recommendations.json`: per measured hardware class and service, the
//!   model the wind tunnel recommends, with its metrics and evidence.
//! - `ai-wind-tunnel/hardware-classes.json`: ordered rules from a hardware fingerprint to a
//!   class, and the next smaller class of each (`smaller`), for classes without measurements.
//!
//! All three are schema version 1. The wind tunnel's TypeScript classifier passes the same
//! fixtures (`ai-wind-tunnel/hardware-class-fixtures.json`) as [`classify`].

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use ts_rs::TS;
use url::Url;

const MANIFEST_JSON: &str = include_str!("manifest.json");
const RECOMMENDATIONS_JSON: &str = include_str!("../../../../ai-wind-tunnel/recommendations.json");
const HARDWARE_CLASSES_JSON: &str =
    include_str!("../../../../ai-wind-tunnel/hardware-classes.json");

/// What a local runtime serves.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, TS)]
#[serde(rename_all = "lowercase")]
#[ts(rename = "LocalAiService")]
pub enum Service {
    Llm,
    Embeddings,
    /// Speech to text (transcription).
    Stt,
    /// Text to speech. No runtime serves it yet.
    Tts,
}

impl Service {
    pub const ALL: [Service; 4] = [
        Service::Llm,
        Service::Embeddings,
        Service::Stt,
        Service::Tts,
    ];

    pub fn model_type(self) -> crate::types::ModelType {
        use crate::types::ModelType;
        match self {
            Service::Llm => ModelType::Llm,
            Service::Embeddings => ModelType::Embedding,
            Service::Stt => ModelType::Transcription,
            Service::Tts => ModelType::Speech,
        }
    }

    pub fn capability(self) -> crate::types::AiCapability {
        (&self.model_type()).into()
    }

    pub fn as_str(self) -> &'static str {
        match self {
            Service::Llm => "llm",
            Service::Embeddings => "embeddings",
            Service::Stt => "stt",
            Service::Tts => "tts",
        }
    }
}

/// The server program a runtime build provides.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, TS)]
#[serde(rename_all = "kebab-case")]
#[ts(rename = "LocalAiProvider")]
pub enum Provider {
    /// llama.cpp's `llama-server` in router mode: chat and embeddings on one port.
    LlamaServer,
    /// whisper.cpp's `whisper-server`: `/v1/audio/transcriptions`.
    WhisperServer,
}

impl Provider {
    pub fn as_str(self) -> &'static str {
        match self {
            Provider::LlamaServer => "llama-server",
            Provider::WhisperServer => "whisper-server",
        }
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Manifest {
    pub schema_version: u32,
    pub runtimes: BTreeMap<String, Runtime>,
    pub models: BTreeMap<String, ModelFile>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Runtime {
    pub provider: Provider,
    pub version: String,
    /// By `<os>-<arch>-<backend>`, e.g. `linux-x86_64-vulkan`.
    pub builds: BTreeMap<String, Build>,
}

/// One downloadable archive of a runtime.
#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Build {
    pub url: Url,
    pub sha256: String,
    pub size_bytes: u64,
    /// The server program's path inside the unpacked archive.
    pub binary: String,
    /// More archives unpacked beside it: the CUDA runtime libraries for a CUDA build.
    #[serde(default)]
    pub extras: Vec<Extra>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Extra {
    pub url: Url,
    pub sha256: String,
    pub size_bytes: u64,
    /// The directory inside the unpacked archive that holds its libraries: the supervisor
    /// puts it on the library path. Empty: the archive's root.
    #[serde(default)]
    pub lib_dir: String,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ModelFile {
    pub service: Service,
    pub provider: Provider,
    pub url: Url,
    pub sha256: String,
    pub size_bytes: u64,
    pub license: String,
    /// Memory the model takes while loaded, weights and a default context together.
    pub memory_bytes: u64,
}

impl ModelFile {
    /// The file name the model is stored under: the last segment of its URL.
    pub fn file_name(&self) -> String {
        self.url
            .path_segments()
            .and_then(|mut segments| segments.next_back())
            .unwrap_or("model")
            .to_string()
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Recommendations {
    pub schema_version: u32,
    pub generated_at: String,
    /// Measured classes only.
    pub classes: BTreeMap<String, BTreeMap<Service, Recommendation>>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Recommendation {
    pub provider: Provider,
    #[serde(default)]
    pub model_id: Option<String>,
    /// A build key (`linux-x86_64-vulkan`) the measurement used, preferred when this device
    /// can run it.
    #[serde(default)]
    pub runtime_build: Option<String>,
    #[serde(default)]
    pub metrics: serde_json::Value,
    /// Paths of the measurement files. A recommendation without evidence counts as not
    /// measured.
    #[serde(default)]
    pub evidence: Vec<String>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct HardwareClasses {
    pub schema_version: u32,
    pub rules: Vec<ClassRule>,
    /// The next class down from each class; `null` for the smallest.
    pub smaller: BTreeMap<String, Option<String>>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
pub struct ClassRule {
    pub class: String,
    pub when: Conditions,
}

/// Every condition set must hold. A list matches when it names the fingerprint's value;
/// a GPU condition names `"none"` to match a fingerprint without a GPU.
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
#[serde(deny_unknown_fields)]
pub struct Conditions {
    #[serde(default)]
    pub os: Option<Vec<String>>,
    #[serde(default)]
    pub arch: Option<Vec<String>>,
    #[serde(default, rename = "gpuVendor")]
    pub gpu_vendor: Option<Vec<String>>,
    #[serde(default, rename = "gpuType")]
    pub gpu_type: Option<Vec<String>>,
    #[serde(default, rename = "minRamGiB")]
    pub min_ram_gib: Option<f64>,
    #[serde(default, rename = "minVramGiB")]
    pub min_vram_gib: Option<f64>,
}

/// What the class rules read of a device.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Fingerprint {
    pub os: String,
    pub arch: String,
    pub ram_bytes: u64,
    pub gpu: Option<FingerprintGpu>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct FingerprintGpu {
    /// `nvidia`, `amd`, `intel`, `apple` or `other`.
    pub vendor: String,
    /// `discrete`, `integrated` or `unified`.
    #[serde(rename = "type")]
    pub kind: String,
    pub vram_bytes: Option<u64>,
}

const GIB: f64 = 1024.0 * 1024.0 * 1024.0;

impl Conditions {
    fn hold_for(&self, fingerprint: &Fingerprint) -> bool {
        let listed = |list: &Option<Vec<String>>, value: &str| {
            list.as_ref()
                .is_none_or(|values| values.iter().any(|v| v == value))
        };
        let gpu = fingerprint.gpu.as_ref();
        listed(&self.os, &fingerprint.os)
            && listed(&self.arch, &fingerprint.arch)
            && listed(
                &self.gpu_vendor,
                gpu.map(|g| g.vendor.as_str()).unwrap_or("none"),
            )
            && listed(
                &self.gpu_type,
                gpu.map(|g| g.kind.as_str()).unwrap_or("none"),
            )
            && self
                .min_ram_gib
                .is_none_or(|min| fingerprint.ram_bytes as f64 / GIB >= min)
            && self.min_vram_gib.is_none_or(|min| {
                gpu.and_then(|g| g.vram_bytes)
                    .is_some_and(|vram| vram as f64 / GIB >= min)
            })
    }
}

/// The class of the first rule that holds for `fingerprint`; the last rule's class when
/// none does (the file ends with a catch-all).
pub fn classify(classes: &HardwareClasses, fingerprint: &Fingerprint) -> String {
    classes
        .rules
        .iter()
        .find(|rule| rule.when.hold_for(fingerprint))
        .or(classes.rules.last())
        .map(|rule| rule.class.clone())
        .unwrap_or_else(|| "cpu-8gb".to_string())
}

impl HardwareClasses {
    /// `class`, then each smaller class in turn, without repeats.
    pub fn chain(&self, class: &str) -> Vec<String> {
        let mut chain = vec![class.to_string()];
        let mut at = class.to_string();
        while let Some(Some(next)) = self.smaller.get(&at) {
            if chain.contains(next) {
                break;
            }
            chain.push(next.clone());
            at = next.clone();
        }
        chain
    }
}

/// The embedded data, parsed once.
pub struct Data {
    pub manifest: Manifest,
    pub recommendations: Recommendations,
    pub classes: HardwareClasses,
}

fn parse<T: serde::de::DeserializeOwned>(name: &str, json: &str) -> anyhow::Result<T> {
    serde_json::from_str(json).map_err(|e| anyhow::anyhow!("The embedded {name} is invalid: {e}"))
}

impl Data {
    pub fn parse(manifest: &str, recommendations: &str, classes: &str) -> anyhow::Result<Data> {
        let data = Data {
            manifest: parse("manifest.json", manifest)?,
            recommendations: parse("recommendations.json", recommendations)?,
            classes: parse("hardware-classes.json", classes)?,
        };
        for (name, version) in [
            ("manifest.json", data.manifest.schema_version),
            ("recommendations.json", data.recommendations.schema_version),
            ("hardware-classes.json", data.classes.schema_version),
        ] {
            if version != 1 {
                anyhow::bail!(
                    "The embedded {name} has schema version {version}; this executor reads 1"
                );
            }
        }
        Ok(data)
    }
}

lazy_static! {
    static ref EMBEDDED: Result<Data, String> =
        Data::parse(MANIFEST_JSON, RECOMMENDATIONS_JSON, HARDWARE_CLASSES_JSON)
            .map_err(|e| e.to_string());
}

/// The data embedded in this build. A unit test parses it, so this fails only on a build
/// whose tests did not run.
pub fn embedded() -> anyhow::Result<&'static Data> {
    EMBEDDED.as_ref().map_err(|e| anyhow::anyhow!("{e}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Deserialize)]
    struct Fixtures {
        cases: Vec<Case>,
    }

    #[derive(Deserialize)]
    struct Case {
        name: String,
        fingerprint: Fingerprint,
        expected: String,
    }

    #[test]
    fn the_embedded_data_parses_and_every_recommendation_names_a_manifest_model() {
        let data = embedded().unwrap();
        for (class, services) in &data.recommendations.classes {
            assert!(
                data.classes.smaller.contains_key(class),
                "{class} is no class of hardware-classes.json"
            );
            for (service, recommendation) in services {
                if let Some(model) = &recommendation.model_id {
                    let file = data.manifest.models.get(model).unwrap_or_else(|| {
                        panic!("{class}/{service:?}: {model} is not in the manifest")
                    });
                    assert_eq!(file.service, *service, "{model}");
                    assert_eq!(file.provider, recommendation.provider, "{model}");
                }
            }
        }
        for rule in &data.classes.rules {
            assert!(
                data.classes.smaller.contains_key(&rule.class),
                "{}",
                rule.class
            );
        }
        for (id, model) in &data.manifest.models {
            assert_eq!(model.sha256.len(), 64, "{id}");
            assert!(
                model.url.as_str().contains("/resolve/"),
                "{id}: pin a commit"
            );
            assert!(
                !model.url.as_str().contains("/resolve/main/"),
                "{id}: pin a commit, not main"
            );
        }
        for runtime in data.manifest.runtimes.values() {
            for (key, build) in &runtime.builds {
                assert_eq!(key.split('-').count(), 3, "{key} is <os>-<arch>-<backend>");
                assert_eq!(build.sha256.len(), 64, "{key}");
            }
        }
    }

    /// The same fixtures the wind tunnel's TypeScript classifier passes.
    #[test]
    fn the_classifier_passes_the_shared_fixtures() {
        let fixtures: Fixtures = serde_json::from_str(include_str!(
            "../../../../ai-wind-tunnel/hardware-class-fixtures.json"
        ))
        .unwrap();
        let classes = &embedded().unwrap().classes;
        assert!(!fixtures.cases.is_empty());
        for case in fixtures.cases {
            assert_eq!(
                classify(classes, &case.fingerprint),
                case.expected,
                "{}",
                case.name
            );
        }
    }

    #[test]
    fn a_chain_walks_smaller_classes_and_stops_at_a_loop() {
        let classes: HardwareClasses = serde_json::from_value(serde_json::json!({
            "schemaVersion": 1,
            "rules": [{ "class": "a", "when": {} }],
            "smaller": { "a": "b", "b": "c", "c": "a" }
        }))
        .unwrap();
        assert_eq!(classes.chain("a"), vec!["a", "b", "c"]);
        assert_eq!(classes.chain("unknown"), vec!["unknown"]);
    }

    #[test]
    fn an_unknown_condition_is_refused_rather_than_ignored() {
        let rule = serde_json::from_value::<ClassRule>(serde_json::json!({
            "class": "x", "when": { "minRamGb": 8 }
        }));
        assert!(rule.is_err());
    }
}
