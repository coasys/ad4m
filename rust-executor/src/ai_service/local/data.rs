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

/// The wind tunnel's recommendations: per measured class and service, the model and runtime
/// that won, with the server flags it was measured with. A service or provider this
/// executor does not run (a `completion` row, speech on speaches) stays raw JSON here and
/// [`Recommendations::get`] skips it.
#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Recommendations {
    pub schema_version: u32,
    pub generated_at: String,
    /// Measured classes only.
    pub classes: BTreeMap<String, BTreeMap<String, serde_json::Value>>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct Recommendation {
    pub provider: Provider,
    #[serde(default)]
    pub model_id: Option<String>,
    /// The manifest build key the measurement used (`linux-x64-vulkan`).
    #[serde(default)]
    pub runtime_build: Option<String>,
    #[serde(default)]
    pub backend: Option<String>,
    /// The server's flags in the measurement, e.g. `-c 8192 -ngl 999 --reasoning off`.
    #[serde(default)]
    pub server_flags: Vec<String>,
    #[serde(default)]
    pub metrics: serde_json::Value,
    /// Paths of the measurement files. A recommendation without evidence counts as not
    /// measured.
    #[serde(default)]
    pub evidence: Vec<String>,
}

impl Recommendations {
    /// The recommendation for `service` on `class`, when there is one and it names a
    /// provider this executor runs.
    pub fn get(&self, class: &str, service: Service) -> Option<Recommendation> {
        let raw = self.classes.get(class)?.get(service.as_str())?;
        serde_json::from_value(raw.clone()).ok()
    }
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct HardwareClasses {
    pub schema_version: u32,
    pub rules: Vec<ClassRule>,
}

#[derive(Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ClassRule {
    pub class: String,
    /// Classes of one family are ordered by `tier_gb`.
    pub family: String,
    pub tier_gb: f64,
    #[serde(rename = "match")]
    pub conditions: Conditions,
}

/// Every condition set must hold. Memory in GiB (2^30 bytes), compared with >=. The GPU
/// conditions (`gpuVendor`, `unifiedMemory`, `minVramGb`) hold when one GPU meets them all.
#[derive(Deserialize, Debug, Clone, PartialEq, Default)]
#[serde(rename_all = "camelCase", deny_unknown_fields)]
pub struct Conditions {
    pub os: Option<String>,
    pub arch: Option<String>,
    pub gpu_vendor: Option<String>,
    pub unified_memory: Option<bool>,
    /// At least one of these backends is usable.
    pub backend_any: Option<Vec<String>>,
    pub min_memory_gb: Option<f64>,
    pub min_vram_gb: Option<f64>,
}

/// What the class rules read of a device (the wind tunnel's `ClassInput`).
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ClassInput {
    /// `linux`, `macos`, `windows`.
    pub os: String,
    /// `x64`, `arm64`.
    pub arch: String,
    pub memory_bytes: u64,
    /// `cpu`, `cuda`, `rocm`, `vulkan`, `metal`.
    pub backends: Vec<String>,
    pub gpus: Vec<ClassGpu>,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
#[serde(rename_all = "camelCase")]
pub struct ClassGpu {
    /// `nvidia`, `amd`, `intel`, `apple`, `other`.
    pub vendor: String,
    pub vram_bytes: u64,
    /// An integrated GPU sharing system RAM.
    pub unified: bool,
}

const GIB: f64 = 1024.0 * 1024.0 * 1024.0;

impl Conditions {
    fn hold_for(&self, input: &ClassInput) -> bool {
        let same = |want: &Option<String>, value: &str| want.as_deref().is_none_or(|w| w == value);
        if !same(&self.os, &input.os) || !same(&self.arch, &input.arch) {
            return false;
        }
        if let Some(any) = &self.backend_any {
            if !any.iter().any(|b| input.backends.contains(b)) {
                return false;
            }
        }
        if self
            .min_memory_gb
            .is_some_and(|min| (input.memory_bytes as f64 / GIB) < min)
        {
            return false;
        }
        if self.gpu_vendor.is_some() || self.unified_memory.is_some() || self.min_vram_gb.is_some()
        {
            return input.gpus.iter().any(|gpu| {
                same(&self.gpu_vendor, &gpu.vendor)
                    && self.unified_memory.is_none_or(|u| u == gpu.unified)
                    && self
                        .min_vram_gb
                        .is_none_or(|min| gpu.vram_bytes as f64 / GIB >= min)
            });
        }
        true
    }
}

/// The class of the first rule that holds for `input`; the last rule's class when none does
/// (the rules end with a catch-all).
pub fn classify(classes: &HardwareClasses, input: &ClassInput) -> String {
    classes
        .rules
        .iter()
        .find(|rule| rule.conditions.hold_for(input))
        .or(classes.rules.last())
        .map(|rule| rule.class.clone())
        .unwrap_or_else(|| "cpu-8gb".to_string())
}

impl HardwareClasses {
    /// The classes to look in for a recommendation, nearest first: the class, the smaller
    /// tiers of its family (largest first), then the cpu family at the same or a smaller
    /// tier (what runs on less memory runs on more; a GPU class runs what a CPU class of
    /// its memory runs). The wind tunnel's `fallbackClasses`.
    pub fn chain(&self, class: &str) -> Vec<String> {
        let Some(own) = self.rules.iter().find(|r| r.class == class) else {
            return vec![class.to_string()];
        };
        let smaller = |family: &str, tier: f64| {
            let mut rules: Vec<&ClassRule> = self
                .rules
                .iter()
                .filter(|r| r.family == family && r.tier_gb <= tier)
                .collect();
            rules.sort_by(|a, b| b.tier_gb.total_cmp(&a.tier_gb));
            rules
                .into_iter()
                .map(|r| r.class.clone())
                .collect::<Vec<_>>()
        };
        let mut chain = vec![class.to_string()];
        chain.extend(smaller(&own.family, own.tier_gb));
        if own.family != "cpu" {
            chain.extend(smaller("cpu", own.tier_gb));
        }
        let mut seen = std::collections::BTreeSet::new();
        chain.retain(|c| seen.insert(c.clone()));
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
        fixtures: Vec<Case>,
    }

    #[derive(Deserialize)]
    struct Case {
        name: String,
        input: ClassInput,
        expected: String,
    }

    /// Each recommendation the executor can run names a manifest model of its service and
    /// provider, and a build the manifest has; each class it names is a rule's.
    #[test]
    fn the_embedded_data_parses_and_every_usable_recommendation_names_the_manifest() {
        let data = embedded().unwrap();
        let mut usable = 0;
        for class in data.recommendations.classes.keys() {
            assert!(
                data.classes.rules.iter().any(|r| &r.class == class),
                "{class} is no class of hardware-classes.json"
            );
            for service in Service::ALL {
                let Some(recommendation) = data.recommendations.get(class, service) else {
                    continue;
                };
                usable += 1;
                if let Some(model) = &recommendation.model_id {
                    let file = data.manifest.models.get(model).unwrap_or_else(|| {
                        panic!("{class}/{service:?}: {model} is not in the manifest")
                    });
                    assert_eq!(file.service, service, "{model}");
                    assert_eq!(file.provider, recommendation.provider, "{model}");
                }
                if let Some(build) = &recommendation.runtime_build {
                    assert!(
                        data.manifest
                            .runtimes
                            .values()
                            .any(|r| r.provider == recommendation.provider
                                && r.builds.contains_key(build)),
                        "{class}/{service:?}: no {build} build"
                    );
                }
            }
        }
        assert!(usable > 0, "no recommendation this executor can run");
        for (id, model) in &data.manifest.models {
            assert_eq!(model.sha256.len(), 64, "{id}");
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
        assert!(!fixtures.fixtures.is_empty());
        for case in fixtures.fixtures {
            assert_eq!(
                classify(classes, &case.input),
                case.expected,
                "{}",
                case.name
            );
        }
    }

    /// As the wind tunnel's `fallbackClasses`: the family's smaller tiers, largest first,
    /// then the cpu family at the same or a smaller tier.
    #[test]
    fn a_chain_walks_the_familys_smaller_tiers_then_the_cpu_family() {
        let classes = &embedded().unwrap().classes;
        assert_eq!(
            classes.chain("amd-igpu-uma-64"),
            vec![
                "amd-igpu-uma-64",
                "amd-igpu-uma-32",
                "amd-igpu-uma-16",
                "cpu-64gb",
                "cpu-32gb",
                "cpu-16gb",
                "cpu-8gb"
            ]
        );
        assert_eq!(
            classes.chain("nvidia-12gb"),
            vec!["nvidia-12gb", "nvidia-8gb", "cpu-8gb"]
        );
        assert_eq!(classes.chain("cpu-16gb"), vec!["cpu-16gb", "cpu-8gb"]);
        assert_eq!(classes.chain("unknown"), vec!["unknown"]);
    }

    /// A `completion` row and speech on speaches are the wind tunnel's: skipped, not an error.
    #[test]
    fn rows_for_services_or_providers_the_executor_does_not_run_are_skipped() {
        let data = embedded().unwrap();
        for (class, services) in &data.recommendations.classes {
            if let Some(tts) = services.get("tts") {
                if tts["provider"] != "llama-server" && tts["provider"] != "whisper-server" {
                    assert!(data.recommendations.get(class, Service::Tts).is_none());
                }
            }
        }
    }

    #[test]
    fn an_unknown_condition_is_refused_rather_than_ignored() {
        let rule = serde_json::from_value::<ClassRule>(serde_json::json!({
            "class": "x", "family": "cpu", "tierGb": 8, "match": { "minRamGb": 8 }
        }));
        assert!(rule.is_err());
    }
}
