//! The install plan (`ai.localPlan`): from this device, the wind tunnel's recommendations
//! and the manifest, which runtime builds and models to fetch, how much that downloads, and
//! whether it fits. Pure: no I/O, so every rule has a unit test.
//!
//! Rules:
//! - The recommendation of a service comes from the device's class, or the nearest smaller
//!   measured class ([`super::data::HardwareClasses::chain`]: the family's smaller tiers,
//!   then the cpu family). A recommendation without evidence, or naming a model the
//!   manifest lacks, counts as not measured. Its `serverFlags` go to the server with the
//!   model; without any, the defaults ([`default_flags`]: no thinking for chat).
//! - A service with no measured recommendation (or one whose runtime has no build for this
//!   device) takes the manifest's smallest model of it, by memory then size, whose runtime
//!   runs here ([`default_candidate`]), marked [`Basis::Default`]: the setup offers
//!   something on every device, and a measured recommendation always wins.
//! - The models of all services run at once, so their memory adds up. It fits when the sum
//!   is at most the budget, min(GPU budget − 1 GiB, RAM − 4 GiB). When it does not, the
//!   service with the largest model steps down its chain, until it fits or nothing can step.
//! - The runtime build of a provider is the recommendation's build when this device can run
//!   it, else the best backend this device has that the manifest builds (CUDA, Metal,
//!   Vulkan, ROCm, CPU). Vulkan replaces a recommended ROCm build where it runs: the ROCm
//!   build runs on the CPU, unannounced, unless the system ROCm libraries load, and the
//!   wind tunnel measured Vulkan as the AMD iGPU's chat backend. One llama-server serves
//!   chat and embeddings, so they share the chat model's build.
//! - The download must fit the free disk space with 1 GiB to spare.

use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use ts_rs::TS;

use super::data::{Data, Provider, Recommendation, Service};
use super::hardware::{Backend, LocalHardware, GIB};

/// Backends best first.
pub(crate) const BACKEND_PREFERENCE: [Backend; 5] = [
    Backend::Cuda,
    Backend::Metal,
    Backend::Vulkan,
    Backend::Rocm,
    Backend::Cpu,
];

/// The services a plan offers; TTS has no runtime yet.
pub const PLANNED_SERVICES: [Service; 3] = [Service::Llm, Service::Embeddings, Service::Stt];

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiPlannedRuntime")]
pub struct PlannedRuntime {
    /// The manifest's runtime id: `llama-server`, `nemo-speech`.
    pub runtime_id: String,
    pub provider: Provider,
    pub version: String,
    /// The manifest's build key: `linux-x64-vulkan`.
    pub build: String,
    pub backend: Backend,
    /// The archive and its extras.
    #[ts(type = "number")]
    pub size_bytes: u64,
    pub installed: bool,
    pub services: Vec<Service>,
}

/// What a planned model is chosen by.
#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, TS)]
#[serde(rename_all = "lowercase")]
#[ts(rename = "LocalAiBasis")]
pub enum Basis {
    /// The wind tunnel measured it on this device's class or a smaller one.
    Measured,
    /// Not measured on this hardware: the smallest model the manifest has for the service.
    Default,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiPlannedModel")]
pub struct PlannedModel {
    pub service: Service,
    /// The manifest's model id, also the id the runtime serves it by.
    pub model_id: String,
    pub provider: Provider,
    #[ts(type = "number")]
    pub size_bytes: u64,
    #[ts(type = "number")]
    pub memory_bytes: u64,
    pub license: String,
    pub installed: bool,
    /// `measured`: the wind tunnel's recommendation. `default`: no class on the chain was
    /// measured for this service, so the plan takes the manifest's smallest model.
    pub basis: Basis,
    /// The measured class the recommendation comes from; for a default, this device's.
    pub from_class: String,
    /// Empty for a default.
    pub evidence: Vec<String>,
    #[ts(type = "Record<string, unknown>")]
    pub metrics: serde_json::Value,
    /// The flags the server runs this model with: the measurement's, else the defaults.
    pub server_flags: Vec<String>,
}

/// A service the plan leaves out, and why.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiUnavailable")]
pub struct Unavailable {
    pub service: Service,
    pub reason: String,
}

/// What `ai.localInstall` would do on this device.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "LocalAiPlan")]
pub struct LocalPlan {
    pub hardware_class: String,
    pub runtimes: Vec<PlannedRuntime>,
    pub models: Vec<PlannedModel>,
    pub unavailable: Vec<Unavailable>,
    /// What is not downloaded yet.
    #[ts(type = "number")]
    pub download_bytes: u64,
    /// Everything the plan installs, downloaded or not.
    #[ts(type = "number")]
    pub total_bytes: u64,
    /// The planned models' memory, together.
    #[ts(type = "number")]
    pub memory_bytes: u64,
    #[ts(type = "number")]
    pub memory_budget_bytes: u64,
    #[ts(type = "number | null")]
    pub disk_free_bytes: Option<u64>,
    /// The models fit the memory budget and the download fits the disk.
    pub fits: bool,
    /// Why it does not fit, and what the plan stepped down or runs on the CPU.
    pub reasons: Vec<String>,
    /// The recommendations' `generatedAt`.
    pub recommendations_generated_at: String,
    /// The operator's device (`local_ai_device`); `null`: the best one detection found.
    pub device: Option<Backend>,
}

/// One service's candidates: the recommendation of each measured class along the chain,
/// largest first, without repeating a model.
fn candidates(data: &Data, chain: &[String], service: Service) -> Vec<(String, Recommendation)> {
    let mut seen = BTreeSet::new();
    chain
        .iter()
        .filter_map(|class| {
            let recommendation = data.recommendations.get(class, service)?;
            let model_id = recommendation.model_id.as_ref()?;
            let measured = !recommendation.evidence.is_empty();
            let known = data
                .manifest
                .models
                .get(model_id)
                .is_some_and(|m| m.service == service && m.provider == recommendation.provider);
            (measured && known && seen.insert(model_id.clone()))
                .then(|| (class.clone(), recommendation))
        })
        .collect()
}

/// The default of a service no class on the chain was measured for: the manifest's smallest
/// model of it (by memory, then download size) whose runtime this device runs, from this
/// device's class, with no evidence, metrics or flags of its own.
fn default_candidate(data: &Data, hardware: &LocalHardware, service: Service) -> Option<Candidate> {
    let (model_id, file) = data
        .manifest
        .models
        .iter()
        .filter(|(_, m)| {
            m.service == service && runtime_build(data, hardware, m.provider, None).is_some()
        })
        .min_by_key(|(id, m)| (m.memory_bytes, m.size_bytes, id.as_str()))?;
    Some((
        hardware.hardware_class.clone(),
        Recommendation {
            provider: file.provider,
            model_id: Some(model_id.clone()),
            runtime_build: None,
            backend: None,
            server_flags: vec![],
            metrics: serde_json::json!({}),
            evidence: vec![],
        },
    ))
}

/// The build of `provider`'s runtime for this device, the backend it uses, and why it is
/// not the preferred one when it is not.
fn runtime_build<'a>(
    data: &'a Data,
    hardware: &LocalHardware,
    provider: Provider,
    preferred: Option<&str>,
) -> Option<(
    &'a String,
    &'a super::data::Runtime,
    String,
    Backend,
    Option<String>,
)> {
    let (runtime_id, runtime) = data
        .manifest
        .runtimes
        .iter()
        .find(|(_, r)| r.provider == provider)?;
    let platform = hardware.platform();
    let usable = |key: &str| {
        let backend = key
            .strip_prefix(&format!("{platform}-"))
            .and_then(Backend::from_key_suffix)?;
        (hardware.backends.contains(&backend) && runtime.builds.contains_key(key))
            .then_some(backend)
    };
    if let Some(key) = preferred {
        if let Some(backend) = usable(key) {
            let vulkan = format!("{platform}-vulkan");
            if backend == Backend::Rocm && usable(&vulkan).is_some() {
                return Some((
                    runtime_id,
                    runtime,
                    vulkan.clone(),
                    Backend::Vulkan,
                    Some(format!(
                        "{runtime_id} runs {vulkan}, not the recommended {key}: the ROCm build falls back to the CPU without the system ROCm libraries"
                    )),
                ));
            }
            return Some((runtime_id, runtime, key.to_string(), backend, None));
        }
    }
    BACKEND_PREFERENCE.iter().find_map(|backend| {
        let key = format!("{platform}-{}", backend.as_str());
        usable(&key).map(|backend| {
            let note = preferred.map(|p| {
                format!("{runtime_id} runs {key}: this device cannot run the recommended {p}")
            });
            (runtime_id, runtime, key, backend, note)
        })
    })
}

/// The server flags of a model without measured ones: chat without thinking (Qwen3.5 thinks
/// until the context is full and `ai.prompt` answers ""), embeddings only on the embedding
/// model. nemo-speech needs none: the executor sets its device and turns its playground off.
pub fn default_flags(service: Service) -> Vec<String> {
    let flags: &[&str] = match service {
        Service::Llm => &["--reasoning", "off"],
        Service::Embeddings => &["--embeddings"],
        Service::Stt | Service::Tts => &[],
    };
    flags.iter().map(|f| f.to_string()).collect()
}

/// The plan with the operator's device (`local_ai_device`): only that backend's builds,
/// and the CPU's for a runtime the manifest builds no other way, are used, and the budget
/// and the class follow from it (a device held to the CPU plans by its RAM). The device is
/// used also when detection did not find it, with a reason: detection can miss a driver.
/// `None`: the best backend detection found, as [`plan`].
pub fn plan_with_device(
    data: &Data,
    hardware: &LocalHardware,
    device: Option<Backend>,
    installed: &dyn Fn(&str) -> bool,
) -> LocalPlan {
    let Some(device) = device else {
        return plan(data, hardware, installed);
    };
    let mut held = hardware.clone();
    held.backends = if device == Backend::Cpu {
        vec![Backend::Cpu]
    } else {
        vec![device, Backend::Cpu]
    };
    super::hardware::finish(&mut held, &data.classes);
    let mut planned = plan(data, &held, installed);
    planned.device = Some(device);
    planned.reasons.insert(
        0,
        if hardware.backends.contains(&device) {
            format!("local_ai_device is {}: the plan uses no other GPU backend", device.as_str())
        } else {
            format!(
                "local_ai_device is {}, which detection did not find on this device: a {} build that finds no device runs on the CPU",
                device.as_str(),
                device.as_str()
            )
        },
    );
    planned
}

/// A recommendation and the measured class it comes from.
type Candidate = (String, Recommendation);

/// A service, its candidates largest first, and the one the plan is at.
type Pick = (Service, Vec<Candidate>, usize);

/// The plan for `hardware`. `installed(sha256)` says whether a file with that digest is
/// in the store already.
pub fn plan(data: &Data, hardware: &LocalHardware, installed: &dyn Fn(&str) -> bool) -> LocalPlan {
    let chain = data.classes.chain(&hardware.hardware_class);
    let mut reasons = Vec::new();
    let mut unavailable = Vec::new();

    // Each service: its candidates, and which one the plan is at.
    let mut picks: Vec<Pick> = Vec::new();
    for service in PLANNED_SERVICES {
        // The measured candidates when this device runs their runtime, else the default.
        let measured = candidates(data, &chain, service);
        let unrunnable = measured
            .first()
            .map(|(_, r)| r.provider)
            .filter(|p| runtime_build(data, hardware, *p, None).is_none());
        let found: Vec<Candidate> = if measured.is_empty() || unrunnable.is_some() {
            default_candidate(data, hardware, service)
                .into_iter()
                .collect()
        } else {
            measured
        };
        if found.is_empty() {
            let reason = match unrunnable {
                Some(provider) => format!(
                    "No {} build for {} in the manifest",
                    provider.as_str(),
                    hardware.platform()
                ),
                None => format!(
                    "No {} model in the manifest runs on {}",
                    service.as_str(),
                    hardware.platform()
                ),
            };
            unavailable.push(Unavailable { service, reason });
            continue;
        }
        picks.push((service, found, 0));
    }

    let memory_of = |picks: &[Pick]| -> u64 {
        picks
            .iter()
            .map(|(_, found, at)| {
                let id = found[*at].1.model_id.as_deref().unwrap_or_default();
                data.manifest
                    .models
                    .get(id)
                    .map(|m| m.memory_bytes)
                    .unwrap_or(0)
            })
            .sum()
    };
    let budget = hardware.memory_budget_bytes;
    while memory_of(&picks) > budget {
        // The largest model that has a smaller one steps down.
        let step = picks
            .iter()
            .enumerate()
            .filter(|(_, (_, found, at))| at + 1 < found.len())
            .max_by_key(|(_, (_, found, at))| {
                let id = found[*at].1.model_id.as_deref().unwrap_or_default();
                data.manifest
                    .models
                    .get(id)
                    .map(|m| m.memory_bytes)
                    .unwrap_or(0)
            })
            .map(|(i, _)| i);
        let Some(i) = step else { break };
        let (service, found, at) = &mut picks[i];
        reasons.push(format!(
            "{} steps down from {} to {} to fit the memory budget",
            service.as_str(),
            found[*at].1.model_id.as_deref().unwrap_or_default(),
            found[*at + 1].1.model_id.as_deref().unwrap_or_default(),
        ));
        *at += 1;
    }
    let memory_bytes = memory_of(&picks);
    let memory_fits = memory_bytes <= budget;
    if !memory_fits {
        reasons.push(format!(
            "The models need {:.1} GiB together; this device has {:.1} GiB for them (min(GPU − 1 GiB, RAM − 4 GiB))",
            memory_bytes as f64 / GIB as f64,
            budget as f64 / GIB as f64
        ));
    }

    let mut models = Vec::new();
    let mut runtimes: Vec<PlannedRuntime> = Vec::new();
    for (service, found, at) in &picks {
        let (from_class, recommendation) = &found[*at];
        let model_id = recommendation.model_id.clone().unwrap_or_default();
        let file = &data.manifest.models[&model_id];
        models.push(PlannedModel {
            service: *service,
            model_id,
            provider: file.provider,
            size_bytes: file.size_bytes,
            memory_bytes: file.memory_bytes,
            license: file.license.clone(),
            installed: installed(&file.sha256),
            // Every measured candidate has evidence ([`candidates`]); a default has none.
            basis: if recommendation.evidence.is_empty() {
                Basis::Default
            } else {
                Basis::Measured
            },
            from_class: from_class.clone(),
            evidence: recommendation.evidence.clone(),
            metrics: recommendation.metrics.clone(),
            server_flags: if recommendation.server_flags.is_empty() {
                default_flags(*service)
            } else {
                recommendation.server_flags.clone()
            },
        });
        if let Some(existing) = runtimes.iter_mut().find(|r| r.provider == file.provider) {
            existing.services.push(*service);
            if let Some(wanted) = recommendation
                .runtime_build
                .as_ref()
                .filter(|w| **w != existing.build)
            {
                reasons.push(format!(
                    "{} runs on {}'s {} ({} was measured on {wanted}): one {} serves both",
                    service.as_str(),
                    existing.services[0].as_str(),
                    existing.build,
                    service.as_str(),
                    existing.runtime_id
                ));
            }
            continue;
        }
        let Some((runtime_id, runtime, key, backend, note)) = runtime_build(
            data,
            hardware,
            file.provider,
            recommendation.runtime_build.as_deref(),
        ) else {
            continue;
        };
        let build = &runtime.builds[&key];
        reasons.extend(note);
        if backend == Backend::Cpu && hardware.backends.iter().any(|b| *b != Backend::Cpu) {
            reasons.push(format!(
                "{} has no GPU build for {} in the manifest: {} runs on the CPU",
                runtime_id,
                hardware.platform(),
                service.as_str()
            ));
        }
        runtimes.push(PlannedRuntime {
            runtime_id: runtime_id.clone(),
            provider: file.provider,
            version: runtime.version.clone(),
            build: key.clone(),
            backend,
            size_bytes: build.size_bytes + build.extras.iter().map(|e| e.size_bytes).sum::<u64>(),
            installed: installed(&build.sha256)
                && build.extras.iter().all(|e| installed(&e.sha256)),
            services: vec![*service],
        });
    }

    let total_bytes = runtimes.iter().map(|r| r.size_bytes).sum::<u64>()
        + models.iter().map(|m| m.size_bytes).sum::<u64>();
    let download_bytes = runtimes
        .iter()
        .filter(|r| !r.installed)
        .map(|r| r.size_bytes)
        .sum::<u64>()
        + models
            .iter()
            .filter(|m| !m.installed)
            .map(|m| m.size_bytes)
            .sum::<u64>();
    let disk_fits = hardware
        .disk_free_bytes
        .is_none_or(|free| download_bytes + GIB <= free);
    if !disk_fits {
        reasons.push(format!(
            "The download is {:.1} GiB; the disk has {:.1} GiB free (1 GiB stays free)",
            download_bytes as f64 / GIB as f64,
            hardware.disk_free_bytes.unwrap_or(0) as f64 / GIB as f64
        ));
    }

    LocalPlan {
        hardware_class: hardware.hardware_class.clone(),
        runtimes,
        fits: memory_fits && disk_fits && !models.is_empty(),
        models,
        unavailable,
        download_bytes,
        total_bytes,
        memory_bytes,
        memory_budget_bytes: budget,
        disk_free_bytes: hardware.disk_free_bytes,
        reasons,
        recommendations_generated_at: data.recommendations.generated_at.clone(),
        device: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ai_service::local::hardware::{CpuInfo, GpuInfo, GpuType, GpuVendor};
    use serde_json::json;

    fn manifest() -> serde_json::Value {
        let build = |name: &str| json!({ "url": format!("https://example.org/{name}.tar.gz"), "sha256": format!("{:0>64}", name.len()), "sizeBytes": 10_000_000, "binary": "bin/server" });
        let model = |service: &str, provider: &str, size: u64, memory: u64| {
            json!({ "service": service, "provider": provider, "url": format!("https://huggingface.co/r/resolve/abc/{service}-{memory}.gguf"),
                    "sha256": format!("{memory:0>64}"), "sizeBytes": size, "license": "MIT", "memoryBytes": memory })
        };
        json!({
            "schemaVersion": 1,
            "runtimes": {
                "llama-server": { "provider": "llama-server", "version": "b1", "builds": {
                    "linux-x64-cpu": build("lcpu"), "linux-x64-vulkan": build("lvulkan"),
                    "linux-x64-rocm": build("lrocm"), "linux-x64-cuda": build("lcuda") } },
                "nemo-speech": { "provider": "nemo-speech", "version": "0.3.0", "builds": {
                    "linux-x64-cpu": build("ncpu"), "linux-x64-vulkan": build("nvulkan"),
                    "macos-arm64-metal": build("nmetal") } },
                // The wind tunnel's baselines: the executor leaves them out.
                "whisper-server": { "provider": "whisper-server", "version": "b2", "builds": {
                    "linux-x64-cpu": build("wcpu") } }
            },
            "models": {
                "big": model("llm", "llama-server", 6_000_000_000, 7 * GIB),
                "small": model("llm", "llama-server", 1_300_000_000, 2 * GIB),
                "embed": model("embeddings", "llama-server", 150_000_000, GIB / 4),
                "parakeet": model("stt", "nemo-speech", 700_000_000, GIB / 2),
                "whisper": model("stt", "whisper-server", 60_000_000, GIB / 3)
            }
        })
    }

    fn recommendation(model: &str, provider: &str, evidence: bool) -> serde_json::Value {
        json!({ "provider": provider, "modelId": model, "metrics": { "tokensPerSecond": 1 },
                "evidence": if evidence { vec!["e.json"] } else { vec![] } })
    }

    fn data(recommendations: serde_json::Value) -> Data {
        let classes = json!({
            "schemaVersion": 1,
            "rules": [
                { "class": "gpu-12gb", "family": "gpu", "tierGb": 12,
                  "match": { "unifiedMemory": false, "backendAny": ["cuda", "vulkan"], "minVramGb": 11 } },
                { "class": "igpu-32", "family": "igpu", "tierGb": 32,
                  "match": { "gpuVendor": "amd", "unifiedMemory": true, "backendAny": ["rocm", "vulkan"], "minMemoryGb": 28 } },
                { "class": "cpu-16gb", "family": "cpu", "tierGb": 16, "match": { "minMemoryGb": 14 } },
                { "class": "cpu-8gb", "family": "cpu", "tierGb": 8, "match": {} }
            ]
        });
        Data::parse(
            &manifest().to_string(),
            &json!({ "schemaVersion": 1, "generatedAt": "2026-11-03T00:00:00Z", "classes": recommendations }).to_string(),
            &classes.to_string(),
        )
        .unwrap()
    }

    fn full() -> serde_json::Value {
        let mut parakeet = recommendation("parakeet", "nemo-speech", true);
        parakeet["runtimeBuild"] = json!("linux-x64-vulkan");
        parakeet["serverFlags"] = json!(["--device", "vulkan", "--no-ui"]);
        json!({
            "gpu-12gb": { "llm": recommendation("big", "llama-server", true),
                          "embeddings": recommendation("embed", "llama-server", true),
                          "stt": parakeet },
            "cpu-8gb": { "llm": recommendation("small", "llama-server", true),
                         "embeddings": recommendation("embed", "llama-server", true),
                         "stt": parakeet }
        })
    }

    fn gpu(vendor: GpuVendor, kind: GpuType, vram_gib: u64) -> GpuInfo {
        GpuInfo {
            vendor,
            kind,
            name: "g".into(),
            vram_bytes: Some(vram_gib * GIB),
            shared_bytes: None,
            driver: None,
            cuda_version: None,
            source: "test".into(),
        }
    }

    fn device(
        data: &Data,
        ram_gib: u64,
        gpus: Vec<GpuInfo>,
        backends: Vec<Backend>,
    ) -> LocalHardware {
        let mut hardware = LocalHardware {
            os: "linux".into(),
            arch: "x64".into(),
            cpu: CpuInfo {
                brand: "c".into(),
                physical_cores: Some(4),
                logical_cores: 8,
                features: vec![],
            },
            ram_bytes: ram_gib * GIB,
            available_ram_bytes: ram_gib * GIB,
            gpus,
            backends,
            gpu_budget_bytes: None,
            memory_budget_bytes: 0,
            disk_free_bytes: Some(100 * GIB),
            storage_dir: "/store".into(),
            hardware_class: String::new(),
            notes: vec![],
        };
        crate::ai_service::local::hardware::finish(&mut hardware, &data.classes);
        hardware
    }

    fn nvidia_12(data: &Data, ram_gib: u64) -> LocalHardware {
        device(
            data,
            ram_gib,
            vec![gpu(GpuVendor::Nvidia, GpuType::Discrete, 12)],
            vec![Backend::Cpu, Backend::Vulkan, Backend::Cuda],
        )
    }

    fn nothing_installed(_: &str) -> bool {
        false
    }

    #[test]
    fn a_gpu_device_gets_its_class_models_and_the_best_build_it_can_run() {
        let data = data(full());
        let hardware = nvidia_12(&data, 32);
        let plan = plan(&data, &hardware, &nothing_installed);
        assert_eq!(plan.hardware_class, "gpu-12gb");
        let ids: Vec<_> = plan.models.iter().map(|m| m.model_id.as_str()).collect();
        assert_eq!(ids, vec!["big", "embed", "parakeet"]);
        assert!(plan.models.iter().all(|m| m.basis == Basis::Measured));
        assert!(plan.fits, "{:?}", plan.reasons);
        let llama = plan
            .runtimes
            .iter()
            .find(|r| r.runtime_id == "llama-server")
            .unwrap();
        assert_eq!(llama.build, "linux-x64-cuda");
        assert_eq!(llama.services, vec![Service::Llm, Service::Embeddings]);
        let nemo = plan
            .runtimes
            .iter()
            .find(|r| r.runtime_id == "nemo-speech")
            .unwrap();
        assert_eq!(
            (nemo.build.as_str(), nemo.backend),
            ("linux-x64-vulkan", Backend::Vulkan)
        );
        assert_eq!(nemo.services, vec![Service::Stt]);
        assert_eq!(
            plan.runtimes.len(),
            2,
            "no whisper-server: {:?}",
            plan.runtimes
        );
        assert!(plan.reasons.is_empty(), "{:?}", plan.reasons);
        assert_eq!(plan.download_bytes, plan.total_bytes);
        assert_eq!(plan.memory_bytes, 7 * GIB + GIB / 4 + GIB / 2);
    }

    /// cpu-16gb has no measurements: it takes cpu-8gb's, the nearest smaller measured class.
    #[test]
    fn an_unmeasured_class_falls_back_to_the_nearest_smaller_measured_one() {
        let data = data(full());
        let hardware = device(&data, 16, vec![], vec![Backend::Cpu]);
        assert_eq!(hardware.hardware_class, "cpu-16gb");
        let plan = plan(&data, &hardware, &nothing_installed);
        assert!(plan.models.iter().all(|m| m.from_class == "cpu-8gb"));
        assert_eq!(plan.models[0].model_id, "small");
        assert_eq!(plan.runtimes[0].build, "linux-x64-cpu");
        assert!(plan.fits);
    }

    #[test]
    fn a_recommendation_without_evidence_counts_as_unmeasured() {
        let mut recommendations = full();
        recommendations["gpu-12gb"]["llm"] = recommendation("big", "llama-server", false);
        let data = data(recommendations);
        let hardware = nvidia_12(&data, 32);
        let plan = plan(&data, &hardware, &nothing_installed);
        let llm = plan
            .models
            .iter()
            .find(|m| m.service == Service::Llm)
            .unwrap();
        assert_eq!(
            (llm.model_id.as_str(), llm.from_class.as_str()),
            ("small", "cpu-8gb")
        );

        // No measurement anywhere: the service takes the manifest's smallest model, marked.
        let mut recommendations = full();
        recommendations["gpu-12gb"]
            .as_object_mut()
            .unwrap()
            .remove("stt");
        recommendations["cpu-8gb"]["stt"] = recommendation("parakeet", "nemo-speech", false);
        let data = super::tests::data(recommendations);
        let without = super::plan(&data, &hardware, &nothing_installed);
        let stt = without
            .models
            .iter()
            .find(|m| m.service == Service::Stt)
            .unwrap();
        assert_eq!(
            (stt.model_id.as_str(), stt.basis),
            ("parakeet", Basis::Default)
        );
        assert!(without.unavailable.is_empty());
    }

    /// No class measured: each service gets the manifest's smallest model (by memory, then
    /// size) on the best build this device runs, marked `default`, with no evidence.
    #[test]
    fn an_unmeasured_device_gets_the_smallest_models_marked_default() {
        let data = data(json!({}));
        let hardware = nvidia_12(&data, 32);
        let plan = plan(&data, &hardware, &nothing_installed);
        let ids: Vec<_> = plan.models.iter().map(|m| m.model_id.as_str()).collect();
        assert_eq!(ids, vec!["small", "embed", "parakeet"]);
        assert!(plan.models.iter().all(|m| m.basis == Basis::Default
            && m.evidence.is_empty()
            && m.from_class == "gpu-12gb"));
        assert!(plan.unavailable.is_empty(), "{:?}", plan.unavailable);
        assert!(plan.fits, "{:?}", plan.reasons);
        let builds: Vec<_> = plan.runtimes.iter().map(|r| r.build.as_str()).collect();
        assert_eq!(builds, vec!["linux-x64-cuda", "linux-x64-vulkan"]);
        assert_eq!(plan.models[0].server_flags, default_flags(Service::Llm));
        assert_eq!(
            plan.models[1].server_flags,
            default_flags(Service::Embeddings)
        );
    }

    /// A measured service keeps its recommendation; only the unmeasured ones default.
    #[test]
    fn a_measured_service_keeps_its_recommendation_beside_defaults() {
        let data =
            data(json!({ "gpu-12gb": { "llm": recommendation("big", "llama-server", true) } }));
        let plan = plan(&data, &nvidia_12(&data, 32), &nothing_installed);
        let basis: Vec<_> = plan
            .models
            .iter()
            .map(|m| (m.model_id.as_str(), m.basis))
            .collect();
        assert_eq!(
            basis,
            vec![
                ("big", Basis::Measured),
                ("embed", Basis::Default),
                ("parakeet", Basis::Default)
            ]
        );
    }

    /// The smallest models still have to fit: when they do not, the plan says so.
    #[test]
    fn a_default_set_that_cannot_fit_says_so() {
        let data = data(json!({}));
        let plan = plan(
            &data,
            &device(&data, 6, vec![], vec![Backend::Cpu]),
            &nothing_installed,
        );
        assert!(!plan.fits);
        assert!(plan.reasons.iter().any(|r| r.contains("need 2.8 GiB")));
    }

    /// `local_ai_device: cpu` (`local-ai install --device cpu`) on an unmeasured GPU device:
    /// the default models on the CPU builds.
    #[test]
    fn an_unmeasured_device_held_to_the_cpu_plans_the_cpu_builds() {
        let data = data(json!({}));
        let plan = plan_with_device(
            &data,
            &nvidia_12(&data, 32),
            Some(Backend::Cpu),
            &nothing_installed,
        );
        assert!(plan.fits, "{:?}", plan.reasons);
        assert_eq!(plan.models.len(), 3);
        assert!(plan.models.iter().all(|m| m.basis == Basis::Default));
        assert!(plan
            .runtimes
            .iter()
            .all(|r| r.build == "linux-x64-cpu" && r.backend == Backend::Cpu));
    }

    /// A service or a provider the executor does not run (the wind tunnel's `completion`
    /// rows, speech on speaches) is not a recommendation here.
    #[test]
    fn rows_the_executor_cannot_run_are_ignored() {
        let mut recommendations = full();
        recommendations["gpu-12gb"]["completion"] = recommendation("small", "llama-server", true);
        recommendations["gpu-12gb"]["tts"] =
            json!({ "provider": "speaches", "remoteModel": "kokoro", "evidence": ["e.json"] });
        let data = data(recommendations);
        let plan = plan(&data, &nvidia_12(&data, 32), &nothing_installed);
        assert_eq!(plan.models.len(), 3);
        assert!(plan.fits);
    }

    /// 12 GiB of VRAM but 8 GiB of RAM: the budget is 4 GiB, the 7 GiB model steps down.
    #[test]
    fn the_largest_model_steps_down_until_the_set_fits_the_budget() {
        let data = data(full());
        let hardware = nvidia_12(&data, 8);
        assert_eq!(hardware.memory_budget_bytes, 4 * GIB);
        let plan = plan(&data, &hardware, &nothing_installed);
        assert_eq!(plan.models[0].model_id, "small");
        assert!(plan.fits);
        assert!(plan.reasons[0].contains("steps down from big to small"));
    }

    #[test]
    fn a_set_that_cannot_fit_says_so_with_the_numbers() {
        let data = data(full());
        let hardware = device(&data, 6, vec![], vec![Backend::Cpu]);
        let plan = plan(&data, &hardware, &nothing_installed);
        assert!(!plan.fits);
        assert!(
            plan.reasons
                .iter()
                .any(|r| r.contains("need 2.8 GiB") && r.contains("2.0 GiB")),
            "{:?}",
            plan.reasons
        );
    }

    #[test]
    fn installed_files_leave_the_download_and_a_full_disk_does_not_fit() {
        let data = data(full());
        let mut hardware = device(&data, 16, vec![], vec![Backend::Cpu]);
        let small_sha = data.manifest.models["small"].sha256.clone();
        let plan_with = plan(&data, &hardware, &|sha| sha == small_sha);
        assert!(plan_with.models[0].installed);
        assert_eq!(
            plan_with.download_bytes,
            plan_with.total_bytes - 1_300_000_000
        );

        hardware.disk_free_bytes = Some(GIB);
        let full_disk = plan(&data, &hardware, &nothing_installed);
        assert!(!full_disk.fits);
        assert!(full_disk.reasons.iter().any(|r| r.contains("disk")));
    }

    /// The test manifest builds llama-server for Linux only and nemo-speech for macOS too: a
    /// Mac gets transcription on Metal and is told why chat and embeddings are missing.
    #[test]
    fn a_platform_without_a_build_leaves_its_services_out() {
        let data = data(full());
        let mut hardware = device(&data, 16, vec![], vec![Backend::Cpu, Backend::Metal]);
        hardware.os = "macos".into();
        hardware.arch = "arm64".into();
        let plan = plan(&data, &hardware, &nothing_installed);
        let unavailable: Vec<_> = plan.unavailable.iter().map(|u| u.service).collect();
        assert_eq!(unavailable, vec![Service::Llm, Service::Embeddings]);
        assert!(plan.unavailable[0].reason.contains("macos-arm64"));
        assert_eq!(plan.models.len(), 1);
        assert_eq!(
            (plan.runtimes[0].build.as_str(), plan.runtimes[0].backend),
            ("macos-arm64-metal", Backend::Metal)
        );
        assert!(plan.reasons[0].contains("cannot run the recommended linux-x64-vulkan"));

        hardware.backends = vec![Backend::Cpu];
        let plan = super::plan(&data, &hardware, &nothing_installed);
        assert!(plan.models.is_empty());
        assert!(!plan.fits);
        assert_eq!(plan.unavailable.len(), 3);
    }

    /// Transcription runs the recommended Vulkan build where Vulkan runs, else the CPU build,
    /// with both reasons.
    #[test]
    fn transcription_falls_back_to_the_cpu_build_without_vulkan() {
        let data = data(full());
        let hardware = device(
            &data,
            16,
            vec![gpu(GpuVendor::Nvidia, GpuType::Discrete, 4)],
            vec![Backend::Cpu, Backend::Cuda],
        );
        let plan = plan(&data, &hardware, &nothing_installed);
        let nemo = plan
            .runtimes
            .iter()
            .find(|r| r.provider == Provider::NemoSpeech)
            .unwrap();
        assert_eq!(
            (nemo.build.as_str(), nemo.backend),
            ("linux-x64-cpu", Backend::Cpu)
        );
        assert!(plan
            .reasons
            .iter()
            .any(|r| r == "nemo-speech runs linux-x64-cpu: this device cannot run the recommended linux-x64-vulkan"));
        assert!(plan
            .reasons
            .iter()
            .any(|r| r.contains("nemo-speech has no GPU build for linux-x64")));
    }

    // dev chose its device by cargo feature and detection; here detection picks the backend
    // (CUDA, Metal, Vulkan, ROCm, CPU) and `local_ai_device` overrides it.
    #[test]
    fn the_operators_device_overrides_detection() {
        let data = data(full());
        let hardware = nvidia_12(&data, 32);
        let builds = |plan: &LocalPlan| -> Vec<(String, Backend)> {
            plan.runtimes
                .iter()
                .map(|r| (r.build.clone(), r.backend))
                .collect()
        };

        // Detected: CUDA for chat, Vulkan for transcription.
        let detected = plan_with_device(&data, &hardware, None, &nothing_installed);
        assert_eq!(detected, plan(&data, &hardware, &nothing_installed));
        assert_eq!(detected.device, None);

        // Held to the CPU: CPU builds, the budget by RAM, the CPU class's models.
        let cpu = plan_with_device(&data, &hardware, Some(Backend::Cpu), &nothing_installed);
        assert_eq!(cpu.device, Some(Backend::Cpu));
        assert_eq!(
            builds(&cpu),
            vec![
                ("linux-x64-cpu".to_string(), Backend::Cpu),
                ("linux-x64-cpu".to_string(), Backend::Cpu)
            ]
        );
        assert_eq!(cpu.memory_budget_bytes, 28 * GIB);
        assert_ne!(cpu.hardware_class, detected.hardware_class);
        assert!(
            cpu.reasons[0].starts_with("local_ai_device is cpu"),
            "{:?}",
            cpu.reasons
        );

        // Vulkan on an NVIDIA GPU: Vulkan builds for both runtimes.
        let vulkan = plan_with_device(&data, &hardware, Some(Backend::Vulkan), &nothing_installed);
        assert_eq!(
            builds(&vulkan),
            vec![
                ("linux-x64-vulkan".to_string(), Backend::Vulkan),
                ("linux-x64-vulkan".to_string(), Backend::Vulkan)
            ]
        );

        // ROCm, which detection did not find: llama-server's ROCm build (not swapped for
        // Vulkan), nemo-speech, which has no ROCm build, on the CPU; both said.
        let rocm = plan_with_device(&data, &hardware, Some(Backend::Rocm), &nothing_installed);
        assert_eq!(
            builds(&rocm),
            vec![
                ("linux-x64-rocm".to_string(), Backend::Rocm),
                ("linux-x64-cpu".to_string(), Backend::Cpu)
            ]
        );
        assert!(
            rocm.reasons[0].contains("which detection did not find"),
            "{:?}",
            rocm.reasons
        );
        assert!(rocm
            .reasons
            .iter()
            .any(|r| r.contains("nemo-speech has no GPU build")));
    }

    #[test]
    fn a_mac_held_to_metal_or_the_cpu_uses_only_those_builds() {
        let data = data(full());
        let mut mac = device(&data, 16, vec![], vec![Backend::Cpu, Backend::Metal]);
        mac.os = "macos".into();
        mac.arch = "arm64".into();
        let metal = plan_with_device(&data, &mac, Some(Backend::Metal), &nothing_installed);
        assert_eq!(metal.runtimes[0].backend, Backend::Metal);
        // The test manifest has no CPU build of nemo-speech for a Mac (nor any llama-server
        // build): held to the CPU, nothing runs there.
        let cpu = plan_with_device(&data, &mac, Some(Backend::Cpu), &nothing_installed);
        assert!(cpu.runtimes.is_empty(), "{:?}", cpu.runtimes);
    }

    fn amd_igpu(data: &Data, backends: Vec<Backend>) -> LocalHardware {
        let mut igpu = gpu(GpuVendor::Amd, GpuType::Integrated, 0);
        igpu.vram_bytes = Some(GIB / 2);
        igpu.shared_bytes = Some(30 * GIB);
        device(data, 64, vec![igpu], backends)
    }

    /// The ROCm build runs on the CPU without the system ROCm libraries: where Vulkan runs,
    /// a recommended ROCm build becomes Vulkan, with the reason. Without Vulkan it stays.
    #[test]
    fn a_recommended_rocm_build_becomes_vulkan_where_vulkan_runs() {
        let mut on_rocm = recommendation("big", "llama-server", true);
        on_rocm["runtimeBuild"] = json!("linux-x64-rocm");
        let mut recommendations = full();
        recommendations["igpu-32"] = json!({ "llm": on_rocm });
        let data = data(recommendations);

        let both = amd_igpu(&data, vec![Backend::Cpu, Backend::Vulkan, Backend::Rocm]);
        assert_eq!(both.hardware_class, "igpu-32");
        let plan = plan(&data, &both, &nothing_installed);
        assert_eq!(plan.runtimes[0].build, "linux-x64-vulkan");
        assert!(plan
            .reasons
            .iter()
            .any(|r| r.contains("not the recommended linux-x64-rocm")));

        let rocm_only = amd_igpu(&data, vec![Backend::Cpu, Backend::Rocm]);
        let plan = super::plan(&data, &rocm_only, &nothing_installed);
        assert_eq!(plan.runtimes[0].build, "linux-x64-rocm");
    }

    /// The measurement's flags go with the model; without any, chat runs without thinking.
    #[test]
    fn models_carry_the_measured_server_flags_or_the_defaults() {
        let mut measured = recommendation("big", "llama-server", true);
        measured["serverFlags"] = json!(["-c", "8192", "-ngl", "999", "--reasoning", "off"]);
        let mut recommendations = full();
        recommendations["gpu-12gb"]["llm"] = measured;
        let data = data(recommendations);
        let plan = plan(&data, &nvidia_12(&data, 32), &nothing_installed);
        let flags = |service| {
            plan.models
                .iter()
                .find(|m| m.service == service)
                .unwrap()
                .server_flags
                .join(" ")
        };
        assert_eq!(flags(Service::Llm), "-c 8192 -ngl 999 --reasoning off");
        assert_eq!(flags(Service::Embeddings), "--embeddings");
        assert_eq!(flags(Service::Stt), "--device vulkan --no-ui");
        assert!(default_flags(Service::Stt).is_empty());
        assert_eq!(default_flags(Service::Llm), vec!["--reasoning", "off"]);
    }

    /// The embedded data on the smoke-test device: a Strix Halo APU, 128 GB, Vulkan. The
    /// wind tunnel measured this class: Qwen3.5 4B on Vulkan, nomic (measured on ROCm, here
    /// on the chat model's Vulkan llama-server), Parakeet v3 on nemo-speech's Vulkan build
    /// with its device flag.
    #[test]
    fn the_embedded_data_plans_the_measured_set_on_strix_halo() {
        let data = crate::ai_service::local::data::embedded().unwrap();
        let mut strix = gpu(GpuVendor::Amd, GpuType::Integrated, 0);
        strix.vram_bytes = Some(GIB / 2);
        strix.shared_bytes = Some(61 * GIB);
        strix.source = "sysfs+vulkan".into();
        let hardware = device(data, 122, vec![strix], vec![Backend::Cpu, Backend::Vulkan]);
        assert_eq!(hardware.hardware_class, "amd-igpu-uma-128");
        let plan = plan(data, &hardware, &nothing_installed);
        assert!(plan.fits, "{:?}", plan.reasons);
        let builds: Vec<_> = plan.runtimes.iter().map(|r| r.build.as_str()).collect();
        assert_eq!(builds, vec!["linux-x64-vulkan", "linux-x64-vulkan"]);
        assert_eq!(plan.runtimes[1].provider, Provider::NemoSpeech);
        let ids: Vec<_> = plan.models.iter().map(|m| m.model_id.as_str()).collect();
        assert_eq!(
            ids,
            vec![
                "qwen3.5-4b-q4_k_m",
                "nomic-embed-text-v1.5-q8_0",
                "parakeet-tdt-0.6b-v3-q8_0"
            ]
        );
        assert_eq!(
            plan.models[2].server_flags.join(" "),
            "--device vulkan --no-ui"
        );
        assert!(plan.models[0]
            .server_flags
            .join(" ")
            .contains("--reasoning off"));
        assert!(plan
            .models
            .iter()
            .all(|m| m.from_class == "amd-igpu-uma-128"));
    }

    /// Windows: nemo-speech has no Windows build, and llama-server's audio models need a
    /// projector the executor does not fetch, so transcription is unavailable rather than
    /// planned on a model that cannot take audio.
    #[test]
    fn the_embedded_data_plans_no_transcription_on_windows() {
        let data = crate::ai_service::local::data::embedded().unwrap();
        let mut hardware = device(data, 32, vec![], vec![Backend::Cpu]);
        hardware.os = "windows".into();
        crate::ai_service::local::hardware::finish(&mut hardware, &data.classes);
        let plan = plan(data, &hardware, &nothing_installed);
        assert!(
            plan.models.iter().all(|m| m.service != Service::Stt),
            "{:?}",
            plan.models
        );
        let unavailable: Vec<_> = plan.unavailable.iter().map(|u| u.service).collect();
        assert_eq!(unavailable, vec![Service::Stt]);
    }

    /// A device of a class the wind tunnel has not measured: the RTX 3060 fixture of
    /// `hardware-class-fixtures.json`. It gets the manifest's smallest chat model, nomic and
    /// Parakeet v3, marked `default`, on CUDA; held to the CPU, on the CPU builds.
    #[test]
    fn the_embedded_data_plans_defaults_on_an_unmeasured_class() {
        let data = crate::ai_service::local::data::embedded().unwrap();
        let fixtures: serde_json::Value = serde_json::from_str(include_str!(
            "../../../../ai-wind-tunnel/hardware-class-fixtures.json"
        ))
        .unwrap();
        let fixture = fixtures["fixtures"]
            .as_array()
            .unwrap()
            .iter()
            .find(|f| f["name"] == "RTX 3060 12 GB, CUDA")
            .unwrap();
        let input = &fixture["input"];
        let mut rtx = gpu(GpuVendor::Nvidia, GpuType::Discrete, 0);
        rtx.vram_bytes = input["gpus"][0]["vramBytes"].as_u64();
        let backends: Vec<Backend> = serde_json::from_value(input["backends"].clone()).unwrap();
        let mut hardware = device(data, 0, vec![rtx], backends);
        hardware.ram_bytes = input["memoryBytes"].as_u64().unwrap();
        hardware.available_ram_bytes = hardware.ram_bytes;
        crate::ai_service::local::hardware::finish(&mut hardware, &data.classes);
        assert_eq!(hardware.hardware_class, fixture["expected"]);
        assert!(data
            .recommendations
            .classes
            .get(&hardware.hardware_class)
            .is_none());

        let expected = vec![
            "qwen3.5-0.8b-q8_0",
            "nomic-embed-text-v1.5-q8_0",
            "parakeet-tdt-0.6b-v3-q8_0",
        ];
        let plan = plan(data, &hardware, &nothing_installed);
        assert!(plan.fits, "{:?}", plan.reasons);
        let ids: Vec<_> = plan.models.iter().map(|m| m.model_id.as_str()).collect();
        assert_eq!(ids, expected);
        assert!(plan.models.iter().all(|m| m.basis == Basis::Default));
        let builds: Vec<_> = plan.runtimes.iter().map(|r| r.build.as_str()).collect();
        assert_eq!(builds, vec!["linux-x64-cuda", "linux-x64-cuda"]);

        let cpu = plan_with_device(data, &hardware, Some(Backend::Cpu), &nothing_installed);
        assert!(cpu.fits, "{:?}", cpu.reasons);
        let ids: Vec<_> = cpu.models.iter().map(|m| m.model_id.as_str()).collect();
        assert_eq!(ids, expected);
        let builds: Vec<_> = cpu.runtimes.iter().map(|r| r.build.as_str()).collect();
        assert_eq!(builds, vec!["linux-x64-cpu", "linux-x64-cpu"]);
    }
}
