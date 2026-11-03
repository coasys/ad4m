//! The install plan (`ai.localPlan`): from this device, the wind tunnel's recommendations
//! and the manifest, which runtime builds and models to fetch, how much that downloads, and
//! whether it fits. Pure: no I/O, so every rule has a unit test.
//!
//! Rules:
//! - The recommendation of a service comes from the device's class, or the nearest smaller
//!   measured class (`hardware-classes.json`'s `smaller` chain). A recommendation without
//!   evidence, or naming a model the manifest lacks, counts as not measured.
//! - The models of all services run at once, so their memory adds up. It fits when the sum
//!   is at most the budget, min(GPU budget − 1 GiB, RAM − 4 GiB). When it does not, the
//!   service with the largest model steps down its chain, until it fits or nothing can step.
//! - The runtime build of a provider is the recommendation's build when this device can run
//!   it, else the best backend this device has that the manifest builds (CUDA 13, CUDA 12,
//!   Metal, Vulkan, CPU).
//! - The download must fit the free disk space with 1 GiB to spare.

use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use ts_rs::TS;

use super::data::{Data, Provider, Recommendation, Service};
use super::hardware::{Backend, LocalHardware, GIB};

/// Backends best first.
const BACKEND_PREFERENCE: [Backend; 5] = [
    Backend::Cuda13,
    Backend::Cuda12,
    Backend::Metal,
    Backend::Vulkan,
    Backend::Cpu,
];

/// The services a plan offers; TTS has no runtime yet.
pub const PLANNED_SERVICES: [Service; 3] = [Service::Llm, Service::Embeddings, Service::Stt];

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiPlannedRuntime")]
pub struct PlannedRuntime {
    /// The manifest's runtime id: `llama.cpp`, `whisper.cpp`.
    pub runtime_id: String,
    pub provider: Provider,
    pub version: String,
    /// The manifest's build key: `linux-x86_64-vulkan`.
    pub build: String,
    pub backend: Backend,
    /// The archive and its extras.
    #[ts(type = "number")]
    pub size_bytes: u64,
    pub installed: bool,
    pub services: Vec<Service>,
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
    /// The measured class the recommendation comes from.
    pub from_class: String,
    pub evidence: Vec<String>,
    #[ts(type = "Record<string, unknown>")]
    pub metrics: serde_json::Value,
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
}

/// One service's candidates: the recommendation of each measured class along the chain,
/// largest first, without repeating a model.
fn candidates(data: &Data, chain: &[String], service: Service) -> Vec<(String, Recommendation)> {
    let mut seen = BTreeSet::new();
    chain
        .iter()
        .filter_map(|class| {
            let recommendation = data.recommendations.classes.get(class)?.get(&service)?;
            let model_id = recommendation.model_id.as_ref()?;
            let measured = !recommendation.evidence.is_empty();
            let known = data
                .manifest
                .models
                .get(model_id)
                .is_some_and(|m| m.service == service && m.provider == recommendation.provider);
            (measured && known && seen.insert(model_id.clone()))
                .then(|| (class.clone(), recommendation.clone()))
        })
        .collect()
}

/// The build of `provider`'s runtime for this device, and the backend it uses.
fn runtime_build<'a>(
    data: &'a Data,
    hardware: &LocalHardware,
    provider: Provider,
    preferred: Option<&str>,
) -> Option<(&'a String, &'a super::data::Runtime, String, Backend)> {
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
            return Some((runtime_id, runtime, key.to_string(), backend));
        }
    }
    BACKEND_PREFERENCE.iter().find_map(|backend| {
        let key = format!("{platform}-{}", backend.as_str());
        usable(&key).map(|backend| (runtime_id, runtime, key, backend))
    })
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
        let found = candidates(data, &chain, service);
        if found.is_empty() {
            unavailable.push(Unavailable {
                service,
                reason: format!(
                    "No measured recommendation for {} on {} or a smaller class",
                    service.as_str(),
                    hardware.hardware_class
                ),
            });
            continue;
        }
        let provider = found[0].1.provider;
        if runtime_build(
            data,
            hardware,
            provider,
            found[0].1.runtime_build.as_deref(),
        )
        .is_none()
        {
            unavailable.push(Unavailable {
                service,
                reason: format!(
                    "No {} build for {} in the manifest",
                    provider.as_str(),
                    hardware.platform()
                ),
            });
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
            from_class: from_class.clone(),
            evidence: recommendation.evidence.clone(),
            metrics: recommendation.metrics.clone(),
        });
        if let Some(existing) = runtimes.iter_mut().find(|r| r.provider == file.provider) {
            existing.services.push(*service);
            continue;
        }
        let Some((runtime_id, runtime, key, backend)) = runtime_build(
            data,
            hardware,
            file.provider,
            recommendation.runtime_build.as_deref(),
        ) else {
            continue;
        };
        let build = &runtime.builds[&key];
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
                "llama.cpp": { "provider": "llama-server", "version": "b1", "builds": {
                    "linux-x86_64-cpu": build("lcpu"), "linux-x86_64-vulkan": build("lvulkan"),
                    "linux-x86_64-cuda13": build("lcuda13") } },
                "whisper.cpp": { "provider": "whisper-server", "version": "b2", "builds": {
                    "linux-x86_64-cpu": build("wcpu") } }
            },
            "models": {
                "big": model("llm", "llama-server", 6_000_000_000, 7 * GIB),
                "small": model("llm", "llama-server", 1_300_000_000, 2 * GIB),
                "embed": model("embeddings", "llama-server", 150_000_000, GIB / 4),
                "whisper": model("stt", "whisper-server", 60_000_000, GIB / 2)
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
                { "class": "gpu-12gb", "when": { "gpuType": ["discrete"], "minVramGiB": 11 } },
                { "class": "cpu-16gb", "when": { "minRamGiB": 14 } },
                { "class": "cpu-8gb", "when": {} }
            ],
            "smaller": { "gpu-12gb": "cpu-16gb", "cpu-16gb": "cpu-8gb", "cpu-8gb": null }
        });
        Data::parse(
            &manifest().to_string(),
            &json!({ "schemaVersion": 1, "generatedAt": "2026-11-03T00:00:00Z", "classes": recommendations }).to_string(),
            &classes.to_string(),
        )
        .unwrap()
    }

    fn full() -> serde_json::Value {
        json!({
            "gpu-12gb": { "llm": recommendation("big", "llama-server", true),
                          "embeddings": recommendation("embed", "llama-server", true),
                          "stt": recommendation("whisper", "whisper-server", true) },
            "cpu-8gb": { "llm": recommendation("small", "llama-server", true),
                         "embeddings": recommendation("embed", "llama-server", true),
                         "stt": recommendation("whisper", "whisper-server", true) }
        })
    }

    fn device(
        data: &Data,
        ram_gib: u64,
        gpu_gib: Option<u64>,
        backends: Vec<Backend>,
    ) -> LocalHardware {
        let mut hardware = LocalHardware {
            os: "linux".into(),
            arch: "x86_64".into(),
            cpu: CpuInfo {
                brand: "c".into(),
                physical_cores: Some(4),
                logical_cores: 8,
                features: vec![],
            },
            ram_bytes: ram_gib * GIB,
            available_ram_bytes: ram_gib * GIB,
            gpus: gpu_gib
                .map(|g| GpuInfo {
                    vendor: GpuVendor::Nvidia,
                    kind: GpuType::Discrete,
                    name: "card".into(),
                    vram_bytes: Some(g * GIB),
                    shared_bytes: None,
                    driver: None,
                    cuda_version: Some("13.0".into()),
                    source: "nvml".into(),
                })
                .into_iter()
                .collect(),
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

    fn nothing_installed(_: &str) -> bool {
        false
    }

    #[test]
    fn a_gpu_device_gets_its_class_models_and_the_best_build_it_can_run() {
        let data = data(full());
        let hardware = device(
            &data,
            32,
            Some(12),
            vec![
                Backend::Cpu,
                Backend::Vulkan,
                Backend::Cuda12,
                Backend::Cuda13,
            ],
        );
        let plan = plan(&data, &hardware, &nothing_installed);
        assert_eq!(plan.hardware_class, "gpu-12gb");
        let ids: Vec<_> = plan.models.iter().map(|m| m.model_id.as_str()).collect();
        assert_eq!(ids, vec!["big", "embed", "whisper"]);
        assert!(plan.fits, "{:?}", plan.reasons);
        let llama = plan
            .runtimes
            .iter()
            .find(|r| r.runtime_id == "llama.cpp")
            .unwrap();
        assert_eq!(llama.build, "linux-x86_64-cuda13");
        assert_eq!(llama.services, vec![Service::Llm, Service::Embeddings]);
        let whisper = plan
            .runtimes
            .iter()
            .find(|r| r.runtime_id == "whisper.cpp")
            .unwrap();
        assert_eq!(whisper.backend, Backend::Cpu);
        assert!(plan
            .reasons
            .iter()
            .any(|r| r.contains("whisper.cpp has no GPU build")));
        assert_eq!(plan.download_bytes, plan.total_bytes);
        assert_eq!(plan.memory_bytes, 7 * GIB + GIB / 4 + GIB / 2);
    }

    /// cpu-16gb has no measurements: it takes cpu-8gb's, the nearest smaller measured class.
    #[test]
    fn an_unmeasured_class_falls_back_to_the_nearest_smaller_measured_one() {
        let data = data(full());
        let hardware = device(&data, 16, None, vec![Backend::Cpu]);
        assert_eq!(hardware.hardware_class, "cpu-16gb");
        let plan = plan(&data, &hardware, &nothing_installed);
        assert!(plan.models.iter().all(|m| m.from_class == "cpu-8gb"));
        assert_eq!(plan.models[0].model_id, "small");
        assert_eq!(plan.runtimes[0].build, "linux-x86_64-cpu");
        assert!(plan.fits);
    }

    #[test]
    fn a_recommendation_without_evidence_counts_as_unmeasured() {
        let mut recommendations = full();
        recommendations["gpu-12gb"]["llm"] = recommendation("big", "llama-server", false);
        let data = data(recommendations);
        let hardware = device(&data, 32, Some(12), vec![Backend::Cpu, Backend::Cuda13]);
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

        // No measurement anywhere: the service is left out, with the reason.
        let mut recommendations = full();
        recommendations["gpu-12gb"]
            .as_object_mut()
            .unwrap()
            .remove("stt");
        recommendations["cpu-8gb"]["stt"] = recommendation("whisper", "whisper-server", false);
        let data = super::tests::data(recommendations);
        let without = super::plan(&data, &hardware, &nothing_installed);
        assert!(without.models.iter().all(|m| m.service != Service::Stt));
        assert_eq!(without.unavailable[0].service, Service::Stt);
    }

    /// 12 GiB of VRAM but 8 GiB of RAM: the budget is 4 GiB, the 7 GiB model steps down.
    #[test]
    fn the_largest_model_steps_down_until_the_set_fits_the_budget() {
        let data = data(full());
        let hardware = device(&data, 8, Some(12), vec![Backend::Cpu, Backend::Cuda13]);
        assert_eq!(hardware.memory_budget_bytes, 4 * GIB);
        let plan = plan(&data, &hardware, &nothing_installed);
        assert_eq!(plan.models[0].model_id, "small");
        assert!(plan.fits);
        assert!(plan.reasons[0].contains("steps down from big to small"));
    }

    #[test]
    fn a_set_that_cannot_fit_says_so_with_the_numbers() {
        let data = data(full());
        let hardware = device(&data, 6, None, vec![Backend::Cpu]);
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
        let mut hardware = device(&data, 16, None, vec![Backend::Cpu]);
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

    #[test]
    fn a_platform_without_a_build_leaves_its_services_out() {
        let data = data(full());
        let mut hardware = device(&data, 16, None, vec![Backend::Cpu]);
        hardware.os = "macos".into();
        hardware.arch = "aarch64".into();
        let plan = plan(&data, &hardware, &nothing_installed);
        assert!(plan.models.is_empty());
        assert!(!plan.fits);
        assert_eq!(plan.unavailable.len(), 3);
        assert!(plan.unavailable[0].reason.contains("macos-aarch64"));
    }

    /// The embedded data on the smoke-test device: a Strix Halo APU, 128 GB, Vulkan.
    #[test]
    fn the_embedded_data_plans_vulkan_llama_and_cpu_whisper_on_strix_halo() {
        let data = crate::ai_service::local::data::embedded().unwrap();
        let mut hardware = device(data, 122, None, vec![Backend::Cpu, Backend::Vulkan]);
        hardware.gpus = vec![GpuInfo {
            vendor: GpuVendor::Amd,
            kind: GpuType::Integrated,
            name: "Radeon 8060S".into(),
            vram_bytes: Some(GIB / 2),
            shared_bytes: Some(61 * GIB),
            driver: None,
            cuda_version: None,
            source: "sysfs+vulkan".into(),
        }];
        crate::ai_service::local::hardware::finish(&mut hardware, &data.classes);
        assert_eq!(hardware.hardware_class, "igpu-32gb");
        let plan = plan(data, &hardware, &nothing_installed);
        assert!(plan.fits, "{:?}", plan.reasons);
        let builds: Vec<_> = plan.runtimes.iter().map(|r| r.build.as_str()).collect();
        assert_eq!(builds, vec!["linux-x86_64-vulkan", "linux-x86_64-cpu"]);
        assert_eq!(plan.models.len(), 3);
    }
}
