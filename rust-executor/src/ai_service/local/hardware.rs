//! What this device can run: OS, CPU, memory, GPUs, the backends a runtime build can use,
//! and the memory budget the plan's fit rule reads (`ai.localHardware`).
//!
//! Sources, none needing a tool or root:
//! - `sysinfo`: RAM, CPU brand and cores, free disk space.
//! - std: x86 and ARM CPU features.
//! - NVIDIA: NVML (`libnvidia-ml.so.1` / `nvml.dll`), opened at run time when the driver
//!   ships it: names, VRAM, driver and the CUDA version it supports.
//! - Linux and Windows: the Vulkan loader (`libvulkan.so.1` / `vulkan-1.dll`), opened at run
//!   time: every GPU a Vulkan build of llama.cpp can use, whether integrated or discrete,
//!   and its device-local memory. No `wgpu`.
//! - Linux AMD: amdgpu's sysfs `mem_info_vram_total` / `mem_info_gtt_total`.
//! - macOS: Apple Silicon by architecture; the GPU working set from `iogpu.wired_limit_mb`,
//!   else estimated as Metal sizes it (2/3 of RAM up to 36 GiB, 3/4 above).
//!
//! Detection of GPUs runs once per process (it loads drivers); memory is read per call.

use serde::{Deserialize, Serialize};
use std::path::Path;
use std::sync::OnceLock;
use ts_rs::TS;

use super::data::{classify, ClassGpu, ClassInput, HardwareClasses};

pub const GIB: u64 = 1024 * 1024 * 1024;

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, TS)]
#[serde(rename_all = "lowercase")]
#[ts(rename = "LocalAiBackend")]
pub enum Backend {
    Cpu,
    Metal,
    Vulkan,
    /// NVIDIA, with a driver for CUDA 12 or later.
    Cuda,
    /// AMD, with the system ROCm (HIP) libraries: llama.cpp's ROCm build does not ship them
    /// and runs on the CPU without them.
    Rocm,
}

impl Backend {
    /// The suffix of a manifest build key (`linux-x64-<backend>`), and the name the class
    /// rules use (`backendAny`).
    pub fn as_str(self) -> &'static str {
        match self {
            Backend::Cpu => "cpu",
            Backend::Metal => "metal",
            Backend::Vulkan => "vulkan",
            Backend::Cuda => "cuda",
            Backend::Rocm => "rocm",
        }
    }

    pub fn from_key_suffix(suffix: &str) -> Option<Backend> {
        [
            Backend::Cpu,
            Backend::Metal,
            Backend::Vulkan,
            Backend::Cuda,
            Backend::Rocm,
        ]
        .into_iter()
        .find(|b| b.as_str() == suffix)
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, TS)]
#[serde(rename_all = "lowercase")]
#[ts(rename = "LocalAiGpuVendor")]
pub enum GpuVendor {
    Nvidia,
    Amd,
    Intel,
    Apple,
    Other,
}

impl GpuVendor {
    fn from_pci(id: u32) -> GpuVendor {
        match id {
            0x10de => GpuVendor::Nvidia,
            0x1002 | 0x1022 => GpuVendor::Amd,
            0x8086 => GpuVendor::Intel,
            0x106b => GpuVendor::Apple,
            _ => GpuVendor::Other,
        }
    }

    pub fn as_str(self) -> &'static str {
        match self {
            GpuVendor::Nvidia => "nvidia",
            GpuVendor::Amd => "amd",
            GpuVendor::Intel => "intel",
            GpuVendor::Apple => "apple",
            GpuVendor::Other => "other",
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, Copy, PartialEq, Eq, TS)]
#[serde(rename_all = "lowercase")]
#[ts(rename = "LocalAiGpuType")]
pub enum GpuType {
    /// Its own VRAM.
    Discrete,
    /// Shares system RAM (an AMD APU, Intel graphics).
    Integrated,
    /// Apple Silicon: one memory for CPU and GPU.
    Unified,
}

impl GpuType {
    pub fn as_str(self) -> &'static str {
        match self {
            GpuType::Discrete => "discrete",
            GpuType::Integrated => "integrated",
            GpuType::Unified => "unified",
        }
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiGpu")]
pub struct GpuInfo {
    pub vendor: GpuVendor,
    #[serde(rename = "type")]
    pub kind: GpuType,
    pub name: String,
    /// Dedicated memory (an integrated GPU's carve-out).
    #[ts(type = "number | null")]
    pub vram_bytes: Option<u64>,
    /// System memory the GPU may map besides (amdgpu's GTT), or Vulkan's device-local heap
    /// of an integrated GPU.
    #[ts(type = "number | null")]
    pub shared_bytes: Option<u64>,
    pub driver: Option<String>,
    /// The highest CUDA version the NVIDIA driver supports, e.g. `13.0`.
    pub cuda_version: Option<String>,
    /// `nvml`, `vulkan`, `sysfs` or `sysctl`.
    pub source: String,
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(rename = "LocalAiCpu")]
pub struct CpuInfo {
    pub brand: String,
    pub physical_cores: Option<u32>,
    pub logical_cores: u32,
    /// avx, avx2, fma, f16c, avx512f; neon, dotprod, sve, i8mm.
    pub features: Vec<String>,
}

/// This device, as `ai.localHardware` answers.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, TS)]
#[serde(rename_all = "camelCase")]
#[ts(export, rename = "LocalAiHardware")]
pub struct LocalHardware {
    /// `linux`, `macos` or `windows`.
    pub os: String,
    /// `x64` or `arm64` (the names the manifest's build keys use).
    pub arch: String,
    pub cpu: CpuInfo,
    #[ts(type = "number")]
    pub ram_bytes: u64,
    #[ts(type = "number")]
    pub available_ram_bytes: u64,
    pub gpus: Vec<GpuInfo>,
    /// The backends a runtime build may use here, in no order.
    pub backends: Vec<Backend>,
    /// Memory the GPU backend may use: VRAM, an integrated GPU's VRAM and GTT, Apple's
    /// working set. Unset without a usable GPU.
    #[ts(type = "number | null")]
    pub gpu_budget_bytes: Option<u64>,
    /// What the models may take together: min(GPU budget − 1 GiB, RAM − 4 GiB), or
    /// RAM − 4 GiB without a GPU budget.
    #[ts(type = "number")]
    pub memory_budget_bytes: u64,
    /// Free space where local AI stores its files.
    #[ts(type = "number | null")]
    pub disk_free_bytes: Option<u64>,
    /// The per-user directory of runtimes and models, shared by every agent.
    pub storage_dir: String,
    /// The class `ai-wind-tunnel/hardware-classes.json` gives this device.
    pub hardware_class: String,
    /// What detection could not read, and estimates.
    pub notes: Vec<String>,
}

impl LocalHardware {
    /// The GPU a GPU backend would use: among the GPUs of the vendor the preferred GPU
    /// backend runs on (CUDA: NVIDIA, ROCm: AMD, Metal: Apple; Vulkan: any), the discrete
    /// one with the most VRAM, else the first integrated or unified one. With GPUs of two
    /// vendors, the budget is thus the GPU's the plan's build runs on.
    pub fn primary_gpu(&self) -> Option<&GpuInfo> {
        let vendor = super::plan::BACKEND_PREFERENCE
            .iter()
            .find(|b| **b != Backend::Cpu && self.backends.contains(b))
            .and_then(|backend| match backend {
                Backend::Cuda => Some(GpuVendor::Nvidia),
                Backend::Rocm => Some(GpuVendor::Amd),
                Backend::Metal => Some(GpuVendor::Apple),
                Backend::Vulkan | Backend::Cpu => None,
            })
            .filter(|v| self.gpus.iter().any(|g| g.vendor == *v));
        let mut gpus = self
            .gpus
            .iter()
            .filter(|g| vendor.is_none_or(|v| g.vendor == v));
        gpus.clone()
            .filter(|g| g.kind == GpuType::Discrete)
            .max_by_key(|g| g.vram_bytes.unwrap_or(0))
            .or_else(|| gpus.next())
    }

    /// What the class rules read.
    pub fn class_input(&self) -> ClassInput {
        ClassInput {
            os: self.os.clone(),
            arch: self.arch.clone(),
            memory_bytes: self.ram_bytes,
            backends: self
                .backends
                .iter()
                .map(|b| b.as_str().to_string())
                .collect(),
            gpus: self
                .gpus
                .iter()
                .map(|gpu| ClassGpu {
                    vendor: gpu.vendor.as_str().to_string(),
                    vram_bytes: gpu.vram_bytes.unwrap_or(0),
                    unified: gpu.kind != GpuType::Discrete,
                })
                .collect(),
        }
    }

    /// The build key prefix of this device: `linux-x64`.
    pub fn platform(&self) -> String {
        format!("{}-{}", self.os, self.arch)
    }
}

/// The GPU budget of `gpu`: VRAM for a discrete GPU, VRAM plus GTT (or the Vulkan heap)
/// for an integrated one, the working set for Apple's.
pub fn gpu_budget(gpu: &GpuInfo) -> Option<u64> {
    match gpu.kind {
        GpuType::Discrete => gpu.vram_bytes,
        GpuType::Integrated => match (gpu.vram_bytes, gpu.shared_bytes) {
            (Some(vram), Some(shared)) if gpu.source == "sysfs+vulkan" || gpu.source == "sysfs" => {
                Some(vram + shared)
            }
            (_, Some(shared)) => Some(shared),
            (vram, None) => vram,
        },
        GpuType::Unified => gpu.shared_bytes,
    }
}

/// The fit rule's budget: min(GPU budget − 1 GiB, RAM − 4 GiB), or RAM − 4 GiB.
pub fn memory_budget(ram_bytes: u64, gpu_budget_bytes: Option<u64>) -> u64 {
    let ram = ram_bytes.saturating_sub(4 * GIB);
    match gpu_budget_bytes {
        Some(gpu) => ram.min(gpu.saturating_sub(GIB)),
        None => ram,
    }
}

/// What the GPU probes found, once per process.
#[derive(Clone, Debug, Default)]
struct GpuProbe {
    gpus: Vec<GpuInfo>,
    backends: Vec<Backend>,
    notes: Vec<String>,
}

static GPU_PROBE: OnceLock<GpuProbe> = OnceLock::new();

/// Detect this device. Blocking (it may load GPU drivers the first time): call it from
/// `spawn_blocking`.
pub fn detect(storage_dir: &Path, classes: &HardwareClasses) -> LocalHardware {
    use sysinfo::{CpuRefreshKind, MemoryRefreshKind, RefreshKind, System};

    let system = System::new_with_specifics(
        RefreshKind::nothing()
            .with_memory(MemoryRefreshKind::nothing().with_ram())
            .with_cpu(CpuRefreshKind::nothing()),
    );
    let ram_bytes = system.total_memory();
    let cpus = system.cpus();
    let cpu = CpuInfo {
        brand: cpus
            .first()
            .map(|c| c.brand().trim().to_string())
            .unwrap_or_default(),
        physical_cores: System::physical_core_count().map(|n| n as u32),
        logical_cores: cpus.len().max(1) as u32,
        features: cpu_features(),
    };

    let probe = GPU_PROBE
        .get_or_init(|| probe_gpus(ram_bytes, &cpu.brand))
        .clone();
    let mut hardware = LocalHardware {
        os: std::env::consts::OS.to_string(),
        arch: arch_name(std::env::consts::ARCH).to_string(),
        cpu,
        ram_bytes,
        available_ram_bytes: system.available_memory(),
        gpus: probe.gpus,
        backends: probe.backends,
        gpu_budget_bytes: None,
        memory_budget_bytes: 0,
        disk_free_bytes: disk_free(storage_dir),
        storage_dir: storage_dir.to_string_lossy().into_owned(),
        hardware_class: String::new(),
        notes: probe.notes,
    };
    finish(&mut hardware, classes);
    hardware
}

/// Rust's architecture name as the manifest writes it: `x64`, `arm64`.
pub fn arch_name(rust_arch: &str) -> &str {
    match rust_arch {
        "x86_64" => "x64",
        "aarch64" => "arm64",
        other => other,
    }
}

/// Whether the system ROCm (HIP) runtime is installed: `ROCM_PATH`, else `/opt/rocm`.
fn rocm_installed() -> bool {
    if !cfg!(target_os = "linux") {
        return false;
    }
    let root = std::env::var("ROCM_PATH")
        .ok()
        .filter(|p| !p.is_empty())
        .unwrap_or_else(|| "/opt/rocm".to_string());
    let lib = Path::new(&root).join("lib");
    std::fs::read_dir(lib).is_ok_and(|entries| {
        entries.flatten().any(|e| {
            e.file_name()
                .to_string_lossy()
                .starts_with("libamdhip64.so")
        })
    })
}

/// Fill in what follows from the rest: the GPU budget (only with a GPU backend to use it),
/// the memory budget and the class.
pub fn finish(hardware: &mut LocalHardware, classes: &HardwareClasses) {
    let gpu_backend = hardware.backends.iter().any(|b| *b != Backend::Cpu);
    hardware.gpu_budget_bytes = if gpu_backend {
        hardware.primary_gpu().and_then(gpu_budget)
    } else {
        None
    };
    hardware.memory_budget_bytes = memory_budget(hardware.ram_bytes, hardware.gpu_budget_bytes);
    hardware.hardware_class = classify(classes, &hardware.class_input());
}

fn cpu_features() -> Vec<String> {
    let mut features = Vec::new();
    #[cfg(target_arch = "x86_64")]
    {
        for (name, has) in [
            ("avx", std::arch::is_x86_feature_detected!("avx")),
            ("avx2", std::arch::is_x86_feature_detected!("avx2")),
            ("fma", std::arch::is_x86_feature_detected!("fma")),
            ("f16c", std::arch::is_x86_feature_detected!("f16c")),
            ("avx512f", std::arch::is_x86_feature_detected!("avx512f")),
        ] {
            if has {
                features.push(name.to_string());
            }
        }
    }
    #[cfg(target_arch = "aarch64")]
    {
        for (name, has) in [
            ("neon", std::arch::is_aarch64_feature_detected!("neon")),
            (
                "dotprod",
                std::arch::is_aarch64_feature_detected!("dotprod"),
            ),
            ("sve", std::arch::is_aarch64_feature_detected!("sve")),
            ("i8mm", std::arch::is_aarch64_feature_detected!("i8mm")),
        ] {
            if has {
                features.push(name.to_string());
            }
        }
    }
    features
}

/// Free space on the disk that holds `dir` (or its nearest existing ancestor).
fn disk_free(dir: &Path) -> Option<u64> {
    let mut existing = dir.to_path_buf();
    while !existing.exists() {
        existing = existing.parent()?.to_path_buf();
    }
    let existing = existing.canonicalize().ok()?;
    let disks = sysinfo::Disks::new_with_refreshed_list();
    disks
        .list()
        .iter()
        .filter(|disk| existing.starts_with(disk.mount_point()))
        .max_by_key(|disk| disk.mount_point().as_os_str().len())
        .map(|disk| disk.available_space())
}

fn probe_gpus(ram_bytes: u64, cpu_brand: &str) -> GpuProbe {
    let mut probe = GpuProbe {
        backends: vec![Backend::Cpu],
        ..Default::default()
    };
    if cfg!(target_os = "macos") {
        if cfg!(target_arch = "aarch64") {
            let (working_set, estimated) = apple_working_set(ram_bytes);
            probe.gpus.push(GpuInfo {
                vendor: GpuVendor::Apple,
                kind: GpuType::Unified,
                name: cpu_brand.to_string(),
                vram_bytes: None,
                shared_bytes: Some(working_set),
                driver: None,
                cuda_version: None,
                source: "sysctl".to_string(),
            });
            probe.backends.push(Backend::Metal);
            if estimated {
                probe.notes.push(
                    "The GPU working set is estimated (2/3 of RAM up to 36 GiB, 3/4 above), as Metal sizes it".into(),
                );
            }
        } else {
            probe
                .notes
                .push("GPUs of Intel Macs are not detected: the CPU build runs".into());
        }
        return probe;
    }

    let nvml = nvml::probe();
    if let Some(nvml) = &nvml {
        for device in &nvml.devices {
            probe.gpus.push(GpuInfo {
                vendor: GpuVendor::Nvidia,
                kind: GpuType::Discrete,
                name: device.name.clone(),
                vram_bytes: Some(device.total_bytes),
                shared_bytes: None,
                driver: nvml.driver.clone(),
                cuda_version: nvml
                    .cuda_version
                    .map(|v| format!("{}.{}", v / 1000, (v % 1000) / 10)),
                source: "nvml".to_string(),
            });
        }
        match nvml.cuda_version {
            Some(v) if v >= 12000 => probe.backends.push(Backend::Cuda),
            _ => probe
                .notes
                .push("The NVIDIA driver supports no CUDA 12 or 13: Vulkan or the CPU runs".into()),
        }
    }

    let sysfs = sysfs_gpus();
    match vulkan::probe() {
        Ok(devices) => {
            let usable: Vec<_> = devices
                .into_iter()
                .filter(|d| {
                    matches!(
                        d.kind,
                        vulkan::DeviceKind::Integrated | vulkan::DeviceKind::Discrete
                    )
                })
                .collect();
            if !usable.is_empty() {
                probe.backends.push(Backend::Vulkan);
            }
            for device in usable {
                let vendor = GpuVendor::from_pci(device.vendor_id);
                if vendor == GpuVendor::Nvidia && nvml.is_some() {
                    continue;
                }
                let on_sysfs = sysfs
                    .iter()
                    .find(|s| s.vendor_id == device.vendor_id && s.device_id == device.device_id);
                let (kind, vram, shared, source) = match device.kind {
                    vulkan::DeviceKind::Discrete => (
                        GpuType::Discrete,
                        on_sysfs
                            .and_then(|s| s.vram)
                            .or(Some(device.device_local_bytes)),
                        None,
                        "vulkan",
                    ),
                    _ => match on_sysfs {
                        Some(s) if s.gtt.is_some() => {
                            (GpuType::Integrated, s.vram, s.gtt, "sysfs+vulkan")
                        }
                        _ => (
                            GpuType::Integrated,
                            None,
                            Some(device.device_local_bytes),
                            "vulkan",
                        ),
                    },
                };
                probe.gpus.push(GpuInfo {
                    vendor,
                    kind,
                    name: device.name,
                    vram_bytes: vram,
                    shared_bytes: shared,
                    driver: None,
                    cuda_version: None,
                    source: source.to_string(),
                });
            }
        }
        Err(reason) => {
            probe.notes.push(format!(
                "No Vulkan ({reason}): GPUs without CUDA run on the CPU build"
            ));
            for s in sysfs {
                let vendor = GpuVendor::from_pci(s.vendor_id);
                if vendor == GpuVendor::Nvidia {
                    continue;
                }
                // Guess without Vulkan: a small carve-out is an integrated GPU.
                let integrated = s.vram.is_none_or(|vram| vram <= 2 * GIB);
                probe.gpus.push(GpuInfo {
                    vendor,
                    kind: if integrated {
                        GpuType::Integrated
                    } else {
                        GpuType::Discrete
                    },
                    name: format!("PCI {:04x}:{:04x}", s.vendor_id, s.device_id),
                    vram_bytes: s.vram,
                    shared_bytes: s.gtt,
                    driver: None,
                    cuda_version: None,
                    source: "sysfs".to_string(),
                });
            }
        }
    }
    if probe.gpus.iter().any(|g| g.vendor == GpuVendor::Amd) && rocm_installed() {
        probe.backends.push(Backend::Rocm);
    }
    if cfg!(target_os = "windows") {
        probe.notes.push(
            "Windows GPU memory comes from Vulkan's device-local heap (no DXGI query yet)".into(),
        );
    }
    probe
}

/// Apple's GPU working set: `iogpu.wired_limit_mb` when the user set it, else Metal's
/// default. Answers whether it is the estimate.
fn apple_working_set(ram_bytes: u64) -> (u64, bool) {
    #[cfg(target_os = "macos")]
    {
        if let Some(mb) = sysctl_u64("iogpu.wired_limit_mb").filter(|mb| *mb > 0) {
            return (mb * 1024 * 1024, false);
        }
    }
    let estimate = if ram_bytes <= 36 * GIB {
        ram_bytes / 3 * 2
    } else {
        ram_bytes / 4 * 3
    };
    (estimate, true)
}

#[cfg(target_os = "macos")]
fn sysctl_u64(name: &str) -> Option<u64> {
    let name = std::ffi::CString::new(name).ok()?;
    let mut value: u64 = 0;
    let mut size = std::mem::size_of::<u64>();
    // SAFETY: `value` is a u64 and `size` its size; sysctlbyname writes at most `size` bytes.
    let rc = unsafe {
        libc::sysctlbyname(
            name.as_ptr(),
            &mut value as *mut u64 as *mut libc::c_void,
            &mut size,
            std::ptr::null_mut(),
            0,
        )
    };
    match (rc, size) {
        (0, 8) => Some(value),
        // A 32-bit value: the low half on little-endian.
        (0, 4) => Some(value & 0xffff_ffff),
        _ => None,
    }
}

/// A DRM card's PCI ids and amdgpu memory figures.
#[derive(Debug, Clone, PartialEq)]
struct SysfsGpu {
    vendor_id: u32,
    device_id: u32,
    vram: Option<u64>,
    gtt: Option<u64>,
}

fn sysfs_gpus() -> Vec<SysfsGpu> {
    if !cfg!(target_os = "linux") {
        return Vec::new();
    }
    sysfs_gpus_in(Path::new("/sys/class/drm"))
}

fn sysfs_gpus_in(drm: &Path) -> Vec<SysfsGpu> {
    let Ok(entries) = std::fs::read_dir(drm) else {
        return Vec::new();
    };
    let read = |path: std::path::PathBuf| std::fs::read_to_string(path).ok();
    let hex = |text: Option<String>| {
        text.and_then(|t| u32::from_str_radix(t.trim().trim_start_matches("0x"), 16).ok())
    };
    let number = |text: Option<String>| text.and_then(|t| t.trim().parse::<u64>().ok());
    let mut cards: Vec<_> = entries
        .flatten()
        .filter(|e| {
            let name = e.file_name().to_string_lossy().into_owned();
            name.strip_prefix("card")
                .is_some_and(|n| !n.is_empty() && n.chars().all(|c| c.is_ascii_digit()))
        })
        .map(|e| e.path().join("device"))
        .collect();
    cards.sort();
    cards
        .into_iter()
        .filter_map(|device| {
            Some(SysfsGpu {
                vendor_id: hex(read(device.join("vendor")))?,
                device_id: hex(read(device.join("device")))?,
                vram: number(read(device.join("mem_info_vram_total"))),
                gtt: number(read(device.join("mem_info_gtt_total"))),
            })
        })
        .collect()
}

/// NVML, opened at run time: absent library, no NVIDIA GPU.
mod nvml {
    use std::ffi::{c_char, c_int, c_uint, c_void};

    pub struct Device {
        pub name: String,
        pub total_bytes: u64,
    }

    pub struct Nvml {
        pub driver: Option<String>,
        /// `major * 1000 + minor * 10`, e.g. 12040.
        pub cuda_version: Option<i32>,
        pub devices: Vec<Device>,
    }

    #[repr(C)]
    struct Memory {
        total: u64,
        free: u64,
        used: u64,
    }

    const LIBRARIES: &[&str] = if cfg!(windows) {
        &["nvml.dll"]
    } else {
        &["libnvidia-ml.so.1", "libnvidia-ml.so"]
    };

    fn c_string(buffer: &[c_char]) -> String {
        let bytes: Vec<u8> = buffer
            .iter()
            .take_while(|c| **c != 0)
            .map(|c| *c as u8)
            .collect();
        String::from_utf8_lossy(&bytes).into_owned()
    }

    pub fn probe() -> Option<Nvml> {
        // SAFETY: the symbols are NVML's documented C API with these signatures; buffers
        // are sized as NVML asks (96 bytes for names and versions). The library stays
        // loaded for the life of the process.
        unsafe {
            let library = LIBRARIES
                .iter()
                .find_map(|name| libloading::Library::new(name).ok())?;
            let library: &'static libloading::Library = Box::leak(Box::new(library));
            let init: libloading::Symbol<unsafe extern "C" fn() -> c_int> =
                library.get(b"nvmlInit_v2\0").ok()?;
            if init() != 0 {
                return None;
            }
            let count_fn: libloading::Symbol<unsafe extern "C" fn(*mut c_uint) -> c_int> =
                library.get(b"nvmlDeviceGetCount_v2\0").ok()?;
            let handle_fn: libloading::Symbol<
                unsafe extern "C" fn(c_uint, *mut *mut c_void) -> c_int,
            > = library.get(b"nvmlDeviceGetHandleByIndex_v2\0").ok()?;
            let name_fn: libloading::Symbol<
                unsafe extern "C" fn(*mut c_void, *mut c_char, c_uint) -> c_int,
            > = library.get(b"nvmlDeviceGetName\0").ok()?;
            let memory_fn: libloading::Symbol<
                unsafe extern "C" fn(*mut c_void, *mut Memory) -> c_int,
            > = library.get(b"nvmlDeviceGetMemoryInfo\0").ok()?;

            let mut count: c_uint = 0;
            if count_fn(&mut count) != 0 {
                return None;
            }
            let mut devices = Vec::new();
            for index in 0..count {
                let mut handle: *mut c_void = std::ptr::null_mut();
                if handle_fn(index, &mut handle) != 0 {
                    continue;
                }
                let mut name = [0 as c_char; 96];
                let _ = name_fn(handle, name.as_mut_ptr(), name.len() as c_uint);
                let mut memory = Memory {
                    total: 0,
                    free: 0,
                    used: 0,
                };
                if memory_fn(handle, &mut memory) != 0 {
                    continue;
                }
                devices.push(Device {
                    name: c_string(&name),
                    total_bytes: memory.total,
                });
            }

            let driver = library
                .get::<unsafe extern "C" fn(*mut c_char, c_uint) -> c_int>(
                    b"nvmlSystemGetDriverVersion\0",
                )
                .ok()
                .and_then(|f| {
                    let mut buffer = [0 as c_char; 96];
                    (f(buffer.as_mut_ptr(), buffer.len() as c_uint) == 0).then(|| c_string(&buffer))
                });
            let cuda_version = library
                .get::<unsafe extern "C" fn(*mut c_int) -> c_int>(
                    b"nvmlSystemGetCudaDriverVersion_v2\0",
                )
                .ok()
                .and_then(|f| {
                    let mut version: c_int = 0;
                    (f(&mut version) == 0).then_some(version)
                });
            if let Ok(shutdown) = library.get::<unsafe extern "C" fn() -> c_int>(b"nvmlShutdown\0")
            {
                shutdown();
            }
            Some(Nvml {
                driver,
                cuda_version,
                devices,
            })
        }
    }
}

/// The Vulkan loader, opened at run time: the physical devices, their type and their
/// device-local memory. Only the core 1.0 entry points the loader exports.
mod vulkan {
    use std::ffi::{c_char, c_void};

    #[derive(Debug, Clone, Copy, PartialEq)]
    pub enum DeviceKind {
        Other,
        Integrated,
        Discrete,
        Virtual,
        Cpu,
    }

    #[derive(Debug, Clone)]
    pub struct Device {
        pub vendor_id: u32,
        pub device_id: u32,
        pub kind: DeviceKind,
        pub name: String,
        pub device_local_bytes: u64,
    }

    #[repr(C)]
    struct InstanceCreateInfo {
        s_type: u32,
        p_next: *const c_void,
        flags: u32,
        p_application_info: *const c_void,
        enabled_layer_count: u32,
        pp_enabled_layer_names: *const *const c_char,
        enabled_extension_count: u32,
        pp_enabled_extension_names: *const *const c_char,
    }

    const STRUCTURE_TYPE_INSTANCE_CREATE_INFO: u32 = 1;
    const MEMORY_HEAP_DEVICE_LOCAL: u32 = 1;

    // VkPhysicalDeviceProperties: apiVersion, driverVersion, vendorID, deviceID,
    // deviceType (u32 each), deviceName[256], ...; 824 bytes in all.
    const PROPERTIES_SIZE: usize = 1024;
    const VENDOR_OFFSET: usize = 8;
    const DEVICE_ID_OFFSET: usize = 12;
    const TYPE_OFFSET: usize = 16;
    const NAME_OFFSET: usize = 20;
    const NAME_LEN: usize = 256;
    // VkPhysicalDeviceMemoryProperties: memoryTypeCount, memoryTypes[32] (8 bytes each),
    // memoryHeapCount at 260, memoryHeaps[16] at 264 (size u64, flags u32, 16 bytes each).
    const MEMORY_SIZE: usize = 640;
    const HEAP_COUNT_OFFSET: usize = 260;
    const HEAPS_OFFSET: usize = 264;
    const HEAP_STRIDE: usize = 16;

    const LIBRARIES: &[&str] = if cfg!(windows) {
        &["vulkan-1.dll"]
    } else {
        &["libvulkan.so.1", "libvulkan.so"]
    };

    type Handle = *mut c_void;

    fn u32_at(buffer: &[u8], offset: usize) -> u32 {
        u32::from_ne_bytes(buffer[offset..offset + 4].try_into().expect("4 bytes"))
    }

    fn u64_at(buffer: &[u8], offset: usize) -> u64 {
        u64::from_ne_bytes(buffer[offset..offset + 8].try_into().expect("8 bytes"))
    }

    pub fn probe() -> Result<Vec<Device>, String> {
        if cfg!(target_os = "macos") {
            return Err("not on macOS".into());
        }
        // SAFETY: the entry points are Vulkan 1.0 core with these signatures; the property
        // structs are read through byte buffers larger than the structs, 8-byte aligned,
        // at the offsets the specification's layout gives. The loader stays loaded: some
        // drivers crash when unloaded.
        unsafe {
            let library = LIBRARIES
                .iter()
                .find_map(|name| libloading::Library::new(name).ok())
                .ok_or("no Vulkan loader")?;
            let library: &'static libloading::Library = Box::leak(Box::new(library));
            let symbol = |name: &[u8]| {
                library
                    .get::<*const c_void>(name)
                    .map(|s| *s)
                    .map_err(|e| e.to_string())
            };
            let create: unsafe extern "C" fn(
                *const InstanceCreateInfo,
                *const c_void,
                *mut Handle,
            ) -> i32 = std::mem::transmute(symbol(b"vkCreateInstance\0")?);
            let destroy: unsafe extern "C" fn(Handle, *const c_void) =
                std::mem::transmute(symbol(b"vkDestroyInstance\0")?);
            let enumerate: unsafe extern "C" fn(Handle, *mut u32, *mut Handle) -> i32 =
                std::mem::transmute(symbol(b"vkEnumeratePhysicalDevices\0")?);
            let properties: unsafe extern "C" fn(Handle, *mut u8) =
                std::mem::transmute(symbol(b"vkGetPhysicalDeviceProperties\0")?);
            let memory: unsafe extern "C" fn(Handle, *mut u8) =
                std::mem::transmute(symbol(b"vkGetPhysicalDeviceMemoryProperties\0")?);

            let info = InstanceCreateInfo {
                s_type: STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
                p_next: std::ptr::null(),
                flags: 0,
                p_application_info: std::ptr::null(),
                enabled_layer_count: 0,
                pp_enabled_layer_names: std::ptr::null(),
                enabled_extension_count: 0,
                pp_enabled_extension_names: std::ptr::null(),
            };
            let mut instance: Handle = std::ptr::null_mut();
            let rc = create(&info, std::ptr::null(), &mut instance);
            if rc != 0 || instance.is_null() {
                return Err(format!("vkCreateInstance failed ({rc})"));
            }
            let mut count = 0u32;
            enumerate(instance, &mut count, std::ptr::null_mut());
            let mut handles: Vec<Handle> = vec![std::ptr::null_mut(); count as usize];
            let rc = enumerate(instance, &mut count, handles.as_mut_ptr());
            if rc < 0 {
                destroy(instance, std::ptr::null());
                return Err(format!("vkEnumeratePhysicalDevices failed ({rc})"));
            }
            handles.truncate(count as usize);
            let mut devices = Vec::new();
            for handle in handles {
                let mut props = vec![0u64; PROPERTIES_SIZE / 8];
                properties(handle, props.as_mut_ptr() as *mut u8);
                let props: &[u8] =
                    std::slice::from_raw_parts(props.as_ptr() as *const u8, PROPERTIES_SIZE);
                let mut mem = vec![0u64; MEMORY_SIZE / 8];
                memory(handle, mem.as_mut_ptr() as *mut u8);
                let mem: &[u8] = std::slice::from_raw_parts(mem.as_ptr() as *const u8, MEMORY_SIZE);

                let name_bytes = &props[NAME_OFFSET..NAME_OFFSET + NAME_LEN];
                let name_len = name_bytes.iter().position(|b| *b == 0).unwrap_or(NAME_LEN);
                let heaps = u32_at(mem, HEAP_COUNT_OFFSET).min(16) as usize;
                let device_local_bytes = (0..heaps)
                    .map(|i| HEAPS_OFFSET + i * HEAP_STRIDE)
                    .filter(|at| u32_at(mem, at + 8) & MEMORY_HEAP_DEVICE_LOCAL != 0)
                    .map(|at| u64_at(mem, at))
                    .sum();
                devices.push(Device {
                    vendor_id: u32_at(props, VENDOR_OFFSET),
                    device_id: u32_at(props, DEVICE_ID_OFFSET),
                    kind: match u32_at(props, TYPE_OFFSET) {
                        1 => DeviceKind::Integrated,
                        2 => DeviceKind::Discrete,
                        3 => DeviceKind::Virtual,
                        4 => DeviceKind::Cpu,
                        _ => DeviceKind::Other,
                    },
                    name: String::from_utf8_lossy(&name_bytes[..name_len]).into_owned(),
                    device_local_bytes,
                });
            }
            destroy(instance, std::ptr::null());
            Ok(devices)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn gpu(
        vendor: GpuVendor,
        kind: GpuType,
        vram: Option<u64>,
        shared: Option<u64>,
        source: &str,
    ) -> GpuInfo {
        GpuInfo {
            vendor,
            kind,
            name: "g".into(),
            vram_bytes: vram,
            shared_bytes: shared,
            driver: None,
            cuda_version: None,
            source: source.into(),
        }
    }

    #[test]
    fn the_memory_budget_is_the_smaller_of_gpu_less_one_and_ram_less_four() {
        assert_eq!(memory_budget(32 * GIB, Some(12 * GIB)), 11 * GIB);
        assert_eq!(memory_budget(8 * GIB, Some(12 * GIB)), 4 * GIB);
        assert_eq!(memory_budget(8 * GIB, None), 4 * GIB);
        assert_eq!(memory_budget(2 * GIB, None), 0);
        assert_eq!(memory_budget(16 * GIB, Some(GIB / 2)), 0);
    }

    #[test]
    fn an_integrated_gpu_budget_counts_its_carve_out_and_gtt() {
        let strix = gpu(
            GpuVendor::Amd,
            GpuType::Integrated,
            Some(GIB / 2),
            Some(61 * GIB),
            "sysfs+vulkan",
        );
        assert_eq!(gpu_budget(&strix), Some(GIB / 2 + 61 * GIB));
        let intel = gpu(
            GpuVendor::Intel,
            GpuType::Integrated,
            None,
            Some(8 * GIB),
            "vulkan",
        );
        assert_eq!(gpu_budget(&intel), Some(8 * GIB));
        let card = gpu(
            GpuVendor::Nvidia,
            GpuType::Discrete,
            Some(12 * GIB),
            None,
            "nvml",
        );
        assert_eq!(gpu_budget(&card), Some(12 * GIB));
    }

    #[test]
    fn the_primary_gpu_is_the_largest_discrete_one() {
        let mut hardware = LocalHardware {
            os: "linux".into(),
            arch: "x64".into(),
            cpu: CpuInfo {
                brand: "c".into(),
                physical_cores: Some(8),
                logical_cores: 16,
                features: vec![],
            },
            ram_bytes: 32 * GIB,
            available_ram_bytes: 16 * GIB,
            gpus: vec![
                gpu(
                    GpuVendor::Intel,
                    GpuType::Integrated,
                    None,
                    Some(8 * GIB),
                    "vulkan",
                ),
                gpu(
                    GpuVendor::Nvidia,
                    GpuType::Discrete,
                    Some(6 * GIB),
                    None,
                    "nvml",
                ),
                gpu(
                    GpuVendor::Nvidia,
                    GpuType::Discrete,
                    Some(12 * GIB),
                    None,
                    "nvml",
                ),
            ],
            backends: vec![Backend::Cpu, Backend::Cuda, Backend::Vulkan],
            gpu_budget_bytes: None,
            memory_budget_bytes: 0,
            disk_free_bytes: None,
            storage_dir: "/x".into(),
            hardware_class: String::new(),
            notes: vec![],
        };
        finish(
            &mut hardware,
            &super::super::data::embedded().unwrap().classes,
        );
        assert_eq!(hardware.primary_gpu().unwrap().vram_bytes, Some(12 * GIB));
        assert_eq!(hardware.gpu_budget_bytes, Some(12 * GIB));
        assert_eq!(hardware.memory_budget_bytes, 11 * GIB);
        assert_eq!(hardware.hardware_class, "nvidia-12gb");

        // Without a GPU backend the GPU budget does not count.
        hardware.backends = vec![Backend::Cpu];
        finish(
            &mut hardware,
            &super::super::data::embedded().unwrap().classes,
        );
        assert_eq!(hardware.gpu_budget_bytes, None);
        assert_eq!(hardware.memory_budget_bytes, 28 * GIB);
    }

    /// A 16 GiB AMD card beside a 4 GiB NVIDIA card: the plan runs the CUDA build, which
    /// uses the NVIDIA card, so the budget is that card's, not the larger AMD card's.
    #[test]
    fn the_budget_is_the_gpu_of_the_backend_the_plan_runs() {
        let mut hardware = LocalHardware {
            os: "linux".into(),
            arch: "x64".into(),
            cpu: CpuInfo {
                brand: "c".into(),
                physical_cores: Some(8),
                logical_cores: 16,
                features: vec![],
            },
            ram_bytes: 64 * GIB,
            available_ram_bytes: 32 * GIB,
            gpus: vec![
                gpu(
                    GpuVendor::Amd,
                    GpuType::Discrete,
                    Some(16 * GIB),
                    None,
                    "sysfs+vulkan",
                ),
                gpu(
                    GpuVendor::Nvidia,
                    GpuType::Discrete,
                    Some(4 * GIB),
                    None,
                    "nvml",
                ),
            ],
            backends: vec![Backend::Cpu, Backend::Vulkan, Backend::Cuda],
            gpu_budget_bytes: None,
            memory_budget_bytes: 0,
            disk_free_bytes: None,
            storage_dir: "/x".into(),
            hardware_class: String::new(),
            notes: vec![],
        };
        let classes = &super::super::data::embedded().unwrap().classes;
        finish(&mut hardware, classes);
        assert_eq!(hardware.primary_gpu().unwrap().vendor, GpuVendor::Nvidia);
        assert_eq!(hardware.gpu_budget_bytes, Some(4 * GIB));
        assert_eq!(hardware.memory_budget_bytes, 3 * GIB);

        // Without CUDA the Vulkan build runs, on the larger card.
        hardware.backends = vec![Backend::Cpu, Backend::Vulkan];
        finish(&mut hardware, classes);
        assert_eq!(hardware.primary_gpu().unwrap().vendor, GpuVendor::Amd);
        assert_eq!(hardware.gpu_budget_bytes, Some(16 * GIB));
    }

    #[test]
    fn sysfs_cards_are_read_by_pci_id_and_amdgpu_memory() {
        let dir = tempfile::tempdir().unwrap();
        let card = dir.path().join("card1/device");
        std::fs::create_dir_all(&card).unwrap();
        std::fs::write(card.join("vendor"), "0x1002\n").unwrap();
        std::fs::write(card.join("device"), "0x1586\n").unwrap();
        std::fs::write(card.join("mem_info_vram_total"), "536870912\n").unwrap();
        std::fs::write(card.join("mem_info_gtt_total"), "65945571328\n").unwrap();
        // A connector of the card, not a card.
        std::fs::create_dir_all(dir.path().join("card1-DP-1/device")).unwrap();
        assert_eq!(
            sysfs_gpus_in(dir.path()),
            vec![SysfsGpu {
                vendor_id: 0x1002,
                device_id: 0x1586,
                vram: Some(536870912),
                gtt: Some(65945571328),
            }]
        );
    }

    /// Detection on the machine that runs the tests: it answers, and what it answers holds
    /// together. Prints it, for a look at a real device.
    #[test]
    fn detection_on_this_machine_is_consistent() {
        let dir = tempfile::tempdir().unwrap();
        let hardware = detect(dir.path(), &super::super::data::embedded().unwrap().classes);
        println!("{}", serde_json::to_string_pretty(&hardware).unwrap());
        assert!(hardware.ram_bytes > 0);
        assert!(hardware.backends.contains(&Backend::Cpu));
        assert!(hardware.memory_budget_bytes <= hardware.ram_bytes);
        assert!(!hardware.hardware_class.is_empty());
        assert!(hardware.disk_free_bytes.is_some());
    }
}
