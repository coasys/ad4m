/**
 * The hardware fingerprint every run records: OS, CPU, RAM, GPUs and their memory, usable
 * compute backends and driver versions. Read from /proc, sysfs, lspci, nvidia-smi and
 * rocm files on Linux; sysctl and system_profiler on macOS; CIM through PowerShell on
 * Windows. Anything unreadable is left out rather than guessed. No host name or user.
 */

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";
import { existsSync, readdirSync, readFileSync, realpathSync } from "node:fs";
import os from "node:os";
import path from "node:path";
import type { Backend, Fingerprint, GpuInfo } from "./types.js";

const GIB = 1024 ** 3;

function run(cmd: string, args: string[], timeout = 5000): string | null {
  try {
    return execFileSync(cmd, args, { encoding: "utf8", timeout, stdio: ["ignore", "pipe", "ignore"] }).trim();
  } catch {
    return null;
  }
}

function read(file: string): string | null {
  try {
    return readFileSync(file, "utf8").trim();
  } catch {
    return null;
  }
}

/**
 * AMD GPUs that share system memory: the APU families' marketing and code names. A
 * discrete Radeon is named "RX", "Pro" or "Instinct" and never matches.
 */
export function isIntegratedAmdName(name: string): boolean {
  if (/\b(RX|Instinct|FirePro)\b|Radeon Pro\b/i.test(name)) return false;
  return /Strix|Phoenix|Rembrandt|Renoir|Cezanne|Lucienne|Barcelo|Hawk Point|Krackan|Mendocino|Raphael|Granite Ridge|Picasso|Raven|Van Gogh|Radeon (?:\d{3,4}M|8060S|8050S|8040S|Graphics|Vega \d+)|Ryzen AI/i.test(
    name,
  );
}

/** The fingerprint's id: a hash of what classifies the hardware (CPU, threads, RAM in GiB, GPU names). */
export function fingerprintId(fp: Omit<Fingerprint, "id">): string {
  const key = [fp.os, fp.arch, fp.cpu.model, fp.cpu.threads, Math.round(fp.memoryBytes / GIB), ...fp.gpus.map((g) => `${g.vendor}:${g.name}:${Math.round(g.vramBytes / GIB)}`)].join("|");
  return createHash("sha256").update(key).digest("hex").slice(0, 8);
}

function linuxGpus(): GpuInfo[] {
  const gpus: GpuInfo[] = [];
  const lspci = run("lspci", ["-mm", "-nn", "-D"]) ?? "";
  const names = new Map<string, string>();
  for (const line of lspci.split("\n")) {
    const m = line.match(/^(\S+)\s+"(VGA compatible controller|3D controller|Display controller)[^"]*"\s+"([^"]*)"\s+"([^"]*)"/);
    if (m) names.set(m[1], `${m[3].replace(/\s*\[[0-9a-f]{4}\]$/, "")} ${m[4].replace(/\s*\[[0-9a-f]{4}\]$/, "")}`.trim());
  }

  // NVIDIA through its tool: it knows the memory and the driver.
  const smi = run("nvidia-smi", ["--query-gpu=name,memory.total,driver_version,pci.bus_id", "--format=csv,noheader,nounits"]);
  if (smi) {
    for (const line of smi.split("\n").filter(Boolean)) {
      const [name, memMiB, driver, bus] = line.split(",").map((s) => s.trim());
      gpus.push({ vendor: "nvidia", name, vramBytes: Number(memMiB) * 1024 * 1024, unified: false, driver, pciId: bus });
    }
  }

  // AMD and Intel through DRM sysfs.
  const drm = "/sys/class/drm";
  const seen = new Set<string>();
  for (const card of existsSync(drm) ? readdirSync(drm).filter((c) => /^card\d+$/.test(c)) : []) {
    const dev = path.join(drm, card, "device");
    const vendorId = read(path.join(dev, "vendor"));
    let slot = "";
    try {
      slot = path.basename(realpathSync(dev));
    } catch {}
    if (!vendorId || seen.has(slot)) continue;
    seen.add(slot);
    const name = names.get(slot) ?? names.get(slot.replace(/^0000:/, "")) ?? card;
    if (vendorId === "0x1002") {
      const vram = Number(read(path.join(dev, "mem_info_vram_total")) ?? 0);
      const gtt = Number(read(path.join(dev, "mem_info_gtt_total")) ?? 0);
      gpus.push({
        vendor: "amd",
        name,
        vramBytes: vram,
        gttBytes: gtt || undefined,
        unified: isIntegratedAmdName(name) || (vram > 0 && vram < 2 * GIB && gtt > vram),
        pciId: slot,
      });
    } else if (vendorId === "0x8086") {
      gpus.push({ vendor: "intel", name, vramBytes: 0, unified: !/Arc A\d|Arc B\d|Data Center/i.test(name), pciId: slot });
    }
  }
  return gpus;
}

function linuxHasVulkan(): boolean {
  const ld = run("ldconfig", ["-p"]) ?? run("/sbin/ldconfig", ["-p"]) ?? "";
  return /libvulkan\.so\.1\b/.test(ld) && existsSync("/dev/dri");
}

function linuxFingerprint(): Omit<Fingerprint, "id"> {
  const osRelease = read("/etc/os-release") ?? "";
  const pretty = osRelease.match(/^PRETTY_NAME="?([^"\n]*)"?/m)?.[1] ?? "Linux";
  const cpuinfo = read("/proc/cpuinfo") ?? "";
  const cores = new Set<string>();
  let phys = "0";
  for (const line of cpuinfo.split("\n")) {
    const [k, v] = line.split(":").map((s) => s?.trim());
    if (k === "physical id") phys = v;
    if (k === "core id") cores.add(`${phys}:${v}`);
  }
  const gpus = linuxGpus();
  const backends: Backend[] = ["cpu"];
  const drivers: Record<string, string> = {};
  const nvidia = gpus.find((g) => g.vendor === "nvidia");
  if (nvidia?.driver) {
    backends.push("cuda");
    drivers.nvidia = nvidia.driver;
    const cuda = run("nvidia-smi", [])?.match(/CUDA Version:\s*([\d.]+)/)?.[1];
    if (cuda) drivers.cuda = cuda;
  }
  if (gpus.some((g) => g.vendor === "amd") && existsSync("/dev/kfd")) {
    backends.push("rocm");
    const rocm = read("/opt/rocm/.info/version");
    if (rocm) drivers.rocm = rocm;
  }
  if (gpus.some((g) => g.vendor === "amd")) {
    const amdgpu = read("/sys/module/amdgpu/version");
    drivers.amdgpu = amdgpu || `in-kernel ${os.release()}`;
  }
  if (gpus.length && linuxHasVulkan()) {
    backends.push("vulkan");
    const api = run("vulkaninfo", ["--summary"])?.match(/apiVersion\s*=\s*([\d.]+)/)?.[1];
    if (api) drivers.vulkan = api;
  }
  return {
    os: "linux",
    osVersion: pretty,
    kernel: os.release(),
    arch: archOf(),
    cpu: { model: os.cpus()[0]?.model?.trim() ?? "unknown", cores: cores.size || os.cpus().length, threads: os.cpus().length },
    memoryBytes: os.totalmem(),
    gpus,
    backends,
    gpuBudgetBytes: gpuBudget(gpus, backends, os.totalmem()),
    drivers,
  };
}

function macFingerprint(): Omit<Fingerprint, "id"> {
  const memory = Number(run("sysctl", ["-n", "hw.memsize"]) ?? os.totalmem());
  const arm = archOf() === "arm64";
  const gpus: GpuInfo[] = [];
  const sp = run("system_profiler", ["SPDisplaysDataType", "-json"], 15000);
  try {
    for (const d of JSON.parse(sp ?? "{}").SPDisplaysDataType ?? []) {
      const name = d.sppci_model ?? d._name ?? "GPU";
      const vendor = arm ? "apple" : /nvidia/i.test(name) ? "nvidia" : /amd|radeon/i.test(name) ? "amd" : /intel/i.test(name) ? "intel" : "other";
      const vram = Number(String(d.spdisplays_vram ?? d.spdisplays_vram_shared ?? "0").match(/(\d+)\s*GB/)?.[1] ?? 0) * GIB;
      gpus.push({ vendor, name, vramBytes: arm ? 0 : vram, unified: arm || vendor === "intel" });
    }
  } catch {}
  if (arm && gpus.length === 0) gpus.push({ vendor: "apple", name: run("sysctl", ["-n", "machdep.cpu.brand_string"]) ?? "Apple Silicon", vramBytes: 0, unified: true });
  const backends: Backend[] = arm ? ["cpu", "metal"] : ["cpu"];
  return {
    os: "macos",
    osVersion: `macOS ${run("sw_vers", ["-productVersion"]) ?? ""}`.trim(),
    kernel: os.release(),
    arch: archOf(),
    cpu: {
      model: run("sysctl", ["-n", "machdep.cpu.brand_string"]) ?? os.cpus()[0]?.model ?? "unknown",
      cores: Number(run("sysctl", ["-n", "hw.physicalcpu"]) ?? os.cpus().length),
      threads: Number(run("sysctl", ["-n", "hw.logicalcpu"]) ?? os.cpus().length),
    },
    memoryBytes: memory,
    gpus,
    backends,
    gpuBudgetBytes: gpuBudget(gpus, backends, memory),
    drivers: {},
  };
}

function windowsFingerprint(): Omit<Fingerprint, "id"> {
  const ps = (cmd: string) => run("powershell", ["-NoProfile", "-Command", cmd], 20000);
  const json = (s: string | null) => {
    try {
      const v = JSON.parse(s ?? "null");
      return Array.isArray(v) ? v : v ? [v] : [];
    } catch {
      return [];
    }
  };
  const cpu = json(ps("Get-CimInstance Win32_Processor | Select-Object Name,NumberOfCores,NumberOfLogicalProcessors | ConvertTo-Json"))[0] ?? {};
  const osInfo = json(ps("Get-CimInstance Win32_OperatingSystem | Select-Object Caption,Version | ConvertTo-Json"))[0] ?? {};
  const gpus: GpuInfo[] = [];
  const smi = run("nvidia-smi", ["--query-gpu=name,memory.total,driver_version", "--format=csv,noheader,nounits"]);
  for (const line of (smi ?? "").split("\n").filter(Boolean)) {
    const [name, memMiB, driver] = line.split(",").map((s) => s.trim());
    gpus.push({ vendor: "nvidia", name, vramBytes: Number(memMiB) * 1024 * 1024, unified: false, driver });
  }
  for (const v of json(ps("Get-CimInstance Win32_VideoController | Select-Object Name,AdapterRAM,DriverVersion | ConvertTo-Json"))) {
    const name = String(v.Name ?? "");
    if (/nvidia/i.test(name) && gpus.some((g) => g.vendor === "nvidia")) continue;
    const vendor = /nvidia/i.test(name) ? "nvidia" : /amd|radeon/i.test(name) ? "amd" : /intel/i.test(name) ? "intel" : "other";
    // AdapterRAM is a 32-bit field: wrong above 4 GiB, so treat it as a floor.
    gpus.push({ vendor, name, vramBytes: Number(v.AdapterRAM ?? 0), unified: vendor === "amd" ? isIntegratedAmdName(name) : vendor === "intel", driver: v.DriverVersion });
  }
  const backends: Backend[] = ["cpu"];
  if (gpus.some((g) => g.vendor === "nvidia" && g.driver)) backends.push("cuda");
  if (gpus.length && existsSync(path.join(process.env.SystemRoot ?? "C:\\Windows", "System32", "vulkan-1.dll"))) backends.push("vulkan");
  const memory = os.totalmem();
  return {
    os: "windows",
    osVersion: `${osInfo.Caption ?? "Windows"} ${osInfo.Version ?? ""}`.trim(),
    kernel: os.release(),
    arch: archOf(),
    cpu: { model: cpu.Name ?? os.cpus()[0]?.model ?? "unknown", cores: cpu.NumberOfCores ?? os.cpus().length, threads: cpu.NumberOfLogicalProcessors ?? os.cpus().length },
    memoryBytes: memory,
    gpus,
    backends,
    gpuBudgetBytes: gpuBudget(gpus, backends, memory),
    drivers: Object.fromEntries(gpus.filter((g) => g.driver).map((g) => [g.vendor, g.driver!])),
  };
}

function archOf(): Fingerprint["arch"] {
  return process.arch === "x64" ? "x64" : process.arch === "arm64" ? "arm64" : "other";
}

/**
 * Memory a GPU backend can use: a discrete card's VRAM; an AMD APU's carve-out plus the
 * GTT it may map; three quarters of RAM on Apple Silicon (macOS's default wired limit).
 * Null without a GPU backend.
 */
export function gpuBudget(gpus: GpuInfo[], backends: Backend[], memoryBytes: number): number | null {
  if (!backends.some((b) => b !== "cpu")) return null;
  const budgets = gpus.map((g) => {
    if (g.vendor === "apple") return Math.floor(memoryBytes * 0.75);
    if (g.unified) return g.vramBytes + (g.gttBytes ?? Math.floor(memoryBytes / 2));
    return g.vramBytes;
  });
  const best = Math.max(0, ...budgets);
  return best > 0 ? best : null;
}

export function collectFingerprint(): Fingerprint {
  const base = process.platform === "linux" ? linuxFingerprint() : process.platform === "darwin" ? macFingerprint() : process.platform === "win32" ? windowsFingerprint() : {
    os: "other" as const,
    osVersion: process.platform,
    kernel: os.release(),
    arch: archOf(),
    cpu: { model: os.cpus()[0]?.model ?? "unknown", cores: os.cpus().length, threads: os.cpus().length },
    memoryBytes: os.totalmem(),
    gpus: [],
    backends: ["cpu" as Backend],
    gpuBudgetBytes: null,
    drivers: {},
  };
  return { ...base, id: fingerprintId(base) };
}

/** `<os>-<arch>` as manifest build keys start. */
export function platformKey(fp: Pick<Fingerprint, "os" | "arch">): string {
  return `${fp.os}-${fp.arch}`;
}
