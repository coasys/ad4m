import type {
  Ad4mClient,
  DetectedEndpoint,
  LocalAiHardware,
  LocalAiPlan,
  LocalAiProgress,
  LocalAiStatus,
} from "@coasys/ad4m";
import { useCallback, useEffect, useState, type Dispatch, type SetStateAction } from "react";
import { basisNote, kindLabel, localAction, serverInstead, toDownload } from "./aiSetup";

/** How often the panel reads local AI's status and plan again while it shows them. */
const REFRESH_MS = 5000;

/** The start of the error a failed read of the device, plan or status shows. */
const LOAD_ERROR = "Could not check this device: ";

const SERVICE_LABELS: Record<string, string> = {
  llm: "Chat",
  embeddings: "Embeddings",
  stt: "Transcription",
  tts: "Speech",
};

function gb(bytes: number): string {
  return bytes >= 1e9 ? `${(bytes / 1e9).toFixed(1)} GB` : `${Math.max(1, Math.round(bytes / 1e6))} MB`;
}

/** One line for the detected device: CPU, RAM, the GPU a runtime would use. */
function describeDevice(hardware: LocalAiHardware): string {
  const gpu =
    hardware.gpus.find((g) => g.type === "discrete") ?? hardware.gpus[0];
  // A discrete GPU whose VRAM is not known gets no suffix.
  const gpuSuffix =
    gpu?.type === "discrete"
      ? gpu.vramBytes
        ? ` (${gb(gpu.vramBytes)})`
        : ""
      : gpu?.type === "unified"
        ? " (unified memory)"
        : " (integrated)";
  const gpuText = gpu ? `${gpu.name}${gpuSuffix}` : "no GPU";
  const backends = hardware.backends.filter((b) => b !== "cpu");
  return `${hardware.cpu.brand || hardware.arch}, ${gb(hardware.ramBytes)} RAM, ${gpuText}${backends.length ? `, ${backends.join(" + ")}` : ""}`;
}

/**
 * Local AI's state for a screen that offers it (onboarding, Settings): the device, the plan
 * and the status, read at once and again every few seconds while `active` and no install
 * runs; the install with its progress, and the start. Errors go to `setError`, the screen's
 * error line.
 */
export function useLocalAi(
  client: Ad4mClient,
  active: boolean,
  setError: Dispatch<SetStateAction<string>>
) {
  const [hardware, setHardware] = useState<LocalAiHardware | null>(null);
  const [plan, setPlan] = useState<LocalAiPlan | null>(null);
  const [status, setStatus] = useState<LocalAiStatus | null>(null);
  const [progress, setProgress] = useState<LocalAiProgress | null>(null);
  const [installing, setInstalling] = useState(false);
  const [starting, setStarting] = useState(false);
  const [loadFailed, setLoadFailed] = useState(false);

  const load = useCallback(async () => {
    try {
      const [h, p, s] = await Promise.all([
        client.ai.localHardware(),
        client.ai.localPlan(),
        client.ai.localStatus(),
      ]);
      setHardware(h);
      setPlan(p);
      setStatus(s);
      setLoadFailed(false);
      // A read that failed before is answered now: its error goes.
      setError((shown) => (shown.startsWith(LOAD_ERROR) ? "" : shown));
    } catch (e) {
      setLoadFailed(true);
      setError(`${LOAD_ERROR}${e}`);
    }
  }, [client, setError]);

  useEffect(() => {
    load();
  }, [load]);

  // While the screen shows local AI: its status and plan again now and then, so a runtime
  // that stops, or a setup that ends elsewhere, shows here.
  useEffect(() => {
    if (!active || installing) return;
    // One refresh at a time, and none lands after the effect ends: an older answer must
    // not overwrite a newer one, or the state an install or a mode change set.
    let cancelled = false;
    let inFlight = false;
    const timer = setInterval(() => {
      if (inFlight) return;
      inFlight = true;
      Promise.all([client.ai.localStatus(), client.ai.localPlan()])
        .then(([s, p]) => {
          if (cancelled) return;
          setStatus(s);
          setPlan(p);
        })
        .catch(() => {})
        .finally(() => {
          inFlight = false;
        });
    }, REFRESH_MS);
    return () => {
      cancelled = true;
      clearInterval(timer);
    };
  }, [active, installing, client]);

  /** Download and set up what the plan names; the status after it, or `null` when it failed. */
  const install = useCallback(async (): Promise<LocalAiStatus | null> => {
    setInstalling(true);
    setError("");
    setProgress(null);
    let installed: LocalAiStatus | null = null;
    try {
      installed = await client.ai.localInstall({}, (p) => setProgress(p));
      setStatus(installed);
      setPlan(await client.ai.localPlan());
    } catch (e) {
      setError(`${e}`);
    }
    setInstalling(false);
    return installed;
  }, [client, setError]);

  /** Start what is installed; the status after it, or `null` when it failed. */
  const start = useCallback(async (): Promise<LocalAiStatus | null> => {
    setStarting(true);
    setError("");
    let started: LocalAiStatus | null = null;
    try {
      started = await client.ai.localStart();
      setStatus(started);
    } catch (e) {
      setError(`${e}`);
    }
    setStarting(false);
    return started;
  }, [client, setError]);

  return { hardware, plan, status, setStatus, progress, installing, starting, loadFailed, load, install, start };
}

export type LocalAi = ReturnType<typeof useLocalAi>;

/**
 * The local AI panel: what this device has, what local AI would download and how much, and
 * the question; the install's progress; the runtimes' state; start when it is installed.
 * Nothing downloads until the person says yes.
 */
export function LocalAiPanel(props: {
  local: LocalAi;
  /** Runs the install (the person said yes). Defaults to `local.install`. */
  onInstall?: () => void;
  /** Runs the start. Defaults to `local.start`. */
  onStart?: () => void;
  /** The "no" answer; without it the panel shows none. */
  onDecline?: { label: string; run: () => void };
  /** Model servers found on this computer, offered instead of a download with `onUseServer`. */
  detected?: DetectedEndpoint[] | null;
  onUseServer?: () => void;
  /** What the panel says while local AI runs. */
  runningText?: string;
  /** Offers a stop while local AI runs; without it the panel shows none. */
  onStop?: { run: () => void; stopping: boolean };
  /** A heading inside the panel's box. */
  title?: string;
}) {
  const { local, onDecline, detected = null, onUseServer, runningText = "Local AI runs.", onStop, title } =
    props;
  const { hardware, plan, status, progress, installing, starting, loadFailed } = local;
  const onInstall = props.onInstall ?? (() => void local.install());
  const onStart = props.onStart ?? (() => void local.start());

  const action = plan ? localAction(status, plan, installing) : "install";
  const instead =
    onUseServer && action === "install" && !installing
      ? serverInstead(
          detected,
          (status?.runtimes ?? []).flatMap((r) => (r.port ? [r.port] : []))
        )
      : null;

  if ((!hardware || !plan) && loadFailed) {
    return (
      <j-button variant="primary" onClick={local.load}>
        Check this device again
      </j-button>
    );
  }
  if (!hardware || !plan) return <j-text>Checking this device...</j-text>;

  return (
    <j-box className="box">
      <j-flex direction="column" gap="300">
        {title && (
          <j-text variant="heading-sm" nomargin>
            {title}
          </j-text>
        )}
        <j-text nomargin color="ui-0">
          Found on this computer: {describeDevice(hardware)}
        </j-text>
        <j-text nomargin size="400" color="ui-700">
          Class {plan.hardwareClass}; the models may use {gb(plan.memoryBudgetBytes)} of memory.
        </j-text>
        {plan.models.map((m) => (
          <j-flex key={m.modelId} a="center" j="between" gap="300">
            <j-text nomargin size="400">
              {SERVICE_LABELS[m.service] ?? m.service}: {m.modelId}
              {basisNote(m) && ` (${basisNote(m)})`}
            </j-text>
            <j-text nomargin size="400" color="ui-700">
              {m.installed ? "downloaded" : gb(m.sizeBytes)}
            </j-text>
          </j-flex>
        ))}
        {plan.runtimes.map((r) => (
          <j-flex key={r.runtimeId} a="center" j="between" gap="300">
            <j-text nomargin size="400">
              Runtime: {r.runtimeId} {r.version} ({r.backend})
            </j-text>
            <j-text nomargin size="400" color="ui-700">
              {r.installed ? "downloaded" : gb(r.sizeBytes)}
            </j-text>
          </j-flex>
        ))}
        {plan.unavailable.map((u) => (
          <j-text key={u.service} nomargin size="400" color="warning-500">
            {SERVICE_LABELS[u.service] ?? u.service}: {u.reason}
          </j-text>
        ))}
        {plan.reasons.map((r) => (
          <j-text key={r} nomargin size="400" color={plan.fits ? "ui-700" : "danger-500"}>
            {r}
          </j-text>
        ))}
        <j-text nomargin color="ui-0">
          {plan.downloadBytes > 0 ? `Download: ${gb(plan.downloadBytes)}` : "Everything is downloaded"}
          {plan.fits ? ", fits this device" : ", does not fit this device"}
        </j-text>

        {installing && progress && (
          <j-flex direction="column" gap="200">
            <progress
              max={Math.max(progress.totalBytes, 1)}
              value={progress.doneBytes}
              style={{ width: "100%" }}
            />
            <j-text nomargin size="400" color="ui-700">
              {progress.phase === "download" && progress.item
                ? `${progress.item}: ${gb(progress.itemDoneBytes)} of ${gb(progress.itemTotalBytes)}`
                : progress.phase}
            </j-text>
            {progress.detail && progress.phase !== "failed" && (
              <j-text nomargin size="400" color="warning-500">
                {progress.detail}
              </j-text>
            )}
          </j-flex>
        )}
        {installing && !progress && <j-text nomargin>Preparing...</j-text>}

        {status?.installed && !installing && (
          <j-flex direction="column" gap="200">
            {status.runtimes.map((r) => (
              <j-text key={r.provider} nomargin size="400">
                {r.provider}: {r.phase ?? "not started"}
                {r.port ? ` on port ${r.port}` : ""}
                {r.onGpu === true ? `, on the GPU (${gb(r.gpuBytes ?? 0)})` : ""}
                {r.onGpu === false ? ", on the CPU: the GPU build found no device" : ""}
                {r.detail ? ` (${r.detail})` : ""}
              </j-text>
            ))}
          </j-flex>
        )}

        {action === "running" ? (
          <j-flex a="center" j="between" gap="300">
            <j-text nomargin color="success-500">
              {runningText}
            </j-text>
            {onStop && (
              <j-button variant="subtle" size="sm" onClick={onStop.run} loading={onStop.stopping}>
                Stop local AI
              </j-button>
            )}
          </j-flex>
        ) : action === "start" ? (
          <j-button variant="primary" onClick={onStart} loading={starting}>
            Start local AI
          </j-button>
        ) : (
          <j-flex direction="column" gap="300">
            {!installing && plan.fits && (
              <j-text nomargin color="ui-0">
                {plan.downloadBytes > 0
                  ? `Local AI would download ${toDownload(plan).join(", ") || "its files"} to this computer: ${gb(plan.downloadBytes)} in total. Nothing is downloaded until you say yes. Download and set it up?`
                  : "Everything local AI needs is on this computer. Set it up?"}
              </j-text>
            )}
            <j-flex gap="300" wrap>
              <j-button
                variant="primary"
                disabled={!plan.fits || installing}
                loading={installing}
                onClick={onInstall}
              >
                {plan.downloadBytes > 0 ? `Yes, download ${gb(plan.downloadBytes)}` : "Yes, set it up"}
              </j-button>
              {!installing && onDecline && (
                <j-button variant="subtle" onClick={onDecline.run}>
                  {onDecline.label}
                </j-button>
              )}
            </j-flex>
            {instead && onUseServer && (
              <j-button variant="link" onClick={onUseServer}>
                Use the {kindLabel(instead.kind)} already running at {instead.baseUrl} instead
              </j-button>
            )}
          </j-flex>
        )}
      </j-flex>
    </j-box>
  );
}
