import type { PluginConfig } from "./types";

export function generateRandomPassphrase(length: number = 32): string {
  const chars =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let result = "";
  for (let i = 0; i < length; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return result;
}

/**
 * Persist plugin config fields to the OpenClaw config file.
 * Uses api.runtime.config to load the full config, patch our plugin entry,
 * and write it back.
 */
export async function updatePluginConfig(
  api: any,
  patch: Partial<PluginConfig>,
  logger?: any,
): Promise<void> {
  try {
    const cfg = api.runtime.config.loadConfig();
    const pluginId = api.id;
    const entries = cfg.plugins?.entries ?? {};
    const existing = entries[pluginId] ?? {};
    const existingConfig = (existing.config ?? {}) as Record<string, unknown>;

    const next = {
      ...cfg,
      plugins: {
        ...cfg.plugins,
        entries: {
          ...entries,
          [pluginId]: {
            ...existing,
            config: { ...existingConfig, ...patch },
          },
        },
      },
    };

    await api.runtime.config.writeConfigFile(next);
    logger?.info?.(
      `[ad4m] Config persisted: ${Object.keys(patch).join(", ")}`,
    );
  } catch (e: any) {
    logger?.error?.(
      `[ad4m] Failed to persist config: ${e.message}`,
    );
  }
}

