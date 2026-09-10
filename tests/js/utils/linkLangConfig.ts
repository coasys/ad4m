// Test matrix helper: same integration tests, different link language.
//
// Each `LinkLangConfig` bundles the language hash the tests should template-
// and-publish, plus the template-variable filler for its specific language.
// The tests call `publishLinkLanguage(client, "some name")` and stay ignorant
// of which language is running underneath — that's the whole point of the
// matrix.
//
// Holochain flavor: perspective-diff-sync, params = {uid, name}.
// Server flavor:    server-link-language,  params = {SERVER_URL, ROOM_ID, name}
//                   (SERVER_URL comes from a running link-server, ROOM_ID is
//                    fresh per neighbourhood so tests don't collide).

import { Ad4mClient, LanguageRef } from "@coasys/ad4m";
import { v4 as uuidv4 } from "uuid";
import { sleep } from "./utils";

export interface LinkLangConfig {
    /** Display label used in describe() output, e.g. "holochain" / "server-link". */
    label: string;
    /** The published language hash to template from (a.k.a. DIFF_SYNC_OFFICIAL). */
    languageHash: string;
    /**
     * Build the JSON string passed as the templateParams argument of
     * `applyTemplateAndPublish`. The `name` becomes the neighbourhood's
     * display name and must survive the template step for existing test
     * assertions that check `socialContext.name === expected`.
     */
    buildTemplateParams: (name: string) => string;
}

export function holochainLinkLang(languageHash: string): LinkLangConfig {
    return {
        label: "holochain",
        languageHash,
        buildTemplateParams: (name) => JSON.stringify({ uid: uuidv4(), name }),
    };
}

export function serverLinkLang(languageHash: string, serverUrl: string): LinkLangConfig {
    return {
        label: "server-link",
        languageHash,
        buildTemplateParams: (name) =>
            JSON.stringify({ SERVER_URL: serverUrl, ROOM_ID: uuidv4(), name }),
    };
}

/**
 * Template-and-publish the configured link language. Convenience wrapper so
 * tests don't have to know the templateParams shape.
 */
export async function publishLinkLanguage(
    client: Ad4mClient,
    cfg: LinkLangConfig,
    name: string,
): Promise<LanguageRef> {
    return client.languages.applyTemplateAndPublish(cfg.languageHash, cfg.buildTemplateParams(name));
}

/**
 * Poll a predicate until it returns true, or throw with `label` after
 * `timeoutMs`. Replaces the "sleep long enough that Holochain surely
 * settled" pattern — different link languages have different natural
 * settle times, and fixed sleeps are the top source of matrix flake.
 *
 * The predicate may return a boolean synchronously or a Promise<boolean>;
 * exceptions from it are treated as "not yet true" so a not-yet-populated
 * lookup doesn't have to be defensively wrapped by every caller.
 */
export async function pollUntil(
    predicate: () => boolean | Promise<boolean>,
    opts: { timeoutMs?: number; intervalMs?: number; label?: string } = {},
): Promise<void> {
    const { timeoutMs = 15000, intervalMs = 200, label = "condition" } = opts;
    const deadline = Date.now() + timeoutMs;
    let lastError: unknown;
    while (Date.now() < deadline) {
        try {
            if (await predicate()) return;
        } catch (err) { lastError = err; /* treat as "not yet" */ }
        await sleep(intervalMs);
    }
    const suffix = lastError ? ` (last error: ${lastError instanceof Error ? lastError.message : String(lastError)})` : "";
    throw new Error(`pollUntil timed out after ${timeoutMs}ms waiting for: ${label}${suffix}`);
}
