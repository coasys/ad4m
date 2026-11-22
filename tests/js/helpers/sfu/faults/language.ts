/**
 * Publish the blocking link language (`blocking-link-language.js`, next to
 * this file) for a neighbourhood: a link language whose commit sleeps for
 * the milliseconds in a link's target when the link's predicate is
 * `soak://slow`, so a call path can be made to wait on the language the way
 * staging's commits made it wait.
 */

import { fileURLToPath } from "node:url";
import type { Ad4mClient } from "@coasys/ad4m";
import { LanguageMetaInput } from "@coasys/ad4m";

const LANGUAGE_PATH = fileURLToPath(new URL("./blocking-link-language.js", import.meta.url));

/** A commit carrying a link with this predicate sleeps for its target's ms. */
export const SLOW_PREDICATE = "soak://slow";

/** Publish the language's template; returns its address for `applyTemplateAndPublish`. */
export async function publishBlockingLanguage(client: Ad4mClient): Promise<string> {
  const meta = new LanguageMetaInput("blocking-link-language", "Test link language whose commit can be told to sleep");
  meta.possibleTemplateParams = ["uid"];
  const published = await client.languages.publish(LANGUAGE_PATH, meta);
  return published.address;
}
