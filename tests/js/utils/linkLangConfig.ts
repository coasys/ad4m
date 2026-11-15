// Test matrix helper: same integration tests, different link language.
//
// Each `LinkLangConfig` knows how to make a perspective a neighbourhood that
// syncs through its language (`shareNeighbourhood`). The tests stay ignorant
// of which language is running underneath — that's the whole point of the
// matrix.
//
// Holochain flavor: perspective-diff-sync. The test templates the language
//                   ({uid, name}) and publishes the perspective with it.
// Server flavor:    server-link-language. Only `neighbourhood.create` may make
//                   such a space: it templates the language with SERVER_URL,
//                   UID and CREATOR (the caller's person; the room's id is
//                   roomIdOf(CREATOR, UID)), writes the creator's `rules` link
//                   and publishes; the config then adds the suite's other
//                   persons as members. The executor refuses
//                   `neighbourhood.publish` for this language, so the caller
//                   must speak for a person (helpers/identity.ts givePerson).

import { Ad4mClient, LanguageRef, Perspective } from "@coasys/ad4m";
import { v4 as uuidv4 } from "uuid";

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
    /**
     * Whether the neighbourhood carries the `meta` perspective given to `shareNeighbourhood`
     * (Holochain: yes; server-link: `neighbourhood.create` publishes an empty meta).
     */
    carriesMeta: boolean;
    /** Makes `perspectiveUuid` a neighbourhood that syncs through this language. */
    share: (client: Ad4mClient, perspectiveUuid: string, name: string, meta?: Perspective) => Promise<SharedNeighbourhood>;
}

/** A neighbourhood `shareNeighbourhood` made. */
export interface SharedNeighbourhood {
    /** The neighbourhood URL (`neighbourhood://…`). */
    url: string;
    /** The address of its link language. */
    linkLanguage: string;
}

export function holochainLinkLang(languageHash: string): LinkLangConfig {
    const cfg: LinkLangConfig = {
        label: "holochain",
        languageHash,
        buildTemplateParams: (name) => JSON.stringify({ uid: uuidv4(), name }),
        carriesMeta: true,
        share: async (client, perspectiveUuid, name, meta = new Perspective()) => {
            const language = await publishLinkLanguage(client, cfg, name);
            const url = await client.neighbourhood.publishFromPerspective(perspectiveUuid, language.address, meta);
            return { url, linkLanguage: language.address };
        },
    };
    return cfg;
}

/**
 * Spaces on the link server at `serverUrl` (a protocol 2 server: `neighbourhood.create` checks
 * its `/.well-known/coasys-link-server`). `share` makes the space with `neighbourhood.create` as
 * the client's person (`add: admins`), then adds each person `members` names, other than the
 * client's own, as a member (`neighbourhood.addMember`): the room admits their keys before
 * they join, so these suites test link sync, not joining. (A person joining an open room by
 * its link alone is `spaces.test.ts`'s.) `buildTemplateParams` is kept for tests that template
 * the language without publishing: `creator` answers the person DID it names.
 */
export function serverLinkLang(
    languageHash: string,
    serverUrl: string,
    creator: () => string,
    members: () => string[] = () => [],
): LinkLangConfig {
    return {
        label: "server-link",
        languageHash,
        buildTemplateParams: (name) =>
            JSON.stringify({ SERVER_URL: serverUrl, UID: uuidv4(), CREATOR: creator(), name }),
        carriesMeta: false,
        share: async (client, perspectiveUuid) => {
            const made = await client.neighbourhood.create({
                perspectiveUuid,
                sync: { kind: "ownLinkServer", url: serverUrl },
                rules: { add: "admins", join: "open" },
            });
            const own = (await client.agent.me()).identity;
            for (const person of members()) {
                if (person !== own) await client.neighbourhood.addMember(perspectiveUuid, person, "member");
            }
            return { url: made.url, linkLanguage: made.choice.linkLanguage };
        },
    };
}

/**
 * Makes the perspective `perspectiveUuid` a neighbourhood that syncs through `cfg`'s language;
 * `name` names its link language where the language takes one, `meta` is published with it
 * where the language carries one (`cfg.carriesMeta`).
 */
export function shareNeighbourhood(
    client: Ad4mClient,
    cfg: LinkLangConfig,
    perspectiveUuid: string,
    name: string,
    meta?: Perspective,
): Promise<SharedNeighbourhood> {
    return cfg.share(client, perspectiveUuid, name, meta);
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
