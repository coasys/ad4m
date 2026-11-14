import { ApiClient } from "../apiClient";
import type { BotAction } from "../generated/api/BotAction";
import type { BotActivity } from "../generated/api/BotActivity";
import type { BotAgent } from "../generated/api/BotAgent";
import type { CreatedBot } from "../generated/api/CreatedBot";
import type { GrantState } from "../generated/api/GrantState";

/** What `bot.setGrant` takes: the whole grant, and the version it replaces. */
export interface BotGrantInput {
    /** Neighbourhood URLs and perspective UUIDs the bot may act in. */
    spaces: string[];
    can: BotAction[];
    /** Unix seconds; absent (or null) for no end but the key's in the owner's log. */
    expires?: number | null;
    state: GrantState;
    /** The version read (`BotAgent.grant.version`); a grant changed since answers 409. */
    version: number;
}

/**
 * Bots: agents that act for a person under a grant the person controls.
 *
 * The owner opens a bot offer with `ad4m-identity-client` (`LinkFlow::open_bot`), which names
 * what the bot's key may ever do (`read`, `write`, `join`) and until when. `createAgent` makes
 * the bot's agent on this executor and claims the offer; once the holder confirms the same
 * match code, `setGrant` says where and what the bot may do now. The bot's harness calls with
 * the token `createAgent` answers and gives a reason with every write
 * (`ApiClient.setReason` or `CallOptions.reason`). A call outside the grant answers 403
 * `bot-grant`, a paused bot `bot-paused`, an action outside the grant `not-in-can`.
 *
 * These calls are the operator's, or an app's on the main agent (which sees only the bots
 * of the main agent's identity).
 */
export class BotClient {
    #apiClient: ApiClient;

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    }

    /** Makes a bot agent that claims `offer` (the owner's bot offer); `name` is what the owner calls it. */
    async createAgent(offer: string, name: string): Promise<CreatedBot> {
        return this.#apiClient.call("bot.createAgent", { offer, name });
    }

    /**
     * Replaces the grant of the bot `bot` (its agent DID). `revoked` is for good. Only the
     * input's fields are sent, so a `BotGrant` read back (with its `updatedAt`) may be passed.
     */
    async setGrant(bot: string, grant: BotGrantInput): Promise<BotAgent> {
        const { spaces, can, expires, state, version } = grant;
        return this.#apiClient.call("bot.setGrant", {
            bot,
            spaces,
            can,
            expires: expires ?? undefined,
            state,
            version,
        });
    }

    /** Pauses the bot: every call in its spaces answers `bot-paused` until `resume`. */
    async pause(bot: string): Promise<BotAgent> {
        return this.#withState(bot, "paused");
    }

    async resume(bot: string): Promise<BotAgent> {
        return this.#withState(bot, "active");
    }

    /** Revokes the bot for good and ends its sessions; its activity log stays. */
    async revoke(bot: string): Promise<BotAgent> {
        return this.#withState(bot, "revoked");
    }

    async #withState(bot: string, state: GrantState): Promise<BotAgent> {
        const { grant } = await this.grant(bot);
        return this.setGrant(bot, { ...grant, state });
    }

    /** The bot, its grant, and what the owner's log says about its key. */
    async grant(bot: string): Promise<BotAgent> {
        return this.#apiClient.call("bot.grant", { bot });
    }

    async list(): Promise<BotAgent[]> {
        return this.#apiClient.call("bot.list", {});
    }

    /** The bot's activity, newest first: its writes and joins, with their reasons, and the calls refused. */
    async log(bot: string, options?: { before?: number; limit?: number }): Promise<BotActivity[]> {
        return this.#apiClient.call("bot.log", { bot, before: options?.before, limit: options?.limit });
    }
}
