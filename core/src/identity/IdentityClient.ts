import { ApiClient } from "../apiClient";
import type { ClaimOfferResult } from "../generated/api/ClaimOfferResult";
import type { DeviceView } from "../generated/api/DeviceView";
import type { IdentityLog } from "../generated/api/IdentityLog";
import type { IdentityStatus } from "../generated/api/IdentityStatus";
import type { JoinStatus } from "../generated/api/JoinStatus";
import type { OfferListing } from "../generated/api/OfferListing";
import type { OfferOpened } from "../generated/api/OfferOpened";
import type { SignedHead } from "../generated/api/SignedHead";
import type { SignedOffer } from "../generated/api/SignedOffer";
import type { SignedRequest } from "../generated/api/SignedRequest";

/**
 * The executor's `did:scid:coasys:1` calls.
 *
 * The executor holds device keys only. Master keys and the 12 words stay with the app
 * (`ad4m-identity-client`), which signs the events and offers these calls pass on.
 * `agent.status()` and `agent.me()` carry `identity`, the `did:scid` the agent speaks for.
 */
export class IdentityClient {
    #apiClient: ApiClient;

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    }

    /** This agent's identity, device key, `enc` key and the cached head. */
    async status(): Promise<IdentityStatus> {
        return this.#apiClient.call("identity.status", {});
    }

    /** A log through the executor's cache; `fresh` refreshes it from the store first. */
    async log(did: string, from?: number, fresh?: boolean): Promise<IdentityLog> {
        return this.#apiClient.call("identity.log", { did, from, fresh });
    }

    /** Adds this agent's device-key proof to an `incept` that lists its key. */
    async cosignIncept(event: unknown): Promise<unknown> {
        return this.#apiClient.call("identity.cosignIncept", { event: event as any });
    }

    /** Appends signed events through the agent language. */
    async publish(events: unknown[]): Promise<SignedHead> {
        return this.#apiClient.call("identity.publish", { events: events as any[] });
    }

    async openOffer(signedOffer: SignedOffer): Promise<OfferOpened> {
        return this.#apiClient.call("identity.openOffer", { signedOffer });
    }

    /** The identity's open and claimed offers, for a request signed by its current master key. */
    async offers(request: SignedRequest, wait?: number): Promise<OfferListing[]> {
        return this.#apiClient.call("identity.offers", { request, wait });
    }

    /** Claims an offer link for this executor (the operator only). */
    async claimOffer(offer: string, hint?: string): Promise<ClaimOfferResult> {
        return this.#apiClient.call("identity.claimOffer", { offer, hint });
    }

    async joinStatus(offerId: string): Promise<JoinStatus> {
        return this.#apiClient.call("identity.joinStatus", { offerId });
    }

    /** The identity's device keys, from its log. */
    async devices(): Promise<DeviceView[]> {
        return this.#apiClient.call("identity.devices", {});
    }
}
