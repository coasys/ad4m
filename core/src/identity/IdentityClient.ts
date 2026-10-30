import { ApiClient } from "../apiClient";
import type { ClaimOfferResult } from "../generated/api/ClaimOfferResult";
import type { DeviceView } from "../generated/api/DeviceView";
import type { DidResolutionResult } from "../generated/api/DidResolutionResult";
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

    /**
     * Resolves a `did:scid:coasys:1` DID URL (`?src=`, `?versionId=`, `#d1`) or a bare device
     * `did:key` to a W3C DID Core resolution result. The executor answers from its key-log
     * cache unless `fresh`, or the cache lacks the log or the asked version. A refusal comes
     * back as `didResolutionMetadata.error` (`invalidDid`, `invalidDidUrl`, `notFound`, …), not
     * as a thrown error. With a fragment, `didDocument` is null and `verificationMethod` holds
     * that method. For a `did:key`, `didResolutionMetadata["coasys:keyId"]` and
     * `["coasys:keyValid"]` say which key of the person's log it is and whether it is valid.
     */
    async resolve(
        didUrl: string,
        options: { fresh?: boolean; accept?: string } = {},
    ): Promise<DidResolutionResult> {
        return this.#apiClient.call("identity.resolve", {
            didUrl,
            fresh: options.fresh,
            accept: options.accept,
        });
    }

    /**
     * Drops this executor's cached copy of `did`'s log (its events, accepted head and recorded
     * forks); the next read refreshes from the configured store and takes the store's branch.
     * The way out of an `identity-fork`. The operator (admin credential) only.
     */
    async resetLog(did: string): Promise<boolean> {
        return this.#apiClient.call("identity.resetLog", { did });
    }

    /**
     * Adds this agent's device-key proof to an `incept` that lists its key. For the main
     * agent the admin credential only; a user's session for its own agent.
     */
    async cosignIncept(event: unknown): Promise<unknown> {
        return this.#apiClient.call("identity.cosignIncept", { event: event as any });
    }

    /**
     * Appends signed events through the agent language. For the main agent the admin
     * credential only; a user's session for its own agent.
     */
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
