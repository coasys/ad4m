import { ApiClient } from "../apiClient";
import type { ClaimOfferResult } from "../generated/api/ClaimOfferResult";
import type { DeviceKeys } from "../generated/api/DeviceKeys";
import type { DeviceView } from "../generated/api/DeviceView";
import type { DidResolutionResult } from "../generated/api/DidResolutionResult";
import type { IdentityLog } from "../generated/api/IdentityLog";
import type { IdentityStatus } from "../generated/api/IdentityStatus";
import type { JoinStatus } from "../generated/api/JoinStatus";
import type { KeyOwner } from "../generated/api/KeyOwner";
import type { OfferListing } from "../generated/api/OfferListing";
import type { OfferOpened } from "../generated/api/OfferOpened";
import type { SignedHead } from "../generated/api/SignedHead";
import type { SignedOffer } from "../generated/api/SignedOffer";
import type { SignedRequest } from "../generated/api/SignedRequest";
import type { DeviceRequest } from "../generated/api/DeviceRequest";
import type { DeviceRequestBody } from "../generated/api/DeviceRequestBody";
import type { InboxAddress } from "../generated/api/InboxAddress";
import type { InboxBind } from "../generated/api/InboxBind";
import type { InboxEntry } from "../generated/api/InboxEntry";
import type { InboxStored } from "../generated/api/InboxStored";
import type { PreparedDevice } from "../generated/api/PreparedDevice";
import type { RecordStored } from "../generated/api/RecordStored";
import type { RequestPosted } from "../generated/api/RequestPosted";
import type { SealedRecord } from "../generated/api/SealedRecord";
import type { TakenMaster } from "../generated/api/TakenMaster";

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

    /**
     * This agent's identity, device key, `enc` key and the cached head. `storeBehind` is set
     * (`{ since, seq }`) while the configured store answers the log behind the executor's cache,
     * and `null` once it catches up. `store` is the identity store this executor uses
     * (`{ url, trusted }`: its config's, else its network's) and whether it trusts a key for it;
     * `null` when its network has no store, so identities cannot be created or linked there.
     */
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

    /**
     * Claims an offer link for this executor (the operator only). `canKeepMaster` says this
     * device keeps the master key, which only an executor started with `--custody-channel`
     * may say (403 `no-custody-channel` otherwise).
     */
    async claimOffer(offer: string, hint?: string, canKeepMaster?: boolean): Promise<ClaimOfferResult> {
        return this.#apiClient.call("identity.claimOffer", { offer, hint, canKeepMaster });
    }

    async joinStatus(offerId: string): Promise<JoinStatus> {
        return this.#apiClient.call("identity.joinStatus", { offerId });
    }

    /**
     * The identity whose log lists `key` (a multikey), from the store's key index; `null` when
     * no log lists it. A restore from the 12 words finds its identity by master #0 this way.
     * A hint only: read the named log and check the key there.
     */
    async keyOwner(key: string): Promise<KeyOwner | null> {
        return this.#apiClient.call("identity.keyOwner", { key });
    }

    /**
     * The identity's device keys, from its log. Names, places and the master-key holders are
     * in the private record (`record()`), which the app opens and joins by `keyId`.
     */
    async devices(): Promise<DeviceView[]> {
        return this.#apiClient.call("identity.devices", {});
    }

    /**
     * This executor's device string (`coasys:device:1:…`), to show as a QR code. Until it
     * expires (5 minutes) the executor reads its pre-join inbox; a holder's ticket raises
     * `link-ticket` (the identity and the match code) once the executor claimed it. The operator
     * on this machine only.
     */
    async prepareDevice(): Promise<PreparedDevice> {
        return this.#apiClient.call("identity.prepareDevice", {});
    }

    /** Passes an inbox write the identity's current master key signed (`bind` for a pre-join address). */
    async inboxPut(
        address: InboxAddress,
        sealed: string,
        request: SignedRequest,
        bind?: InboxBind,
    ): Promise<InboxStored> {
        return this.#apiClient.call("identity.inboxPut", { address, sealed, request, bind });
    }

    /** This executor's waiting inbox messages, by id and body type (`kind`) only. The operator on this machine only. */
    async inbox(): Promise<InboxEntry[]> {
        return this.#apiClient.call("identity.inbox", {});
    }

    /**
     * The custody channel: the master key a holder sealed to this computer, checked against
     * the log, or `null`. Only the custody credential (`AD4M_CUSTODY_CREDENTIAL`) on a loopback
     * listener of an executor started with `--custody-channel`; any other caller, the admin
     * credential included, gets 403. Acknowledge the message
     * (`inboxAck`) once the key is stored.
     */
    async takeMaster(): Promise<TakenMaster | null> {
        return this.#apiClient.call("identity.takeMaster", {});
    }

    /** Deletes inbox message `id` at the store. The operator on this machine only. */
    async inboxAck(id: string): Promise<boolean> {
        return this.#apiClient.call("identity.inboxAck", { id });
    }

    /**
     * The identity's sealed private details record (`null` before the first write). Here and
     * in `recordPut`, `requests` and `deleteRequest`, `request` is a store request the
     * identity's master key signed (a custody service, a restoring app); without one the
     * executor signs with this agent's device key. A post is signed by the device key only.
     */
    async record(request?: SignedRequest): Promise<SealedRecord | null> {
        return this.#apiClient.call("identity.record", { request });
    }

    /** Writes the next version of the sealed record. */
    async recordPut(record: SealedRecord, request?: SignedRequest): Promise<RecordStored> {
        return this.#apiClient.call("identity.recordPut", { record, request });
    }

    /** The request box, since `since` (unix seconds). */
    async requests(since?: number, request?: SignedRequest): Promise<DeviceRequest[]> {
        return this.#apiClient.call("identity.requests", { since, request });
    }

    /** Posts a request to the box; it lives until `expires` (at most, and by default, 7 days). */
    async postRequest(body: DeviceRequestBody, expires?: number): Promise<RequestPosted> {
        return this.#apiClient.call("identity.postRequest", { body, expires });
    }

    async deleteRequest(id: string, request?: SignedRequest): Promise<boolean> {
        return this.#apiClient.call("identity.deleteRequest", { id, request });
    }

    /**
     * A new main wallet key for this executor, so a revoked executor can link again. The old
     * key stays for history. Only the operator on the executor's machine (the admin credential
     * on a loopback listener). A caller without the admin credential gets 403 "Admin credential
     * required"; the admin credential on a network listener gets 403 `local-only`.
     */
    async rekey(): Promise<DeviceKeys> {
        return this.#apiClient.call("identity.rekey", {});
    }

    /**
     * The custody channel: the device keys a master key for this computer is sealed to. Only
     * the custody credential (`AD4M_CUSTODY_CREDENTIAL`) on a loopback listener of an executor
     * started with `--custody-channel`; any other caller, the admin credential included, gets
     * 403.
     */
    async custodyStatus(): Promise<DeviceKeys> {
        return this.#apiClient.call("identity.custodyStatus", {});
    }
}
