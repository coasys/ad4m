import { ApiClient } from "../apiClient";
import type { ClaimOfferResult } from "../generated/api/ClaimOfferResult";
import type { DeviceKeys } from "../generated/api/DeviceKeys";
import type { DeviceView } from "../generated/api/DeviceView";
import type { DidResolutionResult } from "../generated/api/DidResolutionResult";
import type { IdentityLog } from "../generated/api/IdentityLog";
import type { IdentityStatus } from "../generated/api/IdentityStatus";
import type { JoinAccepted } from "../generated/api/JoinAccepted";
import type { JoinCancelled } from "../generated/api/JoinCancelled";
import type { JoinStatus } from "../generated/api/JoinStatus";
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
import type { PreparedEnded } from "../generated/api/PreparedEnded";
import type { RecordStored } from "../generated/api/RecordStored";
import type { RequestPosted } from "../generated/api/RequestPosted";
import type { SealedInboxMessage } from "../generated/api/SealedInboxMessage";
import type { SealedRecord } from "../generated/api/SealedRecord";
import type { StoredRecord } from "../generated/api/StoredRecord";

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

    /**
     * This executor's device key leaves the agent's identity, signed with the key itself: no
     * master key. The key is dead on this executor from then on and the agent is signed out
     * (every session and app token of it ends, this call's included once it answers): the main
     * agent gets a new `did:key`, a user's agent needs a new one to be used again. Answers the
     * log's head after the `unlink`. 409 `no-identity` for an agent without an identity,
     * `not-listed` for a key the log does not list, `key-dead` for a key already unlinked or
     * revoked. A user's session for its own agent or the operator (the admin credential, for
     * the main agent), never an app token: 403 for an app, even one the user approved.
     */
    async unlinkSelf(): Promise<SignedHead> {
        return this.#apiClient.call("identity.unlinkSelf", {});
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
     * device keeps the master key, and `enc` is the custody service's own X25519 key, which
     * the claim carries so a master key is sealed to it. Only the custody service of an
     * executor started with `--custody-channel` passes them (403 `no-custody-channel`
     * otherwise), and only together (400 `no-custody-enc` for `canKeepMaster` without `enc`,
     * 400 for `enc` without `canKeepMaster`). Claiming the same offer again keeps the claim as
     * it is (`needsAccept` as it stands): 409 `cancelled` once cancelled, 409 `offer-conflict`
     * when the offer was claimed for another identity.
     */
    async claimOffer(
        offer: string,
        hint?: string,
        canKeepMaster?: boolean,
        enc?: string,
    ): Promise<ClaimOfferResult> {
        return this.#apiClient.call("identity.claimOffer", { offer, hint, canKeepMaster, enc });
    }

    /**
     * The state of this executor's claim on `offerId`; on `spent` the main agent has joined the
     * identity. `awaiting-accept` while a claim with `needsAccept` waits for `acceptJoin`
     * (nothing is bound, even once the holder confirmed); `cancelled` after `cancelJoin`.
     */
    async joinStatus(offerId: string): Promise<JoinStatus> {
        return this.#apiClient.call("identity.joinStatus", { offerId });
    }

    /**
     * Accepts a claim with `needsAccept` (a desktop's first link, which makes the identity
     * this executor's operator): the next `joinStatus` that reads `spent` joins. Before or
     * after the holder confirms; again is a no-op. 404 when no claim on `offerId` awaits
     * accept, 409 `cancelled` once cancelled. The operator on this machine only.
     */
    async acceptJoin(offerId: string): Promise<JoinAccepted> {
        return this.#apiClient.call("identity.acceptJoin", { offerId });
    }

    /**
     * Cancels this executor's claim on `offerId` (any claim): it never joins, even if the
     * holder confirms it later. Again is a no-op. 404 for no claim, 409 `already-joined` once
     * the main agent joined through it. The operator on this machine only.
     */
    async cancelJoin(offerId: string): Promise<JoinCancelled> {
        return this.#apiClient.call("identity.cancelJoin", { offerId });
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
     * on this machine only. With `enc`, the custody service's own X25519 key (custody channel
     * only, 403 otherwise), the claim says `canKeepMaster` and carries that key.
     */
    async prepareDevice(enc?: string): Promise<PreparedDevice> {
        return this.#apiClient.call("identity.prepareDevice", { enc });
    }

    /**
     * Ends the device string `device` (from `prepareDevice`), as Cancel on the code screen: the
     * executor stops reading for tickets, waits for a ticket it is taking, and cancels every
     * claim it made, so a holder who confirms later binds nothing on this executor. The cancel
     * is local, as `cancelJoin`'s: the identity store is not told and may still accept the
     * holder's confirmation, but this executor never joins through those claims. Answers
     * `{ state: "ended", cancelled }` (the offers of the claims cancelled, none when no ticket
     * came), or `{ state: "already-joined", offerId, did }` when a claim already joined, which
     * a cancel does not undo. The cancel goes on if this call is dropped, and every later call
     * answers what the first one did. 404 for a device string this executor does not know:
     * made before it restarted, an hour past its expiry, or 16 newer device strings ago. The
     * operator on this machine only.
     */
    async endPrepared(device: string): Promise<PreparedEnded> {
        return this.#apiClient.call("identity.endPrepared", { device });
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

    /** This executor's waiting inbox messages, by id and times only. The operator on this machine only. */
    async inbox(): Promise<InboxEntry[]> {
        return this.#apiClient.call("identity.inbox", {});
    }

    /**
     * The custody channel: this computer's joined inbox messages, still sealed to the custody
     * service's own key, with the address each was sealed for (`scid`, `keyId`). The executor
     * opens none; the custody service opens each, checks it against the log, and acknowledges
     * it (`inboxAck`) once stored. Only the custody credential (the first line of the
     * executor's stdin) on a loopback listener of an executor started with
     * `--custody-channel`; any other caller, the admin credential included, gets 403.
     */
    async inboxSealed(): Promise<SealedInboxMessage[]> {
        return this.#apiClient.call("identity.inboxSealed", {});
    }

    /** Deletes inbox message `id` at the store. The operator on this machine only. */
    async inboxAck(id: string): Promise<boolean> {
        return this.#apiClient.call("identity.inboxAck", { id });
    }

    /**
     * The identity's sealed private details record with the store's receipt for it
     * (`{record, receipt}`, `null` before the first write); a reader checks the record's writer
     * at `receipt.seq` (`open_record` in `ad4m-identity-core`). Here and
     * in `recordPut`, `requests` and `deleteRequest`, `request` is a store request the
     * identity's master key signed (a custody service, a restoring app); without one the
     * executor signs with this agent's device key. A post is signed by the device key only.
     */
    async record(request?: SignedRequest): Promise<StoredRecord | null> {
        return this.#apiClient.call("identity.record", { request });
    }

    /** Writes the next version of the sealed record: the version and the store's receipt for it. */
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
     * The custody channel: the main agent's device keys (public keys only), the keys a master
     * key sent to this computer is sealed to. Only the custody credential (the first line of the
     * executor's stdin) on a loopback listener of an executor started with `--custody-channel`;
     * any other caller, the admin credential included, gets 403.
     */
    async custodyStatus(): Promise<DeviceKeys> {
        return this.#apiClient.call("identity.custodyStatus", {});
    }
}
