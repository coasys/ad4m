import { ApiClient } from "../apiClient";
import type { CreateInviteResult } from "../generated/api/CreateInviteResult";
import type { HostInfo } from "../generated/api/HostInfo";
import type { HostJoinStatus } from "../generated/api/HostJoinStatus";
import type { HostPolicyPatch } from "../generated/api/HostPolicyPatch";
import type { HostPolicyView } from "../generated/api/HostPolicyView";
import type { InviteView } from "../generated/api/InviteView";
import type { JoinResult } from "../generated/api/JoinResult";
import type { LeaveResult } from "../generated/api/LeaveResult";
import type { RemoveTenantResult } from "../generated/api/RemoveTenantResult";
import type { TenantUsage } from "../generated/api/TenantUsage";
import type { TenantView } from "../generated/api/TenantView";

/**
 * An executor as a host: a person joins it as a tenant with their `did:scid` identity, uses it,
 * and leaves keeping their data or deleting it.
 *
 * `info`, `join` and `joinStatus` are open to anyone. `usage` and `leave` are the tenant's own
 * (the session `joinStatus` reports active, logged in with its key). The policy, invite and
 * tenant calls are the operator's.
 */
export class HostClient {
    #apiClient: ApiClient;

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    }

    /** What anyone may know of the host: who may sign up, its name, whether it is full. */
    async info(): Promise<HostInfo> {
        return this.#apiClient.call("host.info", {});
    }

    /**
     * Starts a join. `offer` is the `coasys:offer:1:` string the person's identity made for this
     * host's device key; `inviteSecret` is the invite link's fragment when the host asks for one.
     * Answers at once with a handle: poll `joinStatus` until `active`.
     */
    async join(didScid: string, offer: string, inviteSecret?: string): Promise<JoinResult> {
        return this.#apiClient.call("host.join", {
            didScid,
            offer,
            inviteSecret: inviteSecret ?? null,
        });
    }

    /** `pending`, `active`, `expired` or `failed` for a join handle. */
    async joinStatus(joinHandle: string): Promise<HostJoinStatus> {
        return this.#apiClient.call("host.joinStatus", { joinHandle });
    }

    /** The calling tenant's usage. */
    async usage(): Promise<TenantUsage> {
        return this.#apiClient.call("host.usage", {});
    }

    /** The calling tenant leaves; `keepData` false destroys its data key. */
    async leave(keepData: boolean): Promise<LeaveResult> {
        return this.#apiClient.call("host.leave", { keepData });
    }

    /** The sign-up policy (operator). */
    async policy(): Promise<HostPolicyView> {
        return this.#apiClient.call("host.policy", {});
    }

    /** Changes the policy; a field left out stays (operator). */
    async setPolicy(patch: HostPolicyPatch): Promise<HostPolicyView> {
        return this.#apiClient.call("host.setPolicy", patch);
    }

    /** Makes an invite; its secret is shown once (operator). */
    async createInvite(expiresInDays?: number, note?: string): Promise<CreateInviteResult> {
        return this.#apiClient.call("host.createInvite", {
            expiresInDays: expiresInDays ?? null,
            note: note ?? null,
        });
    }

    /** The invites, never their secrets (operator). */
    async listInvites(): Promise<InviteView[]> {
        return this.#apiClient.call("host.listInvites", {});
    }

    /** Revokes an open invite (operator). */
    async revokeInvite(inviteId: string): Promise<boolean> {
        return this.#apiClient.call("host.revokeInvite", { inviteId });
    }

    /** The tenants that are not deleted (operator). */
    async tenants(): Promise<TenantView[]> {
        return this.#apiClient.call("host.tenants", {});
    }

    /** Removes a detached tenant's record and its kept data (operator). */
    async removeTenant(tenantId: string): Promise<RemoveTenantResult> {
        return this.#apiClient.call("host.removeTenant", { tenantId });
    }
}
