# Guests

A **guest** is a web visitor with no identity: a managed agent of kind `Guest` on a host that admits guests, holding only its own device key, which no key log lists (`author_of` answers `NoLog`). Its name derives from that key ("Guest 4521"). A guest enters a neighbourhood only with a guest-role invite, in a neighbourhood whose rules say `guests: true`, and its links show under its name but count nowhere. A guest ends when the operator removes it, or at **"Keep this"**, which makes the guest's key the first device key of a new identity.

**Why:** A person who receives a link on the web can take part before making an identity. The name comes from the key, so every executor and the link server's operator view show the same name with no stored profile. "Keep this" reuses the guest's key, so `author_of` maps its earlier links to the new person with no rewrite. Guests have no password: the older guest login made a random email address and password per guest. Guests have no limits and no expiry; the options that lost were a creation rate limit, a cap on live guests and expiry after 30 idle days.

**Parent:** [Membership](README.md)

<details><summary>How it works</summary>

1. `guest.create` (no session; refused unless the host's sign-up setting is "anyone", `HostInfo.admitsGuests`) makes the managed agent with its own wallet key and a `GuestRecord { did_key, display_name, created_at, last_active_at, spaces }`, and returns the token of a grant with `client: Guest` ([Sessions](../../../agents/access/sessions.md)). No email, no password. Guests made by the older guest login are ordinary users with a stored email address and password, marked `Guest` by their address at the first start; the guest branch of `user.login` serves only them, until each is removed or claimed.
2. The guest joins with the invite secret: its executor signs `membership:guest` with the invite proof ([Open joins and invites](invites.md)). An open neighbourhood without a guest invite does not admit a guest.
3. The guest's session holds the user capability on its own perspectives and the neighbourhoods it joined, and `guest.status` for itself. The operator lists guests with `hosting.guests` and their last activity.
4. `hosting.removeGuest { didKey }` signs the guest's own `removed` link in each neighbourhood it joined, then deletes its managed agent, sessions, grant, private perspectives and key, and sends `guest-removed`. Its links in neighbourhoods stay, under its `did:key`.
5. "Keep this", the executor's half: the new identity's first event lists the guest's key with no expiry; the host co-signs it (`identity.cosignIncept`) and ingests it (`identity.publish`). The executor records the `did:scid` for the agent, changes its kind to `Person`, deletes the `GuestRecord` and ends the guest grant. In an open neighbourhood it writes the person's own member link; in an invite-only neighbourhood the person stays a guest until someone with the add right adds them. The host then serves the person as a tenant when its sign-up setting admits them, or holds their data until they pick another host ([Tenants](../../../agents/executor/managed-agents/tenants.md)).

</details>

<details><summary>Where in the code</summary>

- `UserKind { Person, Guest }` and `GuestRecord` on the executor's managed agents; the design names no file.
- `guest.create`, `guest.status`, `hosting.guests`, `hosting.removeGuest`: [api/users_ws.rs](../../../../rust-executor/src/api/users_ws.rs) and [api/hosting_ws.rs](../../../../rust-executor/src/api/hosting_ws.rs) hold the user and hosting methods.
- `identity.cosignIncept`, `identity.publish`: [identity/](../../../../rust-executor/src/identity) (new).
- SDK: guest entry in [connect/](../../../../connect).

</details>
