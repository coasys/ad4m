//! The brake on new logs with no vouching note (decision 21.3; section 14, host admission
//! policy, of `docs-src/identity/did-scid-coasys-1.md`). It runs only for an append of event #0
//! to a log the store does not hold, without a vouching token, and before the store replays any
//! signature of it. In order:
//!
//! 1. the **failure bucket** of the sender's address ([`FAILURES_PER_HOUR`]): a refused request
//!    on this path (a bad proof, bad events) takes a token, and while it is empty every request
//!    from the address gets `429 {scope:"failure"}`, so nobody makes the store replay signatures
//!    without limit;
//! 2. the **global bucket**: `BRAKE_GLOBAL_PER_HOUR` no-note logs store-wide, refilled over an
//!    hour (`429 {scope:"global"}`, and [`GLOBAL_EMPTY_WARNING`] once per empty episode in the
//!    store's log);
//! 3. the **address bucket**: `BRAKE_ADDR_BURST` no-note logs at once per address, refilled
//!    `BRAKE_ADDR_PER_DAY` per 24 h (`429 {scope:"address"}`);
//! 4. the **proof of work** at `POW_BITS` (or the previous bits for [`POW_GRACE_S`] after a
//!    change) over a one-use challenge `GET /v1/pow` mints for it, good for
//!    [`CHALLENGE_TTL_S`] seconds (`403 {error:"pow"}`).
//!
//! Only an admitted log takes a token from the global and the address bucket, in the append's
//! own transaction. Buckets refill continuously; their levels sit in the `brake` table (the
//! level at `updated_at`; a bucket with no row is full).
//!
//! An address is the IPv4 address itself, or the /64 prefix of an IPv6 address (an IPv4 address
//! mapped into IPv6 counts as the IPv4 address). It is the socket's peer, unless that peer is a
//! trusted proxy (`TRUSTED_PROXIES`): then the one header `CLIENT_IP_HEADER` names, the nearest
//! `X-Forwarded-For` entry that is no trusted proxy (the default) or `CF-Connecting-IP`; the
//! other header is never read.

use std::fmt::Write as _;
use std::net::{IpAddr, Ipv6Addr, SocketAddr};
use std::str::FromStr;
use std::sync::Arc;

use ad4m_ldk::identity::PowChallenge;
use axum::extract::connect_info::{ConnectInfo, MockConnectInfo};
use axum::extract::FromRequestParts;
use axum::http::request::Parts;
use axum::http::HeaderMap;
use hmac::{Hmac, Mac};
use rusqlite::{params, Connection, OptionalExtension};
use sha2::Sha256;

use crate::db::{from_sql, to_sql};
use crate::error::ApiError;
use crate::{Settings, Store};

/// Refused requests per address per hour on the brake's path: the failure bucket's size and
/// hourly refill.
pub const FAILURES_PER_HOUR: u32 = 30;
/// How long after a change of `POW_BITS` a proof at the previous bits still passes: 10 minutes.
pub const POW_GRACE_S: u64 = 600;
/// The longest `retryAfter` the brake answers (a bucket that never refills).
pub const MAX_RETRY_S: u64 = 86_400;
/// The address of a request whose socket peer the server did not record.
pub const UNKNOWN_ADDRESS: &str = "unknown";
/// A challenge's life: `GET /v1/pow` mints one per call, good for the next proof for this
/// many seconds.
pub const CHALLENGE_TTL_S: u64 = 300;
/// The distinct text [`Store::notice_global_empty`] logs when a no-note log hits the empty
/// global bucket — once per empty episode — so an alert can match it.
pub const GLOBAL_EMPTY_WARNING: &str = "the global brake is empty";

const HOUR_S: f64 = 3_600.0;
const DAY_S: f64 = 86_400.0;
/// The rounding slack of bucket levels: a refill rate such as 3 a day is no exact `f64`, so a
/// bucket that waited its `retryAfter` may read a hair below a whole token.
const SLACK: f64 = 1e-9;

/// A network range of `TRUSTED_PROXIES`: an address, or `address/prefix`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Cidr {
    addr: IpAddr,
    prefix: u8,
}

impl Cidr {
    /// True when `ip` lies in the range. An IPv4 address mapped into IPv6 is its IPv4 address.
    pub fn contains(&self, ip: IpAddr) -> bool {
        match (self.addr, canonical(ip)) {
            (IpAddr::V4(net), IpAddr::V4(ip)) => {
                let mask = u32::MAX
                    .checked_shl(32 - u32::from(self.prefix))
                    .unwrap_or(0);
                u32::from(net) & mask == u32::from(ip) & mask
            }
            (IpAddr::V6(net), IpAddr::V6(ip)) => {
                let mask = u128::MAX
                    .checked_shl(128 - u32::from(self.prefix))
                    .unwrap_or(0);
                u128::from(net) & mask == u128::from(ip) & mask
            }
            _ => false,
        }
    }
}

impl FromStr for Cidr {
    type Err = String;

    fn from_str(s: &str) -> Result<Cidr, String> {
        let s = s.trim();
        let (addr, prefix) = match s.split_once('/') {
            Some((addr, prefix)) => (addr, Some(prefix)),
            None => (s, None),
        };
        let addr = canonical(
            IpAddr::from_str(addr).map_err(|_| format!("{s} is no IP address or range"))?,
        );
        let max = if addr.is_ipv4() { 32 } else { 128 };
        let prefix = match prefix {
            Some(p) => p
                .parse::<u8>()
                .ok()
                .filter(|p| *p <= max)
                .ok_or_else(|| format!("{s}: the prefix is 0 to {max}"))?,
            None => max,
        };
        Ok(Cidr { addr, prefix })
    }
}

/// `ip` with an IPv4 address mapped into IPv6 (`::ffff:a.b.c.d`) as the IPv4 address.
fn canonical(ip: IpAddr) -> IpAddr {
    match ip {
        IpAddr::V6(v6) => v6
            .to_ipv4_mapped()
            .map(IpAddr::V4)
            .unwrap_or(IpAddr::V6(v6)),
        v4 => v4,
    }
}

/// The address `ip` counts as: an IPv4 address itself, an IPv6 address by its /64 prefix.
pub fn address_of(ip: IpAddr) -> String {
    match canonical(ip) {
        IpAddr::V4(v4) => v4.to_string(),
        IpAddr::V6(v6) => {
            let prefix = u128::from(v6) & (u128::MAX << 64);
            format!("{}/64", Ipv6Addr::from(prefix))
        }
    }
}

/// The one header a trusted proxy fills with the client's address (`CLIENT_IP_HEADER`). Only
/// that header is ever read: a proxy that passes the other one through unchanged would let a
/// caller write its own address.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum ClientIpHeader {
    /// `X-Forwarded-For`, the default: each proxy appends the address it saw.
    #[default]
    XForwardedFor,
    /// `CF-Connecting-IP`, for Cloudflare ingress: Cloudflare sets it to the client's address.
    CfConnectingIp,
}

impl FromStr for ClientIpHeader {
    type Err = String;

    fn from_str(s: &str) -> Result<ClientIpHeader, String> {
        match s.trim().to_ascii_lowercase().as_str() {
            "x-forwarded-for" => Ok(ClientIpHeader::XForwardedFor),
            "cf-connecting-ip" => Ok(ClientIpHeader::CfConnectingIp),
            other => Err(format!(
                "{other} is neither x-forwarded-for nor cf-connecting-ip"
            )),
        }
    }
}

/// The client's IP: the socket's `peer`, unless the peer is one of `trusted`; then what `header`
/// names: the nearest `X-Forwarded-For` entry that is no trusted proxy (each proxy appends the
/// address it saw, so entries left of it are the client's to write), or the one address of
/// `CF-Connecting-IP`. The other header is ignored. The peer itself when that header is
/// missing or garbled.
pub fn client_ip(
    peer: IpAddr,
    headers: &HeaderMap,
    trusted: &[Cidr],
    header: ClientIpHeader,
) -> IpAddr {
    let is_trusted = |ip: IpAddr| trusted.iter().any(|c| c.contains(ip));
    if !is_trusted(peer) {
        return peer;
    }
    match header {
        ClientIpHeader::CfConnectingIp => headers
            .get("cf-connecting-ip")
            .and_then(|v| v.to_str().ok())
            .and_then(|v| IpAddr::from_str(v.trim()).ok())
            .unwrap_or(peer),
        ClientIpHeader::XForwardedFor => headers
            .get_all("x-forwarded-for")
            .iter()
            .filter_map(|v| v.to_str().ok())
            .flat_map(|v| v.split(','))
            .filter_map(|v| IpAddr::from_str(v.trim()).ok())
            .collect::<Vec<_>>()
            .into_iter()
            .rev()
            .find(|ip| !is_trusted(*ip))
            .unwrap_or(peer),
    }
}

/// The sender's address (see [`address_of`], [`client_ip`]), read from the socket peer the
/// server records (`ConnectInfo`) and the request's headers. [`UNKNOWN_ADDRESS`] when the
/// server recorded no peer: all such requests share one address.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Sender(pub String);

impl FromRequestParts<Arc<Store>> for Sender {
    type Rejection = std::convert::Infallible;

    async fn from_request_parts(
        parts: &mut Parts,
        store: &Arc<Store>,
    ) -> Result<Sender, Self::Rejection> {
        let peer = parts
            .extensions
            .get::<ConnectInfo<SocketAddr>>()
            .map(|c| c.0)
            .or_else(|| {
                parts
                    .extensions
                    .get::<MockConnectInfo<SocketAddr>>()
                    .map(|c| c.0)
            });
        // A request that carries proxy headers but passes no trusted proxy reads its client as
        // the socket peer: behind an unlisted proxy that is the proxy's address for every
        // client. Say so once (`Store::note_untrusted_proxy_headers` logs once).
        if store.settings.trusted_proxies.is_empty()
            && (parts.headers.contains_key("cf-connecting-ip")
                || parts.headers.contains_key("x-forwarded-for"))
        {
            store.note_untrusted_proxy_headers();
        }
        Ok(Sender(match peer {
            Some(peer) => address_of(client_ip(
                peer.ip(),
                &parts.headers,
                &store.settings.trusted_proxies,
                store.settings.client_ip_header,
            )),
            None => UNKNOWN_ADDRESS.to_string(),
        }))
    }
}

/// A token bucket: `size` tokens, refilled at `per_s` tokens a second.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Bucket {
    pub size: f64,
    pub per_s: f64,
}

impl Bucket {
    /// The level at `now` of a bucket that held `tokens` at `at`.
    pub fn level(&self, tokens: f64, at: u64, now: u64) -> f64 {
        let elapsed = now.saturating_sub(at) as f64;
        (tokens + elapsed * self.per_s).min(self.size).max(0.0)
    }

    /// Seconds until a bucket at `level` holds a whole token (at least 1, at most
    /// [`MAX_RETRY_S`]).
    pub fn retry_after(&self, level: f64) -> u64 {
        if self.per_s <= 0.0 || self.size < 1.0 {
            return MAX_RETRY_S;
        }
        let seconds = ((1.0 - level) / self.per_s - SLACK).ceil();
        (seconds as u64).clamp(1, MAX_RETRY_S)
    }
}

/// The store-wide bucket of no-note logs.
pub fn global_bucket(settings: &Settings) -> Bucket {
    let n = f64::from(settings.brake_global_per_hour);
    Bucket {
        size: n,
        per_s: n / HOUR_S,
    }
}

/// The bucket of no-note logs of one address.
pub fn address_bucket(settings: &Settings) -> Bucket {
    Bucket {
        size: f64::from(settings.brake_addr_burst),
        per_s: f64::from(settings.brake_addr_per_day) / DAY_S,
    }
}

/// The bucket of refused requests of one address.
pub fn failure_bucket() -> Bucket {
    let n = f64::from(FAILURES_PER_HOUR);
    Bucket {
        size: n,
        per_s: n / HOUR_S,
    }
}

const GLOBAL: &str = "global";

fn address_name(address: &str) -> String {
    format!("address:{address}")
}

fn failure_name(address: &str) -> String {
    format!("failure:{address}")
}

/// The level of the bucket `name` at `now`.
fn level(conn: &Connection, name: &str, bucket: &Bucket, now: u64) -> Result<f64, ApiError> {
    let row: Option<(f64, i64)> = conn
        .query_row(
            "SELECT tokens, updated_at FROM brake WHERE bucket = ?1",
            params![name],
            |r| Ok((r.get(0)?, r.get(1)?)),
        )
        .optional()?;
    Ok(match row {
        Some((tokens, at)) => bucket.level(tokens, from_sql(at), now),
        None => bucket.size,
    })
}

/// Takes one token (or what is left of one) from the bucket `name` at `now`.
fn take(conn: &Connection, name: &str, bucket: &Bucket, now: u64) -> Result<(), ApiError> {
    let left = (level(conn, name, bucket, now)? - 1.0).max(0.0);
    conn.execute(
        "INSERT INTO brake (bucket, tokens, updated_at) VALUES (?1, ?2, ?3)
         ON CONFLICT(bucket) DO UPDATE SET tokens = excluded.tokens, updated_at = excluded.updated_at",
        params![name, left, to_sql(now)?],
    )?;
    Ok(())
}

/// `429 {scope}` when the bucket `name` holds less than a whole token at `now`.
fn require(
    conn: &Connection,
    scope: &str,
    name: &str,
    bucket: &Bucket,
    now: u64,
) -> Result<(), ApiError> {
    let level = level(conn, name, bucket, now)?;
    if level < 1.0 - SLACK {
        return Err(ApiError::rate_limited(scope, bucket.retry_after(level)));
    }
    Ok(())
}

/// `429 {scope:"failure"}` when the failure bucket of `address` is empty: step 1 of the brake,
/// which every new log from an address passes before any signature replays, with a vouching
/// token or without. Takes nothing.
pub(crate) fn check_failure(conn: &Connection, address: &str, now: u64) -> Result<(), ApiError> {
    require(
        conn,
        "failure",
        &failure_name(address),
        &failure_bucket(),
        now,
    )
}

/// Steps 1 to 3 of the brake for a no-note log from `address`: the failure, global and address
/// buckets, each `429` when empty. Takes nothing.
pub(crate) fn check_buckets(
    conn: &Connection,
    settings: &Settings,
    address: &str,
    now: u64,
) -> Result<(), ApiError> {
    check_failure(conn, address, now)?;
    require(conn, "global", GLOBAL, &global_bucket(settings), now)?;
    require(
        conn,
        "address",
        &address_name(address),
        &address_bucket(settings),
        now,
    )
}

/// Takes the admitted log's tokens from the global and the address bucket, in the append's
/// transaction, and drops the rows of address buckets that refilled.
pub(crate) fn admit(
    conn: &Connection,
    settings: &Settings,
    address: &str,
    now: u64,
) -> Result<(), ApiError> {
    take(conn, GLOBAL, &global_bucket(settings), now)?;
    let bucket = address_bucket(settings);
    take(conn, &address_name(address), &bucket, now)?;
    prune(conn, "address:", &bucket, now)
}

/// Takes a token from the failure bucket of `address`, for a refused request, and drops the
/// rows of failure buckets that refilled.
pub(crate) fn charge_failure(conn: &Connection, address: &str, now: u64) -> Result<(), ApiError> {
    let bucket = failure_bucket();
    take(conn, &failure_name(address), &bucket, now)?;
    prune(conn, "failure:", &bucket, now)
}

/// Deletes the rows of the buckets named `prefix…` that are full at `now`: a full bucket and no
/// row are the same, so the table holds only the addresses seen within a refill.
fn prune(conn: &Connection, prefix: &str, bucket: &Bucket, now: u64) -> Result<(), ApiError> {
    conn.execute(
        "DELETE FROM brake WHERE substr(bucket, 1, length(?1)) = ?1
         AND tokens + (?2 - updated_at) * ?3 >= ?4",
        params![prefix, to_sql(now)?, bucket.per_s, bucket.size - SLACK],
    )?;
    Ok(())
}

/// The proof-of-work cost the store runs with, without the per-request challenge:
/// `GET /v1/pow` reports it and [`check_pow`] applies it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PowCost {
    /// The bits a proof must meet.
    pub bits: u8,
    /// The bits before the last change; proofs at them pass for [`POW_GRACE_S`] after
    /// `changed_at`.
    pub previous_bits: u8,
    /// When `bits` last changed (unix seconds).
    pub changed_at: u64,
}

/// The label of each challenge's tag, and of the challenge key's derivation
/// (`StoreKey::pow_challenge_key`): a tag for one never verifies as the other.
const CHALLENGE_LABEL: &str = "coasys:pow-challenge";
/// A challenge's bytes: `expires` (8 big-endian) ‖ 16 random ‖ the 16-byte tag.
const CHALLENGE_LEN: usize = 8 + 16 + 16;
/// The longest base58 text of a challenge's bytes (an overlong text is refused before
/// decoding, whose cost grows quadratically): ceil(40 * log58(256)) = ceil(54.63) = 55.
const CHALLENGE_TEXT_MAX: usize = 55;

type HmacSha256 = Hmac<Sha256>;

/// `HMAC-SHA256(key, CHALLENGE_LABEL ‖ body)`'s first 16 bytes: a challenge's tag.
fn challenge_tag(key: &[u8; 32], body: &[u8]) -> [u8; 16] {
    let mut mac =
        <HmacSha256 as Mac>::new_from_slice(key).expect("an hmac takes a key of any length");
    mac.update(CHALLENGE_LABEL.as_bytes());
    mac.update(body);
    mac.finalize().into_bytes()[..16].try_into().unwrap()
}

/// `GET /v1/pow`'s challenge for the next proof: `expires` ‖ 16 random ‖ the store's tag, as
/// base58btc. Stateless: the tag makes every unspent challenge openable at check time; spending
/// it writes one `pow_seen` row, kept until `expires` (the check's own delete, and `sweep`).
pub(crate) fn challenge(key: &[u8; 32], now: u64) -> PowChallenge {
    let expires = now + CHALLENGE_TTL_S;
    let mut body = [0u8; 8 + 16];
    body[..8].copy_from_slice(&expires.to_be_bytes());
    body[8..].copy_from_slice(&rand::random::<[u8; 16]>());
    let tag = challenge_tag(key, &body);
    let mut token = [0u8; CHALLENGE_LEN];
    token[..body.len()].copy_from_slice(&body);
    token[body.len()..].copy_from_slice(&tag);
    PowChallenge {
        nonce: bs58::encode(token).into_string(),
        expires,
    }
}

/// The `expires` a `nonce` carries when it is one of this store's challenges — the tag
/// verifies under `key` in constant time. Anything else (empty, overlong, no base58, a wrong
/// tag, another store's challenge) is `None`.
fn open_challenge(key: &[u8; 32], nonce: &str) -> Option<u64> {
    if nonce.len() > CHALLENGE_TEXT_MAX {
        return None;
    }
    let bytes: [u8; CHALLENGE_LEN] = bs58::decode(nonce).into_vec().ok()?.try_into().ok()?;
    let (body, tag) = bytes.split_at(CHALLENGE_LEN - 16);
    <HmacSha256 as Mac>::new_from_slice(key)
        .expect("an hmac takes a key of any length")
        .chain_update(CHALLENGE_LABEL.as_bytes())
        .chain_update(body)
        .verify_truncated_left(tag)
        .ok()?;
    u64::from_be_bytes(bytes[..8].try_into().unwrap()).into()
}

/// Step 4: `proof` meets the store's bits (or the previous bits within [`POW_GRACE_S`] of
/// their change) over a fresh challenge of `GET /v1/pow` — unexpired, unforged, unspent — which
/// this call spends with a `pow_seen` row until `expires`. Missing, expired, forged and spent
/// challenges and a proof under another SCID or challenge are all `403 pow`. Runs inside the
/// append's transaction, so a challenge whose publish is later refused (the events fail the
/// fold) is freed for the retry.
pub(crate) fn check_pow(
    conn: &Connection,
    scid: &str,
    proof: Option<&ad4m_ldk::identity::Pow>,
    params: &PowCost,
    key: &[u8; 32],
    now: u64,
) -> Result<(), ApiError> {
    let refused = || {
        ApiError::forbidden(
            "pow",
            "a new log with no vouching note needs a proof of work at the store's bits over a \
             fresh challenge of GET /v1/pow",
        )
    };
    let proof = proof.ok_or_else(refused)?;
    let Some(expires) = open_challenge(key, &proof.challenge).filter(|e| *e > now) else {
        return Err(refused());
    };
    let proof = ad4m_identity_core::pow::Pow {
        nonce: proof.nonce.clone(),
        bits: proof.bits,
        challenge: proof.challenge.clone(),
    };
    let at_bits = ad4m_identity_core::pow::check(scid, &proof, params.bits);
    let in_grace = now < params.changed_at.saturating_add(POW_GRACE_S)
        && ad4m_identity_core::pow::check(scid, &proof, params.previous_bits);
    if !(at_bits || in_grace) {
        return Err(refused());
    }
    // One use: the row stays until the challenge would have expired anyway. A proof that
    // passes but finds its challenge's row is someone else's already-spent challenge.
    conn.execute(
        "DELETE FROM pow_seen WHERE expires <= ?1",
        params![to_sql(now)?],
    )?;
    let spent = conn.execute(
        "INSERT OR IGNORE INTO pow_seen (nonce, expires) VALUES (?1, ?2)",
        params![proof.challenge, to_sql(expires)?],
    )?;
    if spent == 0 {
        return Err(refused());
    }
    Ok(())
}

/// The proof-of-work cost the store runs with: `bits` from the settings; when they differ
/// from the bits the database last ran with, those become `previousBits` and `changedAt` is
/// now. The first start records `previousBits = bits`, `changedAt = now`.
pub(crate) fn pow_params(conn: &Connection, bits: u8, now: u64) -> Result<PowCost, ApiError> {
    let row: Option<(i64, i64, i64)> = conn
        .query_row(
            "SELECT bits, previous_bits, changed_at FROM pow WHERE id = 1",
            [],
            |r| Ok((r.get(0)?, r.get(1)?, r.get(2)?)),
        )
        .optional()?;
    let params = match row {
        Some((stored, previous, at)) if stored == i64::from(bits) => {
            return Ok(PowCost {
                bits,
                previous_bits: u8::try_from(previous).unwrap_or(bits),
                changed_at: from_sql(at),
            })
        }
        Some((stored, _, _)) => PowCost {
            bits,
            previous_bits: u8::try_from(stored).unwrap_or(bits),
            changed_at: now,
        },
        None => PowCost {
            bits,
            previous_bits: bits,
            changed_at: now,
        },
    };
    conn.execute(
        "INSERT INTO pow (id, bits, previous_bits, changed_at) VALUES (1, ?1, ?2, ?3)
         ON CONFLICT(id) DO UPDATE SET bits = excluded.bits, previous_bits = excluded.previous_bits,
                                       changed_at = excluded.changed_at",
        params![
            i64::from(params.bits),
            i64::from(params.previous_bits),
            to_sql(params.changed_at)?
        ],
    )?;
    Ok(params)
}

/// `GET /v1/metrics`: the brake's levels at `now`, in plain text: the global bucket, and for the
/// address and the failure buckets how many are in use (not full), how many are empty and the
/// lowest level. No address appears: the route is public.
pub(crate) fn metrics(
    conn: &Connection,
    settings: &Settings,
    now: u64,
) -> Result<String, ApiError> {
    let global = global_bucket(settings);
    let mut out = String::new();
    let _ = writeln!(out, "# identity-store brake: tokens left");
    let _ = writeln!(
        out,
        "global {:.2} of {} (refills {} an hour)",
        level(conn, GLOBAL, &global, now)?,
        settings.brake_global_per_hour,
        settings.brake_global_per_hour
    );
    let kinds = [
        (
            "address",
            address_bucket(settings),
            settings.brake_addr_burst,
            format!("{} a day", settings.brake_addr_per_day),
        ),
        (
            "failure",
            failure_bucket(),
            FAILURES_PER_HOUR,
            format!("{FAILURES_PER_HOUR} an hour"),
        ),
    ];
    for (kind, bucket, of, refill) in kinds {
        let mut stmt = conn.prepare(
            "SELECT tokens, updated_at FROM brake WHERE substr(bucket, 1, length(?1)) = ?1",
        )?;
        let levels = stmt
            .query_map(params![format!("{kind}:")], |r| {
                Ok((r.get::<_, f64>(0)?, r.get::<_, i64>(1)?))
            })?
            .map(|row| row.map(|(tokens, at)| bucket.level(tokens, from_sql(at), now)))
            .collect::<Result<Vec<f64>, _>>()?;
        let in_use: Vec<f64> = levels
            .into_iter()
            .filter(|l| *l < bucket.size - SLACK)
            .collect();
        let empty = in_use.iter().filter(|l| **l < 1.0 - SLACK).count();
        let lowest = in_use.iter().copied().fold(bucket.size, f64::min);
        let _ = writeln!(
            out,
            "{kind} buckets: {} in use, {empty} empty, lowest {lowest:.2} of {of} (refill {refill})",
            in_use.len()
        );
    }
    Ok(out)
}

#[cfg(test)]
mod tests {
    use axum::http::HeaderValue;

    use super::*;

    fn ip(s: &str) -> IpAddr {
        s.parse().unwrap()
    }

    #[test]
    fn ipv4_counts_as_itself_and_ipv6_by_its_64() {
        assert_eq!(address_of(ip("203.0.113.7")), "203.0.113.7");
        assert_eq!(
            address_of(ip("2001:db8:1:2:aaaa:bbbb:cccc:dddd")),
            "2001:db8:1:2::/64"
        );
        assert_eq!(
            address_of(ip("2001:db8:1:2::1")),
            address_of(ip("2001:db8:1:2:ffff:ffff:ffff:ffff"))
        );
        assert_ne!(
            address_of(ip("2001:db8:1:2::1")),
            address_of(ip("2001:db8:1:3::1"))
        );
        // IPv4 mapped into IPv6 is the IPv4 address.
        assert_eq!(address_of(ip("::ffff:203.0.113.7")), "203.0.113.7");
    }

    #[test]
    fn ranges_parse_and_match() {
        let net: Cidr = "10.0.0.0/8".parse().unwrap();
        assert!(net.contains(ip("10.200.1.1")));
        assert!(!net.contains(ip("11.0.0.1")));
        assert!(net.contains(ip("::ffff:10.0.0.1")));
        let one: Cidr = "192.0.2.1".parse().unwrap();
        assert!(one.contains(ip("192.0.2.1")));
        assert!(!one.contains(ip("192.0.2.2")));
        let v6: Cidr = "2001:db8::/32".parse().unwrap();
        assert!(v6.contains(ip("2001:db8:ffff::1")));
        assert!(!v6.contains(ip("2001:db9::1")));
        assert!(!v6.contains(ip("10.0.0.1")));
        let all: Cidr = "0.0.0.0/0".parse().unwrap();
        assert!(all.contains(ip("8.8.8.8")));
        assert!("10.0.0.0/33".parse::<Cidr>().is_err());
        assert!("not-an-ip".parse::<Cidr>().is_err());
    }

    fn headers(pairs: &[(&'static str, &str)]) -> HeaderMap {
        let mut h = HeaderMap::new();
        for (name, value) in pairs {
            h.append(*name, HeaderValue::from_str(value).unwrap());
        }
        h
    }

    #[test]
    fn the_client_address_comes_from_the_named_header_only_behind_a_trusted_proxy() {
        use ClientIpHeader::{CfConnectingIp, XForwardedFor};
        let trusted: Vec<Cidr> = vec!["10.0.0.0/8".parse().unwrap()];
        let h = headers(&[
            ("cf-connecting-ip", "198.51.100.9"),
            ("x-forwarded-for", "192.0.2.5"),
        ]);
        // Not from a trusted proxy: the headers are ignored.
        for header in [XForwardedFor, CfConnectingIp] {
            assert_eq!(
                client_ip(ip("203.0.113.1"), &h, &trusted, header),
                ip("203.0.113.1")
            );
        }
        // From one: only the named header counts, whatever the other says.
        assert_eq!(
            client_ip(ip("10.1.1.1"), &h, &trusted, XForwardedFor),
            ip("192.0.2.5")
        );
        assert_eq!(
            client_ip(ip("10.1.1.1"), &h, &trusted, CfConnectingIp),
            ip("198.51.100.9")
        );
        // A client-written CF header does not move an X-Forwarded-For setting, and the reverse.
        let h = headers(&[("cf-connecting-ip", "198.51.100.9")]);
        assert_eq!(
            client_ip(ip("10.1.1.1"), &h, &trusted, XForwardedFor),
            ip("10.1.1.1")
        );
        let h = headers(&[("x-forwarded-for", "192.0.2.5")]);
        assert_eq!(
            client_ip(ip("10.1.1.1"), &h, &trusted, CfConnectingIp),
            ip("10.1.1.1")
        );
        // The nearest X-Forwarded-For entry that is no trusted proxy.
        let h = headers(&[("x-forwarded-for", "1.2.3.4, 192.0.2.5, 10.9.9.9")]);
        assert_eq!(
            client_ip(ip("10.1.1.1"), &h, &trusted, XForwardedFor),
            ip("192.0.2.5")
        );
        let h = headers(&[
            ("x-forwarded-for", "1.2.3.4"),
            ("x-forwarded-for", "192.0.2.6"),
        ]);
        assert_eq!(
            client_ip(ip("10.1.1.1"), &h, &trusted, XForwardedFor),
            ip("192.0.2.6")
        );
        // A garbled header: the peer.
        let h = headers(&[
            ("cf-connecting-ip", "nonsense"),
            ("x-forwarded-for", "junk"),
        ]);
        for header in [XForwardedFor, CfConnectingIp] {
            assert_eq!(
                client_ip(ip("10.1.1.1"), &h, &trusted, header),
                ip("10.1.1.1")
            );
        }
        // No proxies trusted: always the peer.
        let h = headers(&[("cf-connecting-ip", "198.51.100.9")]);
        assert_eq!(
            client_ip(ip("10.1.1.1"), &h, &[], CfConnectingIp),
            ip("10.1.1.1")
        );
    }

    #[test]
    fn a_bucket_refills_continuously_up_to_its_size() {
        // 3 a day, 3 at once.
        let b = Bucket {
            size: 3.0,
            per_s: 3.0 / DAY_S,
        };
        assert_eq!(b.level(0.0, 100, 100), 0.0);
        // A third of a day earns one token.
        assert!((b.level(0.0, 0, 28_800) - 1.0).abs() < 1e-9);
        assert!((b.level(0.5, 0, 14_400) - 1.0).abs() < 1e-9);
        // Never past its size, never below zero, and a clock that went back earns nothing.
        assert_eq!(b.level(2.0, 0, 10 * 86_400), 3.0);
        assert_eq!(b.level(-1.0, 50, 50), 0.0);
        assert_eq!(b.level(1.0, 500, 100), 1.0);
        // The wait for a whole token.
        assert_eq!(b.retry_after(0.0), 28_800);
        assert_eq!(b.retry_after(0.5), 14_400);
        assert_eq!(b.retry_after(0.9999999), 1);
        // 120 an hour: one token every 30 s.
        let g = Bucket {
            size: 120.0,
            per_s: 120.0 / HOUR_S,
        };
        assert_eq!(g.retry_after(0.0), 30);
        // A bucket that never refills.
        let never = Bucket {
            size: 3.0,
            per_s: 0.0,
        };
        assert_eq!(never.retry_after(0.0), MAX_RETRY_S);
    }

    #[test]
    fn take_and_level_round_trip_through_the_table() {
        let conn = Connection::open_in_memory().unwrap();
        conn.execute_batch(crate::db::SCHEMA).unwrap();
        let b = Bucket {
            size: 3.0,
            per_s: 3.0 / DAY_S,
        };
        assert_eq!(level(&conn, "address:x", &b, 0).unwrap(), 3.0);
        for _ in 0..3 {
            take(&conn, "address:x", &b, 0).unwrap();
        }
        assert_eq!(level(&conn, "address:x", &b, 0).unwrap(), 0.0);
        assert!(require(&conn, "address", "address:x", &b, 0).is_err());
        assert!((level(&conn, "address:x", &b, 28_800).unwrap() - 1.0).abs() < 1e-9);
        assert!(require(&conn, "address", "address:x", &b, 28_800).is_ok());
    }

    #[test]
    fn pow_params_record_a_change_of_bits() {
        let conn = Connection::open_in_memory().unwrap();
        conn.execute_batch(crate::db::SCHEMA).unwrap();
        let first = pow_params(&conn, 22, 100).unwrap();
        assert_eq!(
            (first.bits, first.previous_bits, first.changed_at),
            (22, 22, 100)
        );
        // The same bits again: unchanged.
        let again = pow_params(&conn, 22, 900).unwrap();
        assert_eq!(again, first);
        // Raised: the old bits are previous, changed now.
        let raised = pow_params(&conn, 24, 1_000).unwrap();
        assert_eq!(
            (raised.bits, raised.previous_bits, raised.changed_at),
            (24, 22, 1_000)
        );
        assert_eq!(pow_params(&conn, 24, 5_000).unwrap(), raised);
    }
}
