//! A test person's apps over JSON lines, for the agent language suites (AR-01 8.5): the
//! TypeScript language's tests and the Rust port's tests drive it to make real key-log events,
//! offers, claims, signed requests and agent records with the identity core, then send them
//! through the language to a real store.
//!
//! ```text
//! cargo run -q -p identity-store --example test_agent
//! ```
//!
//! Each request is one JSON line `{"op": …, …}`; each answer is one line, `{"ok": value}` or
//! `{"error": text}`. Keys come from names (`sha256(name)`), so a run is repeatable.
//!
//! | op | fields | answer |
//! |---|---|---|
//! | `person` | `name` | `{did, scid, incept}`: the person's #0 (master `m0`, device `d1`) |
//! | `device` | `label` | `{sign, enc, did}` of a device key not yet in a log |
//! | `offer` | `name`, `kind`, `ttl`, `now`, `bound?` (a device label), `bot?` | `{offer, s}` |
//! | `claim` | `name`, `device`, `offerId`, `s`, `canKeepMaster?` | the `Claim` |
//! | `add` | `name`, `offer`, `claim`, `claimedAt`, `expires?` | the `add` event (not pushed) |
//! | `revoke` | `name`, `ids` | the `revoke` event (not pushed) |
//! | `rotate` | `name` | the `rotate` event (not pushed) |
//! | `push` | `name`, `event` | `{seq}`: the person's copy takes the event the store took |
//! | `request` | `name`, `signer` (`master`, `d1` or a device label), `keyId?`, `method`, `path`, `body`, `expires` | the `SignedRequest` |
//! | `record` | `name`, `signer`, `keyId`, `profile`, `version`, `did?`, `anchor?` | a `did:scid` `AgentRecord` (`did`/`anchor` replace the person's, re-signed: a hostile store's row) |
//! | `didKeyRecord` | `device`, `profile`, `version` | a `did:key` `AgentRecord` signed by that key |

#[path = "../tests/common/agent.rs"]
mod agent;

use std::collections::HashMap;
use std::io::{BufRead, Write};

use ad4m_identity_core::link::{Claim, ClaimView, OfferKind, SignedOffer};
use ad4m_identity_core::{BotScope, Event};
use agent::{did_key, did_key_record, enc_of, Device, Person};
use ed25519_dalek::SigningKey;
use serde_json::{json, Value};

#[derive(Default)]
struct Apps {
    people: HashMap<String, Person>,
    devices: HashMap<String, Device>,
}

fn field<'a>(req: &'a Value, name: &str) -> Result<&'a Value, String> {
    req.get(name).ok_or_else(|| format!("missing field {name}"))
}

fn text<'a>(req: &'a Value, name: &str) -> Result<&'a str, String> {
    field(req, name)?
        .as_str()
        .ok_or_else(|| format!("{name} is a string"))
}

fn number(req: &Value, name: &str) -> Result<u64, String> {
    field(req, name)?
        .as_u64()
        .ok_or_else(|| format!("{name} is a whole number"))
}

fn parse<T: serde::de::DeserializeOwned>(req: &Value, name: &str) -> Result<T, String> {
    serde_json::from_value(field(req, name)?.clone()).map_err(|e| format!("{name}: {e}"))
}

fn to_json<T: serde::Serialize>(value: &T) -> Result<Value, String> {
    serde_json::to_value(value).map_err(|e| e.to_string())
}

impl Apps {
    fn person(&mut self, req: &Value) -> Result<&mut Person, String> {
        let name = text(req, "name")?;
        self.people
            .get_mut(name)
            .ok_or_else(|| format!("no person {name}"))
    }

    fn device(&mut self, label: &str) -> &Device {
        self.devices
            .entry(label.to_string())
            .or_insert_with(|| Device::new(label))
    }

    fn signer(&mut self, req: &Value) -> Result<SigningKey, String> {
        let signer = text(req, "signer")?.to_string();
        if signer == "master" {
            return Ok(self.person(req)?.current_master());
        }
        if let Some(n) = signer.strip_prefix('d').and_then(|n| n.parse::<u32>().ok()) {
            return Ok(self.person(req)?.device_key(n));
        }
        Ok(self.device(&signer).key.clone())
    }

    fn handle(&mut self, req: &Value) -> Result<Value, String> {
        match text(req, "op")? {
            "person" => {
                let name = text(req, "name")?.to_string();
                let person = Person::new(&name);
                let answer = json!({
                    "did": person.did(),
                    "scid": person.scid(),
                    "incept": person.json(0),
                });
                self.people.insert(name, person);
                Ok(answer)
            }
            "device" => {
                let device = self.device(text(req, "label")?);
                Ok(json!({
                    "sign": device.sign_key(),
                    "enc": enc_of(&device.key),
                    "did": did_key(&device.key),
                }))
            }
            "offer" => {
                let kind: OfferKind = parse(req, "kind")?;
                let expires = number(req, "now")? + number(req, "ttl")?;
                let bound = req
                    .get("bound")
                    .and_then(Value::as_str)
                    .map(|label| self.device(label).sign_key());
                let bot: Option<BotScope> = match req.get("bot") {
                    Some(v) if !v.is_null() => Some(parse(req, "bot")?),
                    _ => None,
                };
                let (offer, s) = self.person(req)?.offer(kind, bound, bot, expires);
                Ok(json!({ "offer": offer, "s": bs58::encode(s).into_string() }))
            }
            "claim" => {
                let scid = self.person(req)?.scid();
                let s: [u8; 32] = bs58::decode(text(req, "s")?)
                    .into_vec()
                    .map_err(|e| e.to_string())?
                    .try_into()
                    .map_err(|_| "s is 32 bytes".to_string())?;
                let offer_id = text(req, "offerId")?.to_string();
                let keep = req
                    .get("canKeepMaster")
                    .and_then(Value::as_bool)
                    .unwrap_or(false);
                let device = self.device(text(req, "device")?);
                to_json(&device.claim(&scid, &offer_id, s, keep))
            }
            "add" => {
                let offer: SignedOffer = parse(req, "offer")?;
                let claim: Claim = parse(req, "claim")?;
                let view: ClaimView =
                    ad4m_identity_core::link::claim_view(&claim, number(req, "claimedAt")?);
                let expires = req.get("expires").and_then(Value::as_u64);
                let person = self.person(req)?;
                to_json(&person.add_claimed(&offer.offer, &view, expires))
            }
            "revoke" => {
                let ids: Vec<String> = parse(req, "ids")?;
                let ids: Vec<&str> = ids.iter().map(String::as_str).collect();
                to_json(&self.person(req)?.revoke(&ids))
            }
            "rotate" => to_json(&self.person(req)?.rotate(&[])),
            "push" => {
                let event: Event = parse(req, "event")?;
                let person = self.person(req)?;
                person.push(event);
                Ok(json!({ "seq": person.head_seq() }))
            }
            "request" => {
                let signer = self.signer(req)?;
                let person = self.person(req)?;
                let key_id = match req.get("keyId").and_then(Value::as_str) {
                    Some(id) => id.to_string(),
                    None => person.master_id(),
                };
                let body = person.request_body(
                    &key_id,
                    text(req, "method")?,
                    text(req, "path")?,
                    text(req, "body")?.as_bytes(),
                    number(req, "expires")?,
                );
                let header = Person::sign_request(body, &signer);
                let signed = identity_store::request::decode_header(&header)
                    .map_err(|e| e.body.to_string())?;
                to_json(&signed)
            }
            "record" => {
                let signer = self.signer(req)?;
                let key_id = text(req, "keyId")?.to_string();
                let profile = field(req, "profile")?.clone();
                let version = number(req, "version")?;
                let person = self.person(req)?;
                let mut record = person.agent_record(&key_id, &signer, profile, version);
                // A hostile store's row for the suites: another did or anchor, re-signed.
                if let Some(did) = req.get("did").and_then(Value::as_str) {
                    record.did = did.to_string();
                }
                if req.get("anchor").is_some() {
                    record.anchor = Some(parse(req, "anchor")?);
                }
                record.sig = agent::sign_record(&record, &signer);
                to_json(&record)
            }
            "didKeyRecord" => {
                let profile = field(req, "profile")?.clone();
                let version = number(req, "version")?;
                let device = self.device(text(req, "device")?);
                to_json(&did_key_record(&device.key, profile, version))
            }
            other => Err(format!("unknown op {other}")),
        }
    }
}

fn main() {
    let mut apps = Apps::default();
    let stdin = std::io::stdin();
    let mut stdout = std::io::stdout().lock();
    for line in stdin.lock().lines() {
        let Ok(line) = line else { break };
        if line.trim().is_empty() {
            continue;
        }
        let answer = match serde_json::from_str::<Value>(&line) {
            Ok(req) => match apps.handle(&req) {
                Ok(value) => json!({ "ok": value }),
                Err(error) => json!({ "error": error }),
            },
            Err(e) => json!({ "error": format!("not JSON: {e}") }),
        };
        if writeln!(stdout, "{answer}")
            .and_then(|_| stdout.flush())
            .is_err()
        {
            break;
        }
    }
}
