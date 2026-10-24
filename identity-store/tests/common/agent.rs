//! A test person: fixed keys from a name, a key log built with the identity core's builders,
//! and the offers, claims, signed requests and agent records that the person's apps make.
//! The store tests use it directly; `examples/test_agent.rs` serves it over JSON lines to the
//! agent language suites.
#![allow(dead_code)]

use ad4m_hash_chain::{encode_ed25519_multikey, encode_proof_value, hash_bytes, sha256};
use ad4m_identity_core::build::{self, Signer};
use ad4m_identity_core::link::{
    claim_view, make_claim, offer_commit, sign_offer, Claim, ClaimView, Offer, OfferKind, SignedBy,
    SignedOffer,
};
use ad4m_identity_core::{
    commitment, did_of, encode_x25519_multikey, fold, signing_bytes, BotScope, DeviceKey, Event,
    KelChain, KelPolicy, KeyLife, Op,
};
use ad4m_ldk::identity::{AgentRecord, Anchor, RequestBody, SignedRequest};
use ed25519_dalek::SigningKey;
use identity_store::agents::{record_value, RECORD_DOMAIN};
use identity_store::request::{encode_header, REQUEST_DOMAIN};
use serde_json::Value;

/// A key from a label: `sha256(label)` as the Ed25519 secret.
pub fn key(label: &str) -> SigningKey {
    SigningKey::from_bytes(&sha256(label.as_bytes()))
}

/// The signing multikey (`z6Mk…`) of `key`.
pub fn multikey(key: &SigningKey) -> String {
    encode_ed25519_multikey(key.verifying_key().as_bytes())
}

/// The X25519 multikey of `key` (its Montgomery form).
pub fn enc_of(key: &SigningKey) -> String {
    encode_x25519_multikey(&key.verifying_key().to_montgomery().to_bytes())
}

/// `did:key` of `key`.
pub fn did_key(key: &SigningKey) -> String {
    format!("did:key:{}", multikey(key))
}

/// A device that is not yet in any log: its key, and the claims it makes.
pub struct Device {
    pub key: SigningKey,
}

impl Device {
    pub fn new(label: &str) -> Device {
        Device {
            key: key(&format!("device {label}")),
        }
    }

    pub fn sign_key(&self) -> String {
        multikey(&self.key)
    }

    /// A claim on `offer_id` of `scid` with the offer's secret `s`.
    pub fn claim(&self, scid: &str, offer_id: &str, s: [u8; 32], can_keep_master: bool) -> Claim {
        make_claim(
            scid,
            offer_id,
            s,
            &self.key,
            enc_of(&self.key),
            can_keep_master,
            Some("Test device".into()),
        )
        .unwrap()
    }

    /// What the holder reads of this device's claim.
    pub fn view(&self, claim: &Claim, at: u64) -> ClaimView {
        claim_view(claim, at)
    }
}

fn person_master(name: &str, n: u32) -> SigningKey {
    key(&format!("{name} master {n}"))
}

fn person_salt(name: &str, n: u32) -> String {
    bs58::encode(&sha256(format!("{name} salt {n}").as_bytes())[..16]).into_string()
}

fn person_next(name: &str, n: u32) -> String {
    commitment(&multikey(&person_master(name, n)), &person_salt(name, n))
}

fn person_device_key(name: &str, n: u32) -> SigningKey {
    key(&format!("{name} device {n}"))
}

/// A person's identity with its key log as the person's apps hold it.
pub struct Person {
    pub name: String,
    pub events: Vec<Event>,
    pub chain: KelChain,
    /// The number of the current master key.
    pub n: u32,
    /// Offers made so far (gives each a fresh id and secret).
    offers: u32,
}

impl Person {
    /// Event #0 with master 0 and one standing device key `d1`, co-signed by `d1`.
    pub fn new(name: &str) -> Person {
        let d1 = person_device_key(name, 1);
        let device = DeviceKey {
            id: "d1".into(),
            sign: multikey(&d1),
            enc: enc_of(&d1),
            life: KeyLife::Standing,
        };
        let mut event =
            build::incept_human(&person_master(name, 0), person_next(name, 1), vec![device])
                .unwrap();
        build::cosign(&mut event, "d1", &d1).unwrap();
        Person {
            name: name.to_string(),
            chain: fold(std::slice::from_ref(&event)).unwrap(),
            events: vec![event],
            n: 0,
            offers: 0,
        }
    }

    pub fn master(&self, n: u32) -> SigningKey {
        person_master(&self.name, n)
    }

    pub fn salt(&self, n: u32) -> String {
        person_salt(&self.name, n)
    }

    pub fn next(&self, n: u32) -> String {
        person_next(&self.name, n)
    }

    /// The key that #0 lists as `d<n>`.
    pub fn device_key(&self, n: u32) -> SigningKey {
        person_device_key(&self.name, n)
    }

    pub fn scid(&self) -> String {
        self.chain.id().clone()
    }

    pub fn did(&self) -> String {
        did_of(&self.scid())
    }

    pub fn current_master(&self) -> SigningKey {
        self.master(self.n)
    }

    pub fn master_id(&self) -> String {
        self.chain.state().current_master().unwrap().id.clone()
    }

    pub fn head_seq(&self) -> u64 {
        self.chain.head().seq
    }

    /// The JSON of event `seq`.
    pub fn json(&self, seq: usize) -> Value {
        serde_json::to_value(&self.events[seq]).unwrap()
    }

    /// Appends `event` to the person's own copy (after the store took it). A `rotate` moves
    /// the current master on.
    pub fn push(&mut self, event: Event) {
        self.chain.append(&KelPolicy, &event).unwrap();
        if matches!(event.op, Op::Rotate { .. }) {
            self.n += 1;
        }
        self.events.push(event);
    }

    /// Where the current master signs from: its id and the head.
    pub fn signed_by(&self) -> SignedBy {
        let head = self.chain.head();
        SignedBy {
            key_id: self.master_id(),
            kel_seq: head.seq,
            kel_hash: head.hash.clone(),
        }
    }

    /// A fresh offer of `kind`, signed by the current master at the head. Answers the signed
    /// offer and its secret `s`.
    pub fn offer(
        &mut self,
        kind: OfferKind,
        bound: Option<String>,
        bot: Option<BotScope>,
        expires: u64,
    ) -> (SignedOffer, [u8; 32]) {
        self.offers += 1;
        let tag = format!("{} offer {}", self.name, self.offers);
        let s = sha256(format!("{tag} secret").as_bytes());
        let offer_id = bs58::encode(&sha256(format!("{tag} id").as_bytes())[..16]).into_string();
        let offer = Offer {
            scid: self.scid(),
            offer_id,
            kind,
            commit: offer_commit(&s),
            expires,
            device_key: bound,
            bot,
        };
        let signed = sign_offer(offer, self.signed_by(), &self.current_master()).unwrap();
        (signed, s)
    }

    /// The `add` that spends `claim` on `offer` (not pushed).
    pub fn add_claimed(&self, offer: &Offer, claim: &ClaimView, expires: Option<u64>) -> Event {
        build::add_claimed(&self.chain, &self.current_master(), offer, claim, expires).unwrap()
    }

    /// A `revoke` of `ids` (not pushed).
    pub fn revoke(&self, ids: &[&str]) -> Event {
        let ids: Vec<String> = ids.iter().map(|s| s.to_string()).collect();
        build::revoke(&self.chain, &self.current_master(), &ids).unwrap()
    }

    /// The `rotate` to the next master (not pushed).
    pub fn rotate(&self, revoke: &[&str]) -> Event {
        let n = self.n + 1;
        let ids: Vec<String> = revoke.iter().map(|s| s.to_string()).collect();
        build::rotate(
            &self.chain,
            &self.master(n),
            &self.salt(n),
            Some(self.next(n + 1)),
            &ids,
        )
        .unwrap()
    }

    /// The body of a signed request by `key_id` (signing with `signer`) at the person's head.
    pub fn request_body(
        &self,
        key_id: &str,
        method: &str,
        path: &str,
        body: &[u8],
        expires: u64,
    ) -> RequestBody {
        let head = self.chain.head();
        RequestBody {
            did: self.did(),
            key_id: key_id.to_string(),
            kel_seq: head.seq,
            kel_hash: head.hash.clone(),
            method: method.to_string(),
            path: path.to_string(),
            body_hash: hash_bytes(body),
            nonce: bs58::encode(rand::random::<[u8; 16]>()).into_string(),
            expires,
        }
    }

    /// The `X-Coasys-Request` header for `req`, signed by `signer`.
    pub fn sign_request(req: RequestBody, signer: &SigningKey) -> String {
        let bytes = signing_bytes(REQUEST_DOMAIN, &req).unwrap();
        let sig = encode_proof_value(&Signer::sign(signer, &bytes));
        encode_header(&SignedRequest { req, sig })
    }

    /// A signed request header by `key_id`/`signer` that expires at `expires`.
    pub fn request(
        &self,
        key_id: &str,
        signer: &SigningKey,
        method: &str,
        path: &str,
        body: &[u8],
        expires: u64,
    ) -> String {
        Self::sign_request(
            self.request_body(key_id, method, path, body, expires),
            signer,
        )
    }

    /// A signed request by the current master key.
    pub fn master_request(&self, method: &str, path: &str, body: &[u8], expires: u64) -> String {
        self.request(
            &self.master_id(),
            &self.current_master(),
            method,
            path,
            body,
            expires,
        )
    }

    /// The agent record of the person, signed by device key `key_id` (secret `signer`) and
    /// anchored at the head.
    pub fn agent_record(
        &self,
        key_id: &str,
        signer: &SigningKey,
        profile: Value,
        version: u64,
    ) -> AgentRecord {
        let head = self.chain.head();
        let mut record = AgentRecord {
            did: self.did(),
            profile,
            version,
            signed_by: did_key(signer),
            anchor: Some(Anchor {
                kel_did: self.did(),
                key_id: key_id.to_string(),
                kel_seq: head.seq,
                kel_hash: head.hash.clone(),
            }),
            sig: String::new(),
        };
        record.sig = sign_record(&record, signer);
        record
    }
}

/// The record signature (`coasys:link` over `{profile, version, anchor}`) by `signer`.
pub fn sign_record(record: &AgentRecord, signer: &SigningKey) -> String {
    let bytes = signing_bytes(RECORD_DOMAIN, &record_value(record)).unwrap();
    encode_proof_value(&Signer::sign(signer, &bytes))
}

/// A `did:key` agent record of `signer`, signed by that key.
pub fn did_key_record(signer: &SigningKey, profile: Value, version: u64) -> AgentRecord {
    let mut record = AgentRecord {
        did: did_key(signer),
        profile,
        version,
        signed_by: did_key(signer),
        anchor: None,
        sig: String::new(),
    };
    record.sig = sign_record(&record, signer);
    record
}
