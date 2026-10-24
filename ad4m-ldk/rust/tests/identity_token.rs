//! Contract of the identity and token interfaces (AR-01 4.5.2, 4.9, 4.9.1;
//! AR-02 4.3.1, 4.16): a test language implements `IdentityCapability` and
//! `TokenCapability` and expands through `ad4m_language!`; the wire types
//! carry AR-01's JSON field names exactly and round-trip through serde.
//!
//! The shims marshal through `JsValue` and only run inside WASM, so the
//! test calls the trait methods directly and checks that the shims expand.

use ad4m_ldk::prelude::*;
use futures::executor::block_on;
use serde::de::DeserializeOwned;
use serde::Serialize;
use serde_json::{json, Value};

fn head() -> SignedHead {
    SignedHead {
        scid: "zScid".into(),
        seq: 1,
        hash: "zHash1".into(),
        time: 1_700_000_000,
        store: "did:key:zStore".into(),
        sig: "zHeadSig".into(),
    }
}

fn request() -> SignedRequest {
    SignedRequest {
        req: RequestBody {
            did: "did:scid:zScid".into(),
            key_id: "m1".into(),
            kel_seq: 1,
            kel_hash: "zHash1".into(),
            method: "GET".into(),
            path: "/v1/offers/zScid".into(),
            body_hash: "zBody".into(),
            nonce: "zNonce".into(),
            expires: 1_700_000_300,
        },
        sig: "zReqSig".into(),
    }
}

fn signed_offer() -> SignedOffer {
    SignedOffer {
        offer: Offer {
            scid: "zScid".into(),
            offer_id: "zOffer".into(),
            kind: OfferKind::Device,
            commit: "zCommit".into(),
            expires: 1_700_000_300,
            device_key: None,
            bot: None,
        },
        by: SignedBy {
            key_id: "m1".into(),
            kel_seq: 1,
            kel_hash: "zHash1".into(),
        },
        sig: "zOfferSig".into(),
    }
}

fn transfer() -> TokenTransfer {
    TokenTransfer {
        id: "t1".into(),
        from: "did:scid:zA".into(),
        to: "did:key:zB".into(),
        asset: "unyt:x".into(),
        amount: Amount {
            value: "12345678901234567890".into(),
            decimals: 18,
        },
        memo: None,
        time: 5,
        state: TransferState::Settled,
    }
}

/// A test agent language: answers from what it holds and records what it got.
#[derive(Default)]
pub struct Agent {
    appended: Vec<AppendRequest>,
    calls: Vec<String>,
}

impl Language for Agent {
    fn name() -> &'static str {
        "@test/agent"
    }
    fn version() -> &'static str {
        "0.0.0"
    }
    async fn init() -> LanguageResult<Self> {
        Ok(Agent::default())
    }
}

impl IdentityCapability for Agent {
    async fn identity_log(&mut self, did: String, from: u64) -> LanguageResult<Option<LogPage>> {
        self.calls.push(format!("log {did} {from}"));
        if self.appended.is_empty() {
            return Ok(None);
        }
        let events: Vec<Value> = self
            .appended
            .iter()
            .flat_map(|r| r.events.clone())
            .collect();
        Ok(Some(LogPage {
            appended_at: vec![10; events.len()],
            events,
            head: head(),
            note: None,
            vouched: false,
            pauses: Vec::new(),
        }))
    }
    async fn identity_head(&mut self, did: String) -> LanguageResult<Option<SignedHead>> {
        self.calls.push(format!("head {did}"));
        Ok(Some(head()))
    }
    async fn identity_append(
        &mut self,
        did: String,
        req: AppendRequest,
    ) -> LanguageResult<SignedHead> {
        self.calls.push(format!("append {did}"));
        if req.vouch.is_some() && req.pow.is_some() {
            return Err(IdentityErrorCode::Invalid {
                reason: "vouch and pow".into(),
            }
            .into());
        }
        self.appended.push(req);
        Ok(head())
    }
    async fn identity_key_owner(
        &mut self,
        multikey: String,
        wait_s: u8,
    ) -> LanguageResult<Option<KeyIndexEntry>> {
        self.calls.push(format!("keyOwner {multikey} {wait_s}"));
        Ok(None)
    }
    async fn identity_pow_params(&mut self) -> LanguageResult<PowParams> {
        Ok(PowParams {
            bits: 22,
            previous_bits: 20,
            changed_at: 3,
        })
    }
    async fn identity_attach_note(
        &mut self,
        did: String,
        _token: String,
        _req: SignedRequest,
    ) -> LanguageResult<VouchNote> {
        Ok(VouchNote {
            did,
            method: "email".into(),
            time: 4,
            voucher: "did:key:zVouch".into(),
            sig: "zNoteSig".into(),
        })
    }
    async fn identity_put_agent(
        &mut self,
        record: AgentRecord,
        req: Option<SignedRequest>,
    ) -> LanguageResult<()> {
        if record.did.starts_with("did:scid:") && req.is_none() {
            return Err(IdentityErrorCode::Forbidden {
                reason: ForbiddenReason::Signer,
            }
            .into());
        }
        Ok(())
    }
    async fn identity_open_offer(&mut self, offer: SignedOffer) -> LanguageResult<OfferOpened> {
        Ok(OfferOpened {
            offer_id: offer.offer.offer_id,
        })
    }
    async fn identity_list_offers(
        &mut self,
        _did: String,
        _req: SignedRequest,
        _wait_s: u8,
    ) -> LanguageResult<Vec<OfferListing>> {
        Ok(vec![OfferListing {
            offer: signed_offer(),
            state: OfferState::Open,
        }])
    }
    async fn identity_cancel_offer(
        &mut self,
        _did: String,
        offer_id: String,
        _req: SignedRequest,
    ) -> LanguageResult<()> {
        self.calls.push(format!("cancel {offer_id}"));
        Ok(())
    }
    async fn identity_claim_offer(
        &mut self,
        _did: String,
        _offer_id: String,
        claim: Claim,
    ) -> LanguageResult<ClaimReceipt> {
        if claim.s != "zS" {
            return Err(IdentityErrorCode::Forbidden {
                reason: ForbiddenReason::Secret,
            }
            .into());
        }
        Ok(ClaimReceipt {
            state: "claimed".into(),
            kind: OfferKind::Device,
            expires: 1_700_000_300,
        })
    }
    async fn identity_offer_state(
        &mut self,
        _did: String,
        _offer_id: String,
        _key: String,
        _wait_s: u8,
    ) -> LanguageResult<OfferProgress> {
        Ok(OfferProgress {
            state: "spent".into(),
            seq: Some(2),
        })
    }
}

impl TokenCapability for Agent {
    async fn token_assets(&mut self) -> LanguageResult<Vec<String>> {
        Err(TokenErrorCode::Disabled.into())
    }
    async fn token_balance(
        &mut self,
        _account: String,
        _asset: String,
    ) -> LanguageResult<TokenBalance> {
        Err(TokenErrorCode::Disabled.into())
    }
    async fn token_send(
        &mut self,
        _to: String,
        _asset: String,
        _amount: Amount,
        _memo: Option<String>,
    ) -> LanguageResult<TokenTransfer> {
        Err(TokenErrorCode::Disabled.into())
    }
    async fn token_received(
        &mut self,
        _cursor: Option<String>,
        _limit: u32,
    ) -> LanguageResult<TransferPage> {
        Err(TokenErrorCode::Disabled.into())
    }
}

ad4m_language! {
    language: Agent,
    capabilities: [identity, token],
}

/// `value` serialises to exactly `expected`, and `expected` reads back as `value`.
fn wire<T: Serialize + DeserializeOwned + PartialEq + std::fmt::Debug>(value: &T, expected: Value) {
    assert_eq!(serde_json::to_value(value).unwrap(), expected);
    let back: T = serde_json::from_value(expected).unwrap();
    assert_eq!(&back, value);
}

#[test]
fn shims_expand() {
    assert_eq!(__ad4m_name(), "@test/agent");
}

#[test]
fn identity_calls_pass_events_through_unchanged() {
    let mut lang = block_on(Agent::init()).unwrap();
    assert_eq!(
        block_on(lang.identity_log("did:scid:zScid".into(), 0)).unwrap(),
        None
    );

    // Events are opaque JSON: unknown keys, nesting and large integers stay as they are.
    let events = vec![
        json!({"v": 1, "op": "incept", "seq": 0, "keys": [{"id": "d1", "sign": "z6Mk1"}], "x-extra": {"deep": [1, null, true]}}),
        json!({"v": 1, "op": "add", "seq": 1, "prev": "zHash0", "big": 9_007_199_254_740_993u64}),
    ];
    let req = AppendRequest {
        events: events.clone(),
        vouch: None,
        pow: Some(Pow {
            nonce: "zNonce16".into(),
            bits: 22,
        }),
    };
    assert_eq!(
        block_on(lang.identity_append("did:scid:zScid".into(), req)).unwrap(),
        head()
    );
    let page = block_on(lang.identity_log("did:scid:zScid".into(), 0))
        .unwrap()
        .unwrap();
    assert_eq!(page.events, events);
    let page_json = serde_json::to_value(&page).unwrap();
    assert_eq!(page_json["events"], Value::Array(events.clone()));
    let back: LogPage = serde_json::from_value(page_json).unwrap();
    assert_eq!(back.events, events);

    assert_eq!(
        block_on(lang.identity_head("did:scid:zScid".into())).unwrap(),
        Some(head())
    );
    assert_eq!(
        block_on(lang.identity_key_owner("z6MkKey".into(), 8)).unwrap(),
        None
    );
    assert_eq!(
        block_on(lang.identity_pow_params()).unwrap().previous_bits,
        20
    );
    assert_eq!(
        block_on(lang.identity_attach_note("did:scid:zScid".into(), "tok".into(), request()))
            .unwrap()
            .method,
        "email"
    );
    assert_eq!(
        lang.calls,
        vec![
            "log did:scid:zScid 0",
            "append did:scid:zScid",
            "log did:scid:zScid 0",
            "head did:scid:zScid",
            "keyOwner z6MkKey 8"
        ]
    );
}

#[test]
fn identity_offer_and_claim_calls() {
    let mut lang = block_on(Agent::init()).unwrap();
    let did = || "did:scid:zScid".to_string();
    assert_eq!(
        block_on(lang.identity_open_offer(signed_offer()))
            .unwrap()
            .offer_id,
        "zOffer"
    );
    assert_eq!(
        block_on(lang.identity_list_offers(did(), request(), 0)).unwrap()[0].state,
        OfferState::Open
    );
    block_on(lang.identity_cancel_offer(did(), "zOffer".into(), request())).unwrap();
    let claim = Claim {
        offer_id: "zOffer".into(),
        s: "zS".into(),
        device_key: "z6MkNew".into(),
        enc_key: "z6LSNew".into(),
        can_keep_master: false,
        hint: None,
        co_sig: "zCo".into(),
    };
    assert_eq!(
        block_on(lang.identity_claim_offer(did(), "zOffer".into(), claim.clone()))
            .unwrap()
            .kind,
        OfferKind::Device
    );
    let wrong = Claim {
        s: "zWrong".into(),
        ..claim
    };
    let err = block_on(lang.identity_claim_offer(did(), "zOffer".into(), wrong)).unwrap_err();
    assert_eq!(
        IdentityErrorCode::from_language_error(&err),
        Some(IdentityErrorCode::Forbidden {
            reason: ForbiddenReason::Secret
        })
    );
    assert!(matches!(err.code, ErrorCode::PermissionDenied));
    assert_eq!(
        block_on(lang.identity_offer_state(did(), "zOffer".into(), "z6MkNew".into(), 2))
            .unwrap()
            .seq,
        Some(2)
    );
}

#[test]
fn token_calls_answer_disabled() {
    let mut lang = block_on(Agent::init()).unwrap();
    let err = block_on(lang.token_assets()).unwrap_err();
    assert_eq!(
        TokenErrorCode::from_language_error(&err),
        Some(TokenErrorCode::Disabled)
    );
    assert_eq!(err.message, r#"{"code":"disabled"}"#);
    let amount = Amount {
        value: "1".into(),
        decimals: 0,
    };
    assert!(block_on(lang.token_send("did:key:zB".into(), "unyt:x".into(), amount, None)).is_err());
    assert!(block_on(lang.token_received(None, 10)).is_err());
    assert!(block_on(lang.token_balance("did:key:zA".into(), "unyt:x".into())).is_err());
}

#[test]
fn identity_wire_types_use_ar01_field_names() {
    let head_json = json!({"scid": "zScid", "seq": 1, "hash": "zHash1", "time": 1_700_000_000, "store": "did:key:zStore", "sig": "zHeadSig"});
    wire(&head(), head_json.clone());

    let note = VouchNote {
        did: "did:scid:zScid".into(),
        method: "email".into(),
        time: 4,
        voucher: "did:key:zV".into(),
        sig: "zN".into(),
    };
    let note_json = json!({"did": "did:scid:zScid", "method": "email", "time": 4, "voucher": "did:key:zV", "sig": "zN"});
    wire(&note, note_json.clone());

    let by = SignedBy {
        key_id: "d2".into(),
        kel_seq: 3,
        kel_hash: "zH3".into(),
    };
    let by_json = json!({"keyId": "d2", "kelSeq": 3, "kelHash": "zH3"});
    let pause = PauseRecord {
        scid: "zScid".into(),
        key_id: "d4".into(),
        paused: true,
        version: 2,
        by,
        sig: "zP".into(),
        stored_at: 7,
    };
    let pause_json = json!({"scid": "zScid", "keyId": "d4", "paused": true, "version": 2, "by": by_json, "sig": "zP", "storedAt": 7});
    wire(&pause, pause_json.clone());

    wire(
        &LogPage {
            events: vec![json!({"op": "incept"})],
            appended_at: vec![10],
            head: head(),
            note: Some(note),
            vouched: true,
            pauses: vec![pause],
        },
        json!({"events": [{"op": "incept"}], "appendedAt": [10], "head": head_json, "note": note_json, "vouched": true, "pauses": [pause_json]}),
    );

    wire(
        &AppendRequest {
            events: vec![json!({"op": "add"})],
            vouch: Some("tok".into()),
            pow: None,
        },
        json!({"events": [{"op": "add"}], "vouch": "tok", "pow": null}),
    );
    wire(
        &Pow {
            nonce: "zN".into(),
            bits: 22,
        },
        json!({"nonce": "zN", "bits": 22}),
    );
    wire(
        &PowParams {
            bits: 22,
            previous_bits: 20,
            changed_at: 3,
        },
        json!({"bits": 22, "previousBits": 20, "changedAt": 3}),
    );

    wire(
        &KeyIndexEntry {
            did: "did:scid:zScid".into(),
            key_id: "d3".into(),
            added: 1,
            revoked: None,
            bot: Some(json!({"can": ["read"], "expires": 99})),
            expires: Some(99),
            paused: false,
            head: head(),
        },
        json!({"did": "did:scid:zScid", "keyId": "d3", "added": 1, "revoked": null, "bot": {"can": ["read"], "expires": 99},
               "expires": 99, "paused": false, "head": head_json}),
    );

    wire(
        &request(),
        json!({"req": {"did": "did:scid:zScid", "keyId": "m1", "kelSeq": 1, "kelHash": "zHash1", "method": "GET",
                       "path": "/v1/offers/zScid", "bodyHash": "zBody", "nonce": "zNonce", "expires": 1_700_000_300},
               "sig": "zReqSig"}),
    );

    wire(
        &AgentRecord {
            did: "did:scid:zScid".into(),
            profile: json!({"perspective": {"links": []}, "directMessageLanguage": "lang://x"}),
            version: 3,
            signed_by: "did:key:zDev".into(),
            anchor: Some(Anchor {
                kel_did: "did:scid:zScid".into(),
                key_id: "d1".into(),
                kel_seq: 1,
                kel_hash: "zHash1".into(),
            }),
            sig: "zRec".into(),
        },
        json!({"did": "did:scid:zScid", "profile": {"perspective": {"links": []}, "directMessageLanguage": "lang://x"},
               "version": 3, "signedBy": "did:key:zDev",
               "anchor": {"kelDid": "did:scid:zScid", "keyId": "d1", "kelSeq": 1, "kelHash": "zHash1"}, "sig": "zRec"}),
    );
}

#[test]
fn optional_fields_read_when_missing() {
    // A store or a JS language may leave a null field out.
    let page: LogPage = serde_json::from_value(json!({"events": [], "appendedAt": [], "head": serde_json::to_value(head()).unwrap(), "vouched": false})).unwrap();
    assert_eq!(page.note, None);
    assert!(page.pauses.is_empty());
    let req: AppendRequest = serde_json::from_value(json!({"events": []})).unwrap();
    assert_eq!((req.vouch, req.pow), (None, None));
}

#[test]
fn offer_wire_types_use_ar02_field_names() {
    // Unbound, non-bot offer: deviceKey and bot are left out, not null.
    wire(
        &signed_offer(),
        json!({"offer": {"scid": "zScid", "offerId": "zOffer", "kind": "device", "commit": "zCommit", "expires": 1_700_000_300},
               "by": {"keyId": "m1", "kelSeq": 1, "kelHash": "zHash1"}, "sig": "zOfferSig"}),
    );
    let bound = Offer {
        kind: OfferKind::Bot,
        device_key: Some("z6MkBot".into()),
        bot: Some(json!({"can": ["post"], "expires": 9})),
        ..signed_offer().offer
    };
    wire(
        &bound,
        json!({"scid": "zScid", "offerId": "zOffer", "kind": "bot", "commit": "zCommit", "expires": 1_700_000_300,
               "deviceKey": "z6MkBot", "bot": {"can": ["post"], "expires": 9}}),
    );
    assert_eq!(
        serde_json::to_value(OfferKind::Node).unwrap(),
        json!("node")
    );

    let claim = Claim {
        offer_id: "zOffer".into(),
        s: "zS".into(),
        device_key: "z6MkNew".into(),
        enc_key: "z6LSNew".into(),
        can_keep_master: false,
        hint: None,
        co_sig: "zCo".into(),
    };
    wire(
        &claim,
        json!({"offerId": "zOffer", "s": "zS", "deviceKey": "z6MkNew", "encKey": "z6LSNew", "coSig": "zCo"}),
    );
    wire(
        &Claim {
            can_keep_master: true,
            hint: Some("Pixel 8".into()),
            ..claim
        },
        json!({"offerId": "zOffer", "s": "zS", "deviceKey": "z6MkNew", "encKey": "z6LSNew", "canKeepMaster": true, "hint": "Pixel 8", "coSig": "zCo"}),
    );

    let view = ClaimView {
        device_key: "z6MkNew".into(),
        enc_key: "z6LSNew".into(),
        can_keep_master: true,
        hint: None,
        co_sig: "zCo".into(),
        claimed_at: 8,
    };
    let view_json = json!({"deviceKey": "z6MkNew", "encKey": "z6LSNew", "canKeepMaster": true, "hint": null, "coSig": "zCo", "claimedAt": 8});
    wire(&view, view_json.clone());
    wire(&OfferState::Open, json!({"state": "open"}));
    wire(
        &OfferState::Claimed { claim: view },
        json!({"state": "claimed", "claim": view_json}),
    );
    wire(
        &OfferState::Spent { seq: 4 },
        json!({"state": "spent", "seq": 4}),
    );
    for (state, name) in [
        (OfferState::Expired, "expired"),
        (OfferState::Cancelled, "cancelled"),
        (OfferState::Contested, "contested"),
        (OfferState::Locked, "locked"),
        (OfferState::Superseded, "superseded"),
    ] {
        wire(&state, json!({"state": name}));
    }
    wire(
        &OfferOpened {
            offer_id: "zOffer".into(),
        },
        json!({"offerId": "zOffer"}),
    );
    wire(
        &ClaimReceipt {
            state: "claimed".into(),
            kind: OfferKind::Node,
            expires: 5,
        },
        json!({"state": "claimed", "kind": "node", "expires": 5}),
    );
    wire(
        &OfferProgress {
            state: "claimed".into(),
            seq: None,
        },
        json!({"state": "claimed"}),
    );
    wire(
        &OfferProgress {
            state: "spent".into(),
            seq: Some(2),
        },
        json!({"state": "spent", "seq": 2}),
    );
}

#[test]
fn identity_error_codes_match_ar01() {
    wire(
        &IdentityErrorCode::Conflict { head: head() },
        json!({"code": "conflict", "head": serde_json::to_value(head()).unwrap()}),
    );
    wire(
        &IdentityErrorCode::KeyTaken { key: "z6Mk".into() },
        json!({"code": "key-taken", "key": "z6Mk"}),
    );
    for (reason, name) in [
        (ForbiddenReason::Vouch, "vouch"),
        (ForbiddenReason::Pow, "pow"),
        (ForbiddenReason::Claim, "claim"),
        (ForbiddenReason::Signer, "signer"),
        (ForbiddenReason::Secret, "secret"),
        (ForbiddenReason::Bound, "bound"),
        (ForbiddenReason::Cosig, "cosig"),
        (ForbiddenReason::Writer, "writer"),
    ] {
        wire(
            &IdentityErrorCode::Forbidden { reason },
            json!({"code": "forbidden", "reason": name}),
        );
    }
    wire(
        &IdentityErrorCode::Invalid {
            reason: "seq".into(),
        },
        json!({"code": "invalid", "reason": "seq"}),
    );
    wire(
        &IdentityErrorCode::RateLimited {
            scope: RateLimitScope::Identity,
            retry_after_sec: 30,
        },
        json!({"code": "rate-limited", "scope": "identity", "retryAfterSec": 30}),
    );
    wire(
        &IdentityErrorCode::Gone {
            state: "expired".into(),
        },
        json!({"code": "gone", "state": "expired"}),
    );
    wire(&IdentityErrorCode::NotFound, json!({"code": "not-found"}));
    wire(
        &IdentityErrorCode::Unavailable,
        json!({"code": "unavailable"}),
    );

    let err: LanguageError = IdentityErrorCode::Conflict { head: head() }.into();
    assert!(matches!(err.code, ErrorCode::InvalidInput));
    assert_eq!(
        IdentityErrorCode::from_language_error(&err),
        Some(IdentityErrorCode::Conflict { head: head() })
    );
    assert!(matches!(
        LanguageError::from(IdentityErrorCode::Unavailable).code,
        ErrorCode::Transient
    ));
    assert!(matches!(
        LanguageError::from(IdentityErrorCode::NotFound).code,
        ErrorCode::NotFound
    ));
    assert_eq!(
        IdentityErrorCode::from_language_error(&LanguageError::internal("plain")),
        None
    );
}

#[test]
fn token_wire_types_match_ar01() {
    let amount_json = json!({"value": "12345678901234567890", "decimals": 18});
    let transfer_json = json!({"id": "t1", "from": "did:scid:zA", "to": "did:key:zB", "asset": "unyt:x",
                               "amount": amount_json, "time": 5, "state": "settled"});
    wire(&transfer(), transfer_json.clone());
    wire(
        &TokenTransfer {
            memo: Some("hi".into()),
            state: TransferState::Pending,
            ..transfer()
        },
        json!({"id": "t1", "from": "did:scid:zA", "to": "did:key:zB", "asset": "unyt:x",
               "amount": amount_json, "memo": "hi", "time": 5, "state": "pending"}),
    );
    wire(
        &TokenBalance {
            account: "did:key:zA".into(),
            asset: "unyt:x".into(),
            amount: transfer().amount,
            as_of: 6,
        },
        json!({"account": "did:key:zA", "asset": "unyt:x", "amount": amount_json, "asOf": 6}),
    );
    wire(
        &TransferPage {
            items: vec![transfer()],
            cursor: None,
        },
        json!({"items": [transfer_json], "cursor": null}),
    );
    wire(
        &TokenTransferSignal::new(transfer()),
        json!({"tag": "token-transfer", "transfer": transfer_json}),
    );
    assert_eq!(TOKEN_TRANSFER_SIGNAL, "token-transfer");

    for (code, json) in [
        (TokenErrorCode::Disabled, json!({"code": "disabled"})),
        (
            TokenErrorCode::UnknownAsset,
            json!({"code": "unknown-asset"}),
        ),
        (
            TokenErrorCode::UnknownAccount,
            json!({"code": "unknown-account"}),
        ),
        (
            TokenErrorCode::Insufficient,
            json!({"code": "insufficient"}),
        ),
        (
            TokenErrorCode::Refused {
                reason: "limit".into(),
            },
            json!({"code": "refused", "reason": "limit"}),
        ),
        (TokenErrorCode::Unavailable, json!({"code": "unavailable"})),
    ] {
        wire(&code, json);
    }
}
