//! Request/Response type serialization tests — verify REST types parse from JSON correctly.

use serde_json::json;

use crate::rest::types::*;

// ── Agent request types ──

#[test]
fn parse_generate_agent_request() {
    let json = json!({"passphrase": "secret123"});
    let req: GenerateAgentRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.passphrase, "secret123");
}

#[test]
fn parse_lock_agent_request() {
    let json = json!({"passphrase": "secret123"});
    let req: LockAgentRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.passphrase, "secret123");
}

#[test]
fn parse_unlock_agent_request() {
    let json = json!({"passphrase": "secret123", "holochain": true});
    let req: UnlockAgentRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.passphrase, "secret123");
    assert_eq!(req.holochain, Some(true));
}

#[test]
fn parse_unlock_agent_request_minimal() {
    let json = json!({"passphrase": "secret123"});
    let req: UnlockAgentRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.holochain, None);
}

#[test]
fn parse_sign_message_request() {
    let json = json!({"message": "hello world"});
    let req: SignMessageRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.message, "hello world");
}

#[test]
fn parse_request_capability_request() {
    let json = json!({
        "authInfo": {
            "appName": "test-app",
            "appDesc": "A test app",
            "appDomain": "test.example.com",
            "appUrl": "https://test.example.com",
            "capabilities": []
        }
    });
    let req: RequestCapabilityRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.auth_info.app_name, "test-app");
}

#[test]
fn parse_permit_capability_request() {
    let json = json!({"auth": "random-auth-string"});
    let req: PermitCapabilityRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.auth, "random-auth-string");
}

#[test]
fn parse_generate_jwt_request() {
    let json = json!({"rand": "abc123", "requestId": "req-456"});
    let req: GenerateJwtRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.rand, "abc123");
    assert_eq!(req.request_id, "req-456");
}

// ── Entanglement ──

#[test]
fn parse_entanglement_proof_input() {
    let json = json!({
        "deviceKey": "key123",
        "deviceKeyType": "ed25519",
        "deviceKeySignedByDid": "signature-abc",
        "didSignedByDeviceKey": null
    });
    let req: EntanglementProofInput = serde_json::from_value(json).unwrap();
    assert_eq!(req.device_key, "key123");
    assert_eq!(req.device_key_type, "ed25519");
    assert!(req.did_signed_by_device_key.is_none());
}

// ── Perspectives & Links ──

#[test]
fn parse_create_perspective_request() {
    let json = json!({"name": "My Perspective"});
    let req: CreatePerspectiveRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.name, "My Perspective");
}

#[test]
fn parse_update_perspective_request() {
    let json = json!({"name": "Renamed"});
    let req: UpdatePerspectiveRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.name, "Renamed");
}

#[test]
fn parse_link_mutation_request_additions() {
    let json = json!({
        "additions": [
            {"source": "a", "target": "b", "predicate": "links_to"}
        ]
    });
    let req: LinkMutationRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.additions.as_ref().unwrap().len(), 1);
    assert_eq!(req.additions.unwrap()[0].source, "a");
}

#[test]
fn parse_link_mutation_request_updates() {
    let json = json!({
        "updates": [{
            "oldLink": {"source": "a", "target": "b"},
            "newLink": {"source": "a", "target": "c"}
        }]
    });
    let req: LinkMutationRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.updates.as_ref().unwrap().len(), 1);
}

#[test]
fn parse_link_mutation_request_empty() {
    let json = json!({});
    let req: LinkMutationRequest = serde_json::from_value(json).unwrap();
    assert!(req.additions.is_none());
    assert!(req.removals.is_none());
    assert!(req.updates.is_none());
}

#[test]
fn parse_remove_links_bulk_request() {
    let json = json!({
        "links": [{
            "author": "did:test:123",
            "timestamp": "2024-01-01T00:00:00.000Z",
            "data": {"source": "a", "predicate": "links_to", "target": "b"},
            "proof": {"key": "pubkey", "signature": "sig"}
        }],
        "batchId": "batch-123"
    });
    let req: RemoveLinksBulkRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.links.len(), 1);
    assert_eq!(req.links[0].data.source, "a");
    assert_eq!(req.batch_id, Some("batch-123".into()));
}

#[test]
fn parse_query_request() {
    let json = json!({"engine": "prolog", "query": "triple(X, Y, Z)"});
    let req: QueryRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.engine, "prolog");
    assert_eq!(req.query, "triple(X, Y, Z)");
}

// ── Neighbourhoods ──

#[test]
fn parse_join_neighbourhood_request() {
    let json = json!({"url": "neighbourhood://Qm12345"});
    let req: JoinNeighbourhoodRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.url, "neighbourhood://Qm12345");
}

#[test]
fn parse_publish_neighbourhood_request() {
    let json = json!({
        "perspectiveUuid": "test-uuid",
        "linkLanguage": "Qm12345",
        "meta": {"links": []}
    });
    let req: PublishNeighbourhoodRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.perspective_uuid, "test-uuid");
    assert_eq!(req.link_language, "Qm12345");
}

#[test]
fn parse_broadcast_request() {
    let json = json!({
        "payload": {"links": []},
        "signed": true,
        "loopback": false
    });
    let req: BroadcastRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.signed, Some(true));
    assert_eq!(req.loopback, Some(false));
}

#[test]
fn parse_signal_request() {
    let json = json!({
        "remoteAgentDid": "did:test:123",
        "payload": {"links": []}
    });
    let req: SignalRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.remote_agent_did, "did:test:123");
}

// ── Expressions ──

#[test]
fn parse_create_expression_request() {
    let json = json!({"content": "{\"text\": \"hello\"}", "languageAddress": "Qm12345"});
    let req: CreateExpressionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.language_address, "Qm12345");
}

#[test]
fn parse_expression_many_request() {
    let json = json!({"urls": ["lang://Qm1/hash1", "lang://Qm2/hash2"]});
    let req: ExpressionManyRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.urls.len(), 2);
}

// ── Languages ──

#[test]
fn parse_publish_language_request() {
    let json = json!({
        "languagePath": "/path/to/lang",
        "languageMeta": {"name": "test-lang", "description": "A test language", "possibleTemplateParams": []}
    });
    let req: PublishLanguageRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.language_path, "/path/to/lang");
}

#[test]
fn parse_apply_template_request() {
    let json = json!({
        "sourceLanguageHash": "Qm12345",
        "templateData": "{\"key\": \"value\"}"
    });
    let req: ApplyTemplateRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.source_language_hash, "Qm12345");
}

// ── Runtime ──

#[test]
fn parse_set_status_request() {
    let json = json!({"status": {"key": "value"}});
    let req: SetStatusRequest = serde_json::from_value(json).unwrap();
    assert!(req.status.is_object());
}

#[test]
fn parse_open_link_request() {
    let json = json!({"url": "https://example.com"});
    let req: OpenLinkRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.url, "https://example.com");
}

#[test]
fn parse_export_request() {
    let json = json!({"type": "db", "filePath": "/tmp/export.json"});
    let req: ExportRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.export_type, "db");
    assert_eq!(req.file_path, "/tmp/export.json");
    assert!(req.perspective_uuid.is_none());
}

#[test]
fn parse_export_request_with_perspective() {
    let json = json!({
        "type": "perspective",
        "filePath": "/tmp/perspective.json",
        "perspectiveUuid": "test-uuid"
    });
    let req: ExportRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.export_type, "perspective");
    assert_eq!(req.perspective_uuid, Some("test-uuid".to_string()));
}

#[test]
fn parse_import_request() {
    let json = json!({"type": "db", "filePath": "/tmp/import.json"});
    let req: ImportRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.import_type, "db");
}

#[test]
fn parse_friend_send_message_request() {
    let json = json!({"message": "hello friend"});
    let req: FriendSendMessageRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.message, "hello friend");
}

#[test]
fn parse_friends_list_request() {
    let json = json!({"dids": ["did:test:1", "did:test:2"]});
    let req: FriendsListRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.dids.len(), 2);
}

#[test]
fn parse_link_language_templates_request() {
    let json = json!({"addresses": ["Qm1", "Qm2"]});
    let req: LinkLanguageTemplatesRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.addresses.len(), 2);
}

// ── Users ──

#[test]
fn parse_create_user_request() {
    let json = json!({"email": "user@example.com", "password": "pass123"});
    let req: CreateUserRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.email, "user@example.com");
}

#[test]
fn parse_login_user_request() {
    let json = json!({"email": "user@example.com", "password": "pass123", "appName": "MyApp"});
    let req: LoginUserRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.app_name, Some("MyApp".to_string()));
}

#[test]
fn parse_login_user_request_minimal() {
    let json = json!({"email": "user@example.com", "password": "pass123"});
    let req: LoginUserRequest = serde_json::from_value(json).unwrap();
    assert!(req.app_name.is_none());
}

#[test]
fn parse_verify_email_request() {
    let json = json!({
        "email": "user@example.com",
        "code": "123456",
        "verificationType": "login"
    });
    let req: VerifyEmailRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.code, "123456");
    assert_eq!(req.verification_type, Some("login".to_string()));
}

#[test]
fn parse_set_multi_user_request() {
    let json = json!({"enabled": true});
    let req: SetMultiUserRequest = serde_json::from_value(json).unwrap();
    assert!(req.enabled);
}

#[test]
fn parse_set_user_free_access_request() {
    let json = json!({"email": "user@example.com", "enabled": true});
    let req: SetUserFreeAccessRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.email, "user@example.com");
    assert!(req.enabled);
}

// ── AI ──

#[test]
fn parse_prompt_request() {
    let json = json!({"taskId": "task-1", "prompt": "What is AI?"});
    let req: PromptRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.task_id, "task-1");
    assert_eq!(req.prompt, "What is AI?");
}

#[test]
fn parse_embed_request() {
    let json = json!({"modelId": "model-1", "text": "hello world"});
    let req: EmbedRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.model_id, "model-1");
}

#[test]
fn parse_set_default_model_request() {
    let json = json!({"modelType": "llm"});
    let req: SetDefaultModelRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.model_type.to_string(), "LLM");
}

// ── Notifications ──

#[test]
fn parse_notification_input() {
    let json = json!({
        "description": "New message notification",
        "appName": "Flux",
        "appUrl": "https://fluxsocial.io",
        "appIconPath": "/icons/flux.png",
        "trigger": "triple(X, ad4m://has_type, flux://message)",
        "perspectiveIds": ["uuid-1", "uuid-2"],
        "webhookUrl": "https://example.com/webhook",
        "webhookAuth": "bearer-token"
    });
    let req: NotificationInput = serde_json::from_value(json).unwrap();
    assert_eq!(req.app_name, "Flux");
    assert_eq!(req.perspective_ids.len(), 2);
}

#[test]
fn parse_notification_input_no_icon() {
    let json = json!({
        "description": "Test",
        "appName": "Test",
        "appUrl": "https://test.com",
        "trigger": "query",
        "perspectiveIds": [],
        "webhookUrl": "https://test.com/hook",
        "webhookAuth": ""
    });
    let req: NotificationInput = serde_json::from_value(json).unwrap();
    assert!(req.app_icon_path.is_none());
}

// ── SDNA / Commands ──

#[test]
fn parse_add_sdna_request() {
    let json = json!({
        "name": "my-sdna",
        "sdnaCode": "some prolog code",
        "sdnaType": "subject_class"
    });
    let req: AddSdnaRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.name, "my-sdna");
    assert_eq!(req.sdna_type, "subject_class");
}

#[test]
fn parse_execute_commands_request() {
    let json = json!({
        "commands": "[{\"type\":\"add\",\"data\":{}}]",
        "expression": "{\"subject\":\"test\"}",
        "parameters": "[{\"name\":\"value\",\"value\":\"literal:string:abc\"}]",
        "batchId": "batch-123"
    });
    let req: ExecuteCommandsRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.commands, "[{\"type\":\"add\",\"data\":{}}]");
    assert_eq!(
        req.parameters,
        Some("[{\"name\":\"value\",\"value\":\"literal:string:abc\"}]".into())
    );
    assert_eq!(req.batch_id, Some("batch-123".into()));
}

// ── Hosting response types ──

#[test]
fn serialize_hosting_info_response() {
    let resp = HostingInfoResponse {
        user_info: Some(json!({"email": "test@example.com"})),
        rates: Some(json!({"prompt": 0.001})),
        version: Some(json!({"major": 1})),
    };
    let json = serde_json::to_value(&resp).unwrap();
    assert_eq!(json["userInfo"]["email"], "test@example.com");
    assert!(json["rates"]["prompt"].is_number());
}

#[test]
fn serialize_hosting_wallet_response() {
    let resp = HostingWalletResponse {
        balance: Some(json!("100000")),
        pubkey: Some("pubkey123".to_string()),
    };
    let json = serde_json::to_value(&resp).unwrap();
    assert_eq!(json["pubkey"], "pubkey123");
}

#[test]
fn serialize_link_mutation_response() {
    let resp = LinkMutationResponse {
        additions: vec![],
        removals: vec![],
        updates: vec![],
    };
    let json = serde_json::to_value(&resp).unwrap();
    assert!(json["additions"].is_array());
    assert!(json["removals"].is_array());
    assert!(json["updates"].is_array());
}

// ── Verify Signature ──

#[test]
fn parse_verify_signature_request() {
    let json = json!({
        "did": "did:key:z123",
        "data": "hello",
        "signedData": "abc123"
    });
    let req: VerifySignatureRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.did, "did:key:z123");
}

// ── Dev ──

#[test]
fn parse_email_test_request() {
    let json = json!({"action": "enable"});
    let req: EmailTestRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.action, "enable");
    assert!(req.to.is_none());
}

#[test]
fn parse_email_test_request_with_to() {
    let json = json!({"action": "send", "to": "test@example.com"});
    let req: EmailTestRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.to, Some("test@example.com".to_string()));
}

#[test]
fn parse_email_test_request_with_expiry_fields() {
    let json = json!({
        "action": "set-expiry",
        "email": "test@example.com",
        "verificationType": "login",
        "expiresAt": 1234567890
    });
    let req: EmailTestRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.email, Some("test@example.com".to_string()));
    assert_eq!(req.verification_type, Some("login".to_string()));
    assert_eq!(req.expires_at, Some(1234567890));
}

// ── Invalid input tests ──

#[test]
fn missing_required_field_fails() {
    let json = json!({});
    let result = serde_json::from_value::<GenerateAgentRequest>(json);
    assert!(result.is_err());
}

#[test]
fn wrong_type_fails() {
    let json = json!({"passphrase": 123});
    let result = serde_json::from_value::<GenerateAgentRequest>(json);
    assert!(result.is_err());
}

#[test]
fn extra_fields_ignored() {
    let json = json!({"passphrase": "secret", "extraField": "ignored"});
    let req: GenerateAgentRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.passphrase, "secret");
}

// ── camelCase enforcement ──

#[test]
fn camel_case_deserialization() {
    // Fields should use camelCase in JSON
    let json = json!({"perspectiveUuid": "uuid-1", "linkLanguage": "Qm123", "meta": {"links": []}});
    let req: PublishNeighbourhoodRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.perspective_uuid, "uuid-1");
}

#[test]
fn snake_case_rejected() {
    // snake_case should NOT work (camelCase is enforced)
    let json =
        json!({"perspective_uuid": "uuid-1", "link_language": "Qm123", "meta": {"links": []}});
    let result = serde_json::from_value::<PublishNeighbourhoodRequest>(json);
    assert!(
        result.is_err(),
        "snake_case should be rejected when rename_all=camelCase"
    );
}

// ── AI Transcription request types ──

use crate::rest::types::{
    CloseTranscriptionRequest, FeedTranscriptionRequest, OpenTranscriptionRequest,
};
use crate::rest::audio_ws::AudioWsParams;

#[test]
fn parse_open_transcription_request() {
    let json = json!({"modelId": "whisper-small"});
    let req: OpenTranscriptionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.model_id, "whisper-small");
    assert!(req.params.is_none());
}

#[test]
fn parse_open_transcription_request_with_params() {
    let json = json!({
        "modelId": "whisper-medium",
        "params": {
            "startThreshold": 0.5,
            "startWindow": 5,
            "endThreshold": 0.3,
            "endWindow": 10,
            "timeBeforeSpeech": 2
        }
    });
    let req: OpenTranscriptionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.model_id, "whisper-medium");
    let p = req.params.unwrap();
    assert_eq!(p.start_threshold, Some(0.5));
    assert_eq!(p.start_window, Some(5));
}

#[test]
fn parse_feed_transcription_request() {
    let json = json!({
        "streamIds": ["s1", "s2"],
        "audio": [0.1, 0.2, -0.5, 1.0]
    });
    let req: FeedTranscriptionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.stream_ids.len(), 2);
    assert_eq!(req.audio.len(), 4);
}

#[test]
fn parse_close_transcription_request() {
    let json = json!({"streamId": "abc-123"});
    let req: CloseTranscriptionRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.stream_id, "abc-123");
}

#[test]
fn parse_audio_ws_params() {
    let json = json!({"stream_ids": "s1,s2"});
    let params: AudioWsParams = serde_json::from_value(json).unwrap();
    assert_eq!(params.stream_ids, "s1,s2");
}

// ── Agent infos contract regression tests ──

#[test]
fn parse_add_agent_infos_request_array() {
    let json = json!({"agentInfos": ["info1", "info2", "info3"]});
    let req: AddAgentInfosRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.agent_infos, vec!["info1", "info2", "info3"]);
}

#[test]
fn parse_add_agent_infos_request_empty_array() {
    let json = json!({"agentInfos": []});
    let req: AddAgentInfosRequest = serde_json::from_value(json).unwrap();
    assert!(req.agent_infos.is_empty());
}

#[test]
fn parse_add_agent_infos_request_single_element() {
    let json = json!({"agentInfos": ["only-one"]});
    let req: AddAgentInfosRequest = serde_json::from_value(json).unwrap();
    assert_eq!(req.agent_infos, vec!["only-one"]);
}

#[test]
fn reject_add_agent_infos_request_bare_string() {
    // Regression: the old type accepted a bare string; it must now require an array.
    let json = json!({"agentInfos": "not-an-array"});
    assert!(
        serde_json::from_value::<AddAgentInfosRequest>(json).is_err(),
        "AddAgentInfosRequest must reject a bare string — the contract requires string[]"
    );
}

#[test]
fn add_agent_infos_roundtrip_with_agent_infos_output() {
    // Simulate the output of GET /runtime/hc/agent-infos (Vec<String>) being fed
    // directly into the POST body, confirming the GET→POST contract is compatible.
    let get_response: Vec<String> = vec![
        r#"{"agent":"uhCAk...","url_list":["wss://signal.holo.host"]}"#.into(),
        r#"{"agent":"uhCAk...","url_list":["wss://other.host"]}"#.into(),
    ];
    let post_body = json!({"agentInfos": get_response});
    let req: AddAgentInfosRequest = serde_json::from_value(post_body).unwrap();
    assert_eq!(req.agent_infos.len(), 2);
    assert!(req.agent_infos[0].contains("signal.holo.host"));
    assert!(req.agent_infos[1].contains("other.host"));
}
