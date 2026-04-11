//! Generates `core/src/generated/rest/routes.ts` from inventory-collected route metadata.

use crate::rest::route_meta::RouteMetadata;

#[test]
fn generate_route_map() {
    let mut routes: Vec<&RouteMetadata> = inventory::iter::<RouteMetadata>.into_iter().collect();
    routes.sort_by_key(|r| (r.path, r.method));

    let mut ts = String::new();
    ts.push_str("// AUTO-GENERATED — do not edit manually\n");
    ts.push_str("// Regenerate: cd rust-executor && cargo test generate_route_map\n\n");

    // Collect all unique request/response types to build import lists
    let mut rest_types = std::collections::BTreeSet::new();
    let mut domain_types = std::collections::BTreeSet::new();
    let mut ai_types = std::collections::BTreeSet::new();

    // Known domain types (from core/src/index.ts re-exports)
    let domain_set: std::collections::HashSet<&str> = [
        "Agent",
        "AgentSignature",
        "AgentStatus",
        "Apps",
        "EntanglementProof",
        "EntanglementProofInput",
        "InteractionCall",
        "InteractionMeta",
        "LanguageHandle",
        "LanguageMeta",
        "LanguageRef",
        "Notification",
        "NotificationInput",
        "OnlineAgent",
        "Perspective",
        "PerspectiveHandle",
        "RuntimeInfo",
    ]
    .iter()
    .copied()
    .collect();

    let ai_set: std::collections::HashSet<&str> = ["AITask", "Model"].iter().copied().collect();

    // Primitives, built-ins, and locally-defined types to skip in imports
    let skip_types: std::collections::HashSet<&str> = [
        "never",
        "void",
        "unknown",
        "boolean",
        "string",
        // TS built-ins
        "Array",
        "Record",
        // Locally defined in this file (transcription types)
        "OpenTranscriptionRequest",
        "FeedTranscriptionRequest",
        "CloseTranscriptionRequest",
    ]
    .iter()
    .copied()
    .collect();

    for route in &routes {
        for type_str in &[route.request_type, route.response_type] {
            // Extract base type names (strip [], | null, Array<>, etc.)
            let base_types = extract_base_types(type_str);
            for bt in base_types {
                if skip_types.contains(bt.as_str())
                    || bt.starts_with("Record<")
                    || bt.starts_with("Array<")
                {
                    continue;
                }
                if domain_set.contains(bt.as_str()) {
                    domain_types.insert(bt);
                } else if ai_set.contains(bt.as_str()) {
                    ai_types.insert(bt);
                } else {
                    rest_types.insert(bt);
                }
            }
        }
    }

    // Import REST request/response types
    if !rest_types.is_empty() {
        ts.push_str("import type {\n");
        for t in &rest_types {
            ts.push_str(&format!("  {},\n", t));
        }
        ts.push_str("} from './index';\n\n");
    }

    // Import domain types
    if !domain_types.is_empty() {
        ts.push_str("import type {\n");
        for t in &domain_types {
            ts.push_str(&format!("  {},\n", t));
        }
        ts.push_str("} from '../../index';\n\n");
    }

    // Import AI types
    for t in &ai_types {
        if t == "AITask" {
            ts.push_str("import type { AITask } from '../../ai/Tasks';\n");
        } else if t == "Model" {
            ts.push_str("import type { Model } from '../../ai/AITypes';\n");
        }
    }
    if !ai_types.is_empty() {
        ts.push_str("\n");
    }

    // Transcription types (not yet exported via ts-rs)
    ts.push_str("// Transcription request types (not yet in ts-rs exports)\n");
    ts.push_str("export interface OpenTranscriptionRequest {\n");
    ts.push_str("  model_id: string;\n");
    ts.push_str("  params?: {\n");
    ts.push_str("    threshold?: number;\n");
    ts.push_str("    min_speech_duration_ms?: number;\n");
    ts.push_str("    min_silence_duration_ms?: number;\n");
    ts.push_str("    speech_pad_ms?: number;\n");
    ts.push_str("    max_speech_duration_s?: number;\n");
    ts.push_str("  };\n");
    ts.push_str("}\n\n");
    ts.push_str("export interface FeedTranscriptionRequest {\n");
    ts.push_str("  stream_id: string;\n");
    ts.push_str("  audio_base64: string;\n");
    ts.push_str("}\n\n");
    ts.push_str("export interface CloseTranscriptionRequest {\n");
    ts.push_str("  stream_id: string;\n");
    ts.push_str("}\n\n");

    // Generate the RouteMap
    ts.push_str("export interface RouteMap {\n");
    for route in &routes {
        ts.push_str(&format!(
            "  '{} {}': {{ request: {}; response: {} }};\n",
            route.method, route.path, route.request_type, route.response_type
        ));
    }
    ts.push_str("}\n\n");

    // Helper types
    ts.push_str("/** Extract the HTTP method from a route key */\n");
    ts.push_str("export type RouteMethod<K extends keyof RouteMap> = K extends `${infer M} ${string}` ? M : never;\n\n");
    ts.push_str("/** Extract the path from a route key */\n");
    ts.push_str("export type RoutePath<K extends keyof RouteMap> = K extends `${string} ${infer P}` ? P : never;\n\n");
    ts.push_str("/** All GET route keys */\n");
    ts.push_str("export type GetRoutes = { [K in keyof RouteMap]: K extends `GET ${string}` ? K : never }[keyof RouteMap];\n\n");
    ts.push_str("/** All POST route keys */\n");
    ts.push_str("export type PostRoutes = { [K in keyof RouteMap]: K extends `POST ${string}` ? K : never }[keyof RouteMap];\n\n");
    ts.push_str("/** All PUT route keys */\n");
    ts.push_str("export type PutRoutes = { [K in keyof RouteMap]: K extends `PUT ${string}` ? K : never }[keyof RouteMap];\n\n");
    ts.push_str("/** All PATCH route keys */\n");
    ts.push_str("export type PatchRoutes = { [K in keyof RouteMap]: K extends `PATCH ${string}` ? K : never }[keyof RouteMap];\n\n");
    ts.push_str("/** All DELETE route keys */\n");
    ts.push_str("export type DeleteRoutes = { [K in keyof RouteMap]: K extends `DELETE ${string}` ? K : never }[keyof RouteMap];\n");

    let out_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../core/src/generated/rest/routes.ts");
    std::fs::write(&out_path, &ts).unwrap_or_else(|e| {
        panic!("Failed to write {}: {}", out_path.display(), e);
    });
    println!("Generated {}", out_path.display());
}

/// Extract base type names from a TS type string like "Vec<Foo>[]" or "Foo | null"
fn extract_base_types(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    // Split on |, [], <>, whitespace
    let cleaned = s.replace("[]", " ").replace("|", " ").replace("null", " ");
    for part in cleaned.split(|c: char| !c.is_alphanumeric() && c != '_') {
        let part = part.trim();
        if !part.is_empty() && part.chars().next().unwrap().is_uppercase() {
            result.push(part.to_string());
        }
    }
    result
}
