# 9. Versioning & Compatibility

## 9.1 Protocol Versioning

The AD4M protocol uses **semantic versioning** (SemVer):

```text
MAJOR.MINOR.PATCH
```

- **MAJOR** — Breaking changes to core data types, signing scheme, or required interfaces
- **MINOR** — New optional features, new RPC operations/event types, new Language adapter interfaces
- **PATCH** — Clarifications, bug fixes in the spec, non-functional changes

**Current version:** 1.0.0 (Draft)

## 9.2 Compatibility Principles

### Wire Format Stability

1. New fields added to types SHOULD be optional (nullable).
2. Existing field types MUST NOT change in a MINOR version.
3. Implementations MUST ignore unknown fields in received data (forward compatibility).
4. Implementations MUST NOT rely on field ordering in JSON objects.

### RPC API Evolution

1. New RPC operations MAY be added in MINOR versions.
2. Existing RPC operation parameters MUST NOT change in MINOR versions.
3. New optional parameters MAY be added to existing operations.
4. New event types MAY be added in MINOR versions.
5. Return types MAY gain new optional fields.

### Language Interface Evolution

1. The v1.0 Language interface uses a **flat export model** — Languages export named functions at the module level.
2. New optional capability exports MAY be added in MINOR versions.
3. Existing capability export signatures MUST NOT change in MINOR versions.
4. New optional imports MAY be added to the runtime services.
5. The WIT file (`ad4m-lang.wit`) is the canonical interface definition; new versions of the WIT SHOULD follow WIT versioning conventions (package version in the `package` declaration).
6. Languages that target a specific WIT world (`ad4m-language`, `ad4m-language-holochain`, etc.) declare their extension requirements; runtimes that don't support a required extension refuse to load the Language.
7. The `create(context)` factory and adapter-class model MUST NOT be used for Language loading.

## 9.3 Compatibility Matrix

| Component | Version Coupling | Notes |
|-----------|-----------------|-------|
| Expression signing | MAJOR | Changes break all signature verification |
| Link/Expression types | MAJOR | Core data model changes break interop |
| WebSocket RPC API | MINOR | New operations don't break existing clients |
| SDNA SHACL shapes | MINOR | New shapes are additive |
| Language interface (WIT) | MINOR | New capabilities are optional exports |
| P-Diff-Sync DNA | Implementation-defined | DNA changes require new Neighbourhoods |
| Bootstrap Languages | Implementation-defined | Can be swapped for alternatives |
| SPARQL store schema | Implementation-defined | Reifier model is stable; query functions may evolve |

## 9.4 Migration Strategy

### Neighbourhood Migration

When a breaking change affects Link Languages:
1. New Neighbourhoods MUST use the updated Link Language
2. Existing Neighbourhoods continue operating with their installed Link Language
3. Migration of existing Neighbourhoods requires coordinated action by all participants

### Agent Migration

If the DID method or signing scheme changes:
1. Agents generate new identity under the new scheme
2. Entanglement proofs can link identities across schemes
3. Existing expressions remain verifiable as long as the verification code for their signing scheme is available

## 9.5 Implementation Version Reporting

Implementations MUST report their version via the `runtime.info` RPC operation:

```typescript
interface RuntimeInfo {
  ad4mExecutorVersion: string;   // Implementation version
  isInitialized: boolean;
  isUnlocked: boolean;
}
```

> **TBD:** A protocol version field should be added to `RuntimeInfo` to distinguish protocol version from implementation version. This is recommended for future versions of this specification.

## 9.6 Feature Detection

Since not all implementations support all features, clients SHOULD use feature detection:

1. Clients SHOULD probe for operation support by calling the operation and handling `501 Not Implemented` errors gracefully.
2. Check `neighbourhood.hasTelepresence` before using telepresence features.
3. Handle graceful degradation when optional features are unavailable.
4. Use `runtime.info` to determine the executor version and infer supported feature sets.
