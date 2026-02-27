# 9. Versioning & Compatibility

## 9.1 Protocol Versioning

The AD4M protocol uses **semantic versioning** (SemVer):

```
MAJOR.MINOR.PATCH
```

- **MAJOR** — Breaking changes to core data types, signing scheme, or required interfaces
- **MINOR** — New optional features, new GraphQL fields/types, new Language adapter interfaces
- **PATCH** — Clarifications, bug fixes in the spec, non-functional changes

**Current version:** 0.1.0 (Draft)

## 9.2 Compatibility Principles

### Wire Format Stability

1. New fields added to types SHOULD be optional (nullable).
2. Existing field types MUST NOT change in a MINOR version.
3. Implementations MUST ignore unknown fields in received data (forward compatibility).
4. Implementations MUST NOT rely on field ordering in JSON objects.

### GraphQL Schema Evolution

1. New queries, mutations, and subscriptions MAY be added in MINOR versions.
2. Existing queries/mutations MUST NOT change their required parameters in MINOR versions.
3. New optional parameters MAY be added to existing operations.
4. Return types MAY gain new optional fields.
5. Deprecated fields SHOULD be marked with `@deprecated` and maintained for at least one MINOR version.

### Language Interface Evolution

1. New optional adapter interfaces MAY be added (e.g., `TelepresenceAdapter` was added after `LinkSyncAdapter`).
2. Existing adapter method signatures MUST NOT change in MINOR versions.
3. New optional methods MAY be added to existing adapters (implementations that don't support them should not break).

## 9.3 Compatibility Matrix

| Component | Version Coupling | Notes |
|-----------|-----------------|-------|
| Expression signing | MAJOR | Changes break all signature verification |
| Link/Expression types | MAJOR | Core data model changes break interop |
| GraphQL API | MINOR | New fields/operations don't break existing clients |
| SDNA SHACL shapes | MINOR | New shapes are additive |
| Language interfaces | MINOR | New adapters are optional |
| P-Diff-Sync DNA | Implementation-defined | DNA changes require new Neighbourhoods |
| Bootstrap Languages | Implementation-defined | Can be swapped for alternatives |

## 9.4 Migration Strategy

### Neighbourhood Migration

When a breaking change affects Link Languages:
1. New Neighbourhoods MUST use the updated Link Language
2. Existing Neighbourhoods continue operating with their installed Link Language
3. Migration of existing Neighbourhoods requires coordinated action by all participants

### Agent Migration

If the DID method or signing scheme changes:
1. Agents generate new identity under the new scheme
2. Entanglement proofs can link old and new identities
3. Existing expressions signed with the old scheme remain verifiable as long as the old verification code is available

## 9.5 Implementation Version Reporting

Implementations MUST report their version via the `runtimeInfo` query:

```graphql
type RuntimeInfo {
  ad4mExecutorVersion: String!   # Implementation version
  isInitialized: Boolean!
  isUnlocked: Boolean!
}
```

> **TBD:** A protocol version field should be added to `RuntimeInfo` to distinguish protocol version from implementation version. This is recommended for future versions of this specification.

## 9.6 Feature Detection

Since not all implementations will support all features, clients SHOULD use feature detection:

1. Check for optional GraphQL fields/types via introspection
2. Check `neighbourhoodHasTelepresenceAdapter` before using telepresence features
3. Handle graceful degradation when optional features are unavailable
