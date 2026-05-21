# 5. Social DNA (SHACL + SPARQL)

**Social DNA (SDNA)** is the schema layer of AD4M. It defines typed entities — Subject Classes — over the link graph using [W3C SHACL](https://www.w3.org/TR/shacl/), and exposes a [SPARQL 1.1](https://www.w3.org/TR/sparql11-query/) query surface for retrieving them.

SDNA is the *wire-level interoperability layer* between AD4M applications: two apps that agree on SHACL shapes can read and write each other's data even though they were written in different languages, framework versions, or against different executors. SDNA is therefore on the critical path for cross-implementation conformance — see [§7.2.8 in the original interop tally, now](./11-conformance.md).

This section specifies (a) the SHACL JSON wire format, (b) how SHACL shapes are encoded as links inside a Perspective, (c) the SPARQL semantics over the reifier storage model, and (d) the AD4M custom SPARQL functions.

> Decorator-based authoring APIs (TypeScript `@Model`, `@Property`, etc.) and the `Ad4mModel` ORM are **not** part of the protocol — they are a TypeScript developer convenience that compiles down to the SHACL JSON specified here. See [`../docs-src/pages/`](../docs-src/pages/) for the developer-facing API.

## 5.1 SHACL shape JSON

A SHACL `NodeShape` is exchanged over RPC and stored as a JSON object. The shape is the canonical wire form: every executor MUST accept it on `perspective.addSubjectClass` / `perspective.addSdna` (with `shaclJson`), and MUST be able to produce it via `perspective.getSubjectShape`.

```typescript
interface SHACLShapeJSON {
  node_shape_uri: string;        // IRI of the NodeShape node itself, e.g. "shacl://Recipe"
  target_class: string;          // IRI of the typed entity, e.g. "recipe://Recipe"
  parent_shapes?: string[];      // IRIs of sh:node parents (model inheritance)
  properties: SHACLPropertyJSON[];
  constructor_actions?: Action[];   // run on instance creation
  destructor_actions?: Action[];    // run on instance deletion
}

interface SHACLPropertyJSON {
  path: string;                  // predicate URI for the link (sh:path)
  name?: string;                 // friendly name; used to derive property-shape IRI
  datatype?: string;             // e.g. "xsd:string", "xsd:integer"
  node_kind?: string;            // e.g. "IRI", "Literal"
  min_count?: number;            // sh:minCount
  max_count?: number;            // sh:maxCount (1 = scalar; absent = collection)
  min_inclusive?: number;        // sh:minInclusive
  max_inclusive?: number;        // sh:maxInclusive
  pattern?: string;              // sh:pattern (regex)
  has_value?: string;            // sh:hasValue (fixed value / flag predicate)
  local?: boolean;               // ad4m:local — link is local-only, not shared
  writable?: boolean;            // ad4m:writable — UI/app hint, not a constraint
  resolve_language?: string;     // expression Language used to resolve target values
  setter?: Action[];             // override default link-write actions
  adder?: Action[];              // for collection properties
  remover?: Action[];            // for collection properties
  getter?: string;               // computed-value expression (engine-specific)
  conformance_conditions?: ConformanceCondition[];
  class?: string;                // sh:class — target is an instance of this class
  in?: string[];                 // sh:in — enum constraint
}

interface Action {
  action: "addLink" | "removeLink" | "setSingleTarget" | "collectionSetter";
  source: string;                // URI or "this" (the instance being acted on)
  predicate: string;
  target: string;
  local?: boolean;
}

interface ConformanceCondition {
  type: "flag" | "required";
  predicate: string;
  value?: string;                // for flag conditions
}
```

Field names in the SHACL JSON wire form use **snake_case** as enumerated above.

Reference: [`core/src/shacl/SHACLShape.ts`](../core/src/shacl/SHACLShape.ts) `toJSON()`, [`rust-executor/src/perspectives/shacl_parser.rs`](../rust-executor/src/perspectives/shacl_parser.rs).

## 5.2 SHACL link encoding

When SDNA is added to a Perspective, the SHACL shape is decomposed into a sub-graph of links rooted at well-known predicates. This sub-graph travels with the Perspective when synced and is queryable as ordinary RDF.

### 5.2.1 Registration

Every SDNA entry begins with one link from `ad4m://self`:

| Predicate | Target | Meaning |
|---|---|---|
| `ad4m://has_subject_class` | `literal:string:<Name>` | Declares a Subject Class |
| `ad4m://has_flow`          | `literal:string:<Name>` | Declares a Flow |
| `ad4m://has_custom_sdna`   | `literal:string:<Name>` | Declares a custom (non-SHACL) SDNA fragment |

Plus a body link carrying the stringified SHACL JSON for the entry:

| Source | Predicate | Target |
|---|---|---|
| `literal:string:<Name>` | `ad4m://sdna` | `literal:string:<stringified shape JSON>` |

### 5.2.2 NodeShape encoding

For a Subject Class named `Foo` with `target_class = "foo://Foo"`, the SHACL parser writes the following triples (in addition to the registration link above):

```
<foo://Foo>     rdf://type            <ad4m://SubjectClass>
<foo://Foo>     ad4m://shape          <shacl://Foo>
<shacl://Foo>   rdf://type            <sh://NodeShape>
<shacl://Foo>   sh://targetClass      <foo://Foo>
<shacl://Foo>   ad4m://constructor    literal:string:<JSON-encoded constructor_actions>     # if non-empty
<shacl://Foo>   ad4m://destructor     literal:string:<JSON-encoded destructor_actions>      # if non-empty
```

### 5.2.3 PropertyShape encoding

For each property `prop` of the NodeShape, the parser writes a property-shape sub-graph rooted at `<shape_uri>.<prop_name>`:

```
<shacl://Foo>           sh://property      <shacl://Foo.bar>
<shacl://Foo.bar>       rdf://type         <sh://PropertyShape>   # or <ad4m://CollectionShape>
<shacl://Foo.bar>       sh://path          <foo://has_bar>
<shacl://Foo.bar>       sh://datatype      <xsd:string>           # if set
<shacl://Foo.bar>       sh://minCount      literal:number:1       # if set
<shacl://Foo.bar>       sh://maxCount      literal:number:1       # if set
<shacl://Foo.bar>       sh://hasValue      literal:string:<value> # if set (flag properties)
<shacl://Foo.bar>       sh://class         <other://OtherClass>   # if set (typed reference)
<shacl://Foo.bar>       sh://in            literal:json:[...]     # if set (enum)
<shacl://Foo.bar>       ad4m://local       literal:boolean:true   # if set
<shacl://Foo.bar>       ad4m://writable    literal:boolean:true   # if set
<shacl://Foo.bar>       ad4m://resolveLanguage  literal:string:<lang>  # if set
<shacl://Foo.bar>       ad4m://initial     literal:string:<value> # if set
```

The split between `sh://PropertyShape` and `ad4m://CollectionShape` mirrors the `max_count` distinction: scalar properties (`max_count: 1`) use `sh://PropertyShape`; multi-value collections use `ad4m://CollectionShape`.

### 5.2.4 Round-trip

A conforming executor MUST be able to:

- Accept a SHACL JSON shape via `perspective.addSubjectClass` and persist it as the link sub-graph above.
- Reconstruct a SHACL JSON shape from the link sub-graph (used by `perspective.getSubjectShape` and clients that received only sync'd links).
- Preserve any extension predicates it doesn't understand (forward-compat).

## 5.3 Custom SDNA

For schema content that doesn't fit SHACL — application-defined logic, alternative schema languages, etc. — `ad4m://has_custom_sdna` declares an opaque SDNA entry whose body is held in the `ad4m://sdna` link. Such entries MUST be preserved by executors during sync but MAY be ignored for query purposes. Supporting custom SDNA processing is OPTIONAL.

## 5.4 SPARQL query semantics

A conforming executor MUST support **SPARQL 1.1** queries over the link graph via `perspective.querySparql` and `perspective.modelQuery`.

The reference executor uses [Oxigraph](https://oxigraph.org/); any SPARQL 1.1 engine with RDF 1.2 triple-term support is acceptable.

### 5.4.1 Storage model

Queries run over the reifier storage model defined in [§2.10](./02-core-data-model.md#210-link-storage-model--rdf-12-reifiers). Briefly: each LinkExpression contributes a direct triple `<s> <p> <o>` to the default graph, plus a reifier `<link:HASH>` carrying six metadata predicates.

### 5.4.2 Read-only enforcement

All query operations MUST reject update-style SPARQL forms (`INSERT`, `DELETE`, `LOAD`, etc.) before evaluation. Only `SELECT`, `ASK`, `CONSTRUCT`, and `DESCRIBE` are permitted. Reference: `validate_readonly_query` in [`sparql_store.rs`](../rust-executor/src/perspectives/sparql_store.rs).

### 5.4.3 Default prefixes

A conforming engine SHOULD pre-register the following prefixes so common queries need no `PREFIX` clauses:

```
PREFIX rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
PREFIX sh:   <http://www.w3.org/ns/shacl#>
PREFIX ad4m: <ad4m://>
```

### 5.4.4 Custom AD4M SPARQL functions

The protocol defines a small library of custom functions, callable in SPARQL by their full IRI. Conforming executors SHOULD implement all of them; the AD4M URL form ensures portability across engines.

| Function IRI | Behaviour |
|---|---|
| `<ad4m://fn/parse_literal>` | Takes a `literal:` URI and returns the decoded value as a SPARQL literal of the corresponding XSD type. For non-`literal:` inputs, returns the input unchanged. |
| `<ad4m://fn/strip_html>`    | Takes a string, returns the same string with HTML tags removed. |

Example use:

```sparql
SELECT ?todo ?title WHERE {
  ?todo <todo://has_title> ?rawTitle .
  BIND(<ad4m://fn/parse_literal>(?rawTitle) AS ?title)
  FILTER(STRSTARTS(STR(?title), "Buy"))
}
```

Reference: [`sparql_store.rs:32-90`](../rust-executor/src/perspectives/sparql_store.rs).

## 5.5 Reading SDNA-typed instances

For an application to read instances of a Subject Class given only its name (no Subject-Class-specific code), the typical flow is:

1. Read the SHACL shape: `perspective.getSubjectShape(className)` returns the JSON described in §5.1.
2. Construct a SPARQL query using the property paths (`sh:path`) declared in the shape.
3. Execute via `perspective.querySparql`.
4. Decode `literal:` results via `<ad4m://fn/parse_literal>` (preferable, in-engine) or client-side via the `Literal.fromUrl()` helper.

This procedure is what enables MCP servers, foreign-language clients, and tooling to interact with arbitrary app data with no prior knowledge of the app.

## 5.6 Flows (state machines over Subject Classes)

Flows define state machines over Subject Class instances. They are stored under the `ad4m://has_flow` registration predicate using a sub-graph analogous to NodeShapes:

```
<flowUri> rdf://type        <ad4m://Flow>
<flowUri> ad4m://flowName   literal:string:<Name>
<flowUri> ad4m://flowable   ad4m://any  | literal:string:<LinkPattern JSON>
<flowUri> ad4m://startAction literal:string:<AD4MAction[] JSON>
<flowUri> ad4m://hasState   <stateUri>     # per state
<flowUri> ad4m://hasTransition <transitionUri>   # per transition
```

Per-state sub-graph:

```
<stateUri> rdf://type         <ad4m://FlowState>
<stateUri> ad4m://stateName   literal:string:<name>
<stateUri> ad4m://stateValue  literal:number:<value>
<stateUri> ad4m://stateCheck  literal:string:<LinkPattern JSON>
```

Per-transition sub-graph:

```
<transitionUri> rdf://type                <ad4m://FlowTransition>
<transitionUri> ad4m://actionName         literal:string:<name>
<transitionUri> ad4m://fromState          <stateUri>
<transitionUri> ad4m://toState            <stateUri>
<transitionUri> ad4m://transitionActions  literal:string:<AD4MAction[] JSON>
```

Implementations MAY defer full flow execution support; in that case they MUST still preserve `ad4m://has_flow` links during sync so other clients can drive the flow. The detailed runtime semantics (state evaluation, transition execution, available-flow filtering) are not part of the wire protocol — they belong in client libraries — but the link-encoded representation above is normative.

Reference: [`core/src/shacl/SHACLFlow.ts`](../core/src/shacl/SHACLFlow.ts).
