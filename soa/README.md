# State of Affairs (SoA) — AD4M Ontology

A universal ontology for representing knowledge, beliefs, intentions, and plans as structured trees within AD4M perspectives.

## Overview

A **State of Affairs** (SoA) is a proposition about how things are, could be, or should be. It is the fundamental unit of knowledge representation in this ontology.

Every node in a SoA tree is a `StateOfAffair` with a **modality** that describes its epistemic status:

| Modality | Meaning | Example |
|---|---|---|
| `belief` | Something we hold as true | "Holochain handles P2P sync" |
| `observation` | Something directly verified | "CI is green on commit abc123" |
| `intention` | Something we want to make true | "AD4M has MCP support" |
| `vision` | A high-level desired future | "Distributed collective intelligence" |
| `plan` | Steps to reach an intended SoA | "Merge branches → release → prototype" |
| `skill` | A capability an agent has | "Can query perspectives via MCP" |

## Relationships

Relationships between SoA nodes are expressed as **standard AD4M links** using the `soa` language for predicates. No separate model needed — link expressions natively carry author and timestamp.

| Predicate | Meaning |
|---|---|
| `soa://rel_supports` | Evidence or argument for |
| `soa://rel_contradicts` | Evidence or argument against |
| `soa://rel_similar` | Related but not identical |
| `soa://rel_same` | Equivalent propositions |
| `soa://rel_requires` | Dependency (target requires source) |
| `soa://rel_enables` | Capability (source enables target) |
| `soa://rel_parent` | Tree structure (source is parent of target) |
| `soa://rel_refines` | More specific version of |
| `soa://rel_blocks` | Prevents or blocks |

The `soa` language is a static language — these expressions resolve to documentation describing the relationship semantics.

## Authorship & Provenance

SoA nodes do **not** store author or timestamp fields. Every link that constitutes a SoA shape is an AD4M `LinkExpression` which natively includes:
- `author` — the DID of the agent who created the link
- `timestamp` — when the link was created

A single SoA can have multiple authors — when different agents independently create the same state of affairs, the link graph naturally represents this.

## Structure

```text
soa/
├── README.md           # This file
├── src/                # TypeScript model classes
│   ├── index.ts        # Exports + relationship predicate constants
│   └── StateOfAffair.ts
├── language/           # Static SoA language (ReadOnlyLanguage)
│   └── src/
│       ├── index.ts        # Language entry point
│       ├── adapter.ts      # ExpressionAdapter — resolves soa:// to docs
│       └── expressions.ts  # Registry of all soa:// expressions + docs
└── package.json
```

SHACL shapes are auto-generated from the `Ad4mModel` decorators — no separate schema files needed. The TypeScript classes ARE the source of truth.

## Usage

### TypeScript (UI / Flux)
```typescript
import { StateOfAffair, SoA } from '@coasys/soa';

// Create a belief
const belief = new StateOfAffair();
belief.title = "SHACL is the source of truth for subject classes";
belief.modality = "belief";
belief.confidence = 0.95;

// Create an intention
const goal = new StateOfAffair();
goal.title = "Migrate Data's memory to AD4M perspectives";
goal.modality = "intention";

// Link them with a relationship (just an AD4M link!)
perspective.addLink({
  source: belief.baseExpression,
  predicate: SoA.REL_SUPPORTS,
  target: goal.baseExpression,
});

// Tree structure
perspective.addLink({
  source: parentSoA.baseExpression,
  predicate: SoA.REL_PARENT,
  target: childSoA.baseExpression,
});
```

### MCP / Programmatic Access
SoA instances can be created via MCP tools using the auto-generated SHACL shape. Relationships are just `add_link` calls with `soa://rel_*` predicates.

## Design Principles

1. **No flags** — the graph shape + `soa://` namespace is sufficient for type identification
2. **No separate relationship model** — relationships are native AD4M links with typed predicates
3. **No author/timestamp fields** — link expressions handle provenance natively
4. **Works for humans AND agents** — same schema for personal memory, shared task boards, Eve's worldview
5. **Trees AND graphs** — `rel_parent` gives hierarchy; other predicates give cross-links
6. **Fractal** — same pattern at individual, group, and network levels
7. **Evolvable** — new modalities and relationship predicates can be added without breaking existing trees

## The `soa` Language

The `soa` language is a **static ReadOnlyLanguage** — it defines fixed expressions that resolve to structured documentation. When any agent (human or AI) resolves a `soa://` URL, they get back a rich object explaining what it means and how to use it:

```typescript
// Resolving soa://rel_supports returns:
{
  "@type": "SoADocumentation",
  "address": "soa://rel_supports",
  "name": "Supports",
  "kind": "relationship",
  "description": "Source provides evidence, argument, or backing for target...",
  "example": "link(evidence_soa, \"soa://rel_supports\", claim_soa)",
  "agentNotes": "Directional: A supports B means A is evidence FOR B..."
}
```

This makes the ontology **self-documenting and discoverable**. AI agents encountering `soa://` predicates for the first time can resolve them to understand the ontology without external documentation.

### Pattern for other languages

The soa language serves as a template for how AD4M languages can act as **self-documenting semantic vocabularies**. Any domain can create a similar static language to define its own predicates — just map addresses to documentation objects.

Implementation: `soa/language/src/`

## Connection to Eve

In the Eve vision, every agent (human or AI) builds their own SoA tree — their worldview. Shared neighbourhoods allow SoA subtrees to be synchronized, enabling distributed consensus through Eve-to-Eve communication. The same ontology powers personal memory, collaborative task tracking, and collective intelligence.
