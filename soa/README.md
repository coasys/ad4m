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

SoA nodes connect to each other via typed relationships:

| Relationship | Meaning |
|---|---|
| `supports` | Evidence or argument for |
| `contradicts` | Evidence or argument against |
| `similar` | Related but not identical |
| `same` | Equivalent propositions |
| `requires` | Dependency (B requires A) |
| `enables` | Capability relationship (A enables B) |
| `parent` / `child` | Tree composition |

## Structure

```
soa/
├── README.md           # This file
├── src/                # TypeScript model classes (for UIs, Flux apps)
│   ├── index.ts
│   ├── StateOfAffair.ts
│   └── Relationship.ts
├── schemas/            # JSON schemas (for MCP, programmatic access)
│   ├── StateOfAffair.json
│   └── Relationship.json
└── package.json
```

## Usage

### TypeScript (UI / Flux)
```typescript
import { StateOfAffair, SoARelationship } from '@coasys/soa';

// Create a belief
const belief = new StateOfAffair();
belief.title = "SHACL is the source of truth for subject classes";
belief.modality = "belief";
belief.confidence = 0.95;

// Create an intention
const goal = new StateOfAffair();
goal.title = "Migrate Data's memory to AD4M perspectives";
goal.modality = "intention";

// Link them
const rel = new SoARelationship();
rel.type = "supports";
rel.source = belief.baseExpression;  
rel.target = goal.baseExpression;
```

### JSON Schema (MCP / programmatic)
```json
{
  "title": "StateOfAffair",
  "properties": {
    "title": { "type": "string" },
    "modality": { "type": "string", "enum": ["belief", "observation", "intention", "vision", "plan", "skill"] },
    "description": { "type": "string" },
    "confidence": { "type": "number" }
  }
}
```

## Design Principles

1. **Universal base class** — `StateOfAffair` is the super-class; modalities are subtypes
2. **Works for humans AND agents** — same schema for personal memory, shared task boards, Eve's worldview
3. **Trees AND graphs** — parent/child gives hierarchy; relationships give cross-links
4. **Fractal** — same pattern at individual, group, and network levels
5. **Evolvable** — new modalities and relationship types can be added without breaking existing trees

## Connection to Eve

In the Eve vision, every agent (human or AI) builds their own SoA tree — their worldview. Shared neighbourhoods allow SoA subtrees to be synchronized, enabling distributed consensus through Eve-to-Eve communication. The same ontology powers personal memory, collaborative task tracking, and collective intelligence.
