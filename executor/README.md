# AD4M Executor (= JS run-time)

## What's AD4M?

AD4M (_Agent-Centric Distributed Application Meta-Ontology_) is an abstract interface definition that tries to distill the quintessence of agent-centric software architecture such that applications can choose to become interoprable by building components congruent to this interface. It introduces a simple and powerful ontology consisting of three entities

1. **Agent** (=user/human)
2. **Language** (=space of expressions / combining storage and UI on an abstract level)
3. **Perspective** (=what an agent perceives / spaces of links (triplets) between expressions)

See [the AD4M repository](https://github.com/coasys/ad4m/blob/main/README.md) for a more detailed description as well as TypeScript classes and interfaces.

## Modules Use

This module is used to manage installed AD4M Languages via the `LanguageController`; code [here](https://github.com/coasys/ad4m/blob/main/executor/src/core/LanguageController.ts), maintain an agents internal Perspectives via the `PerspectivesController`; code [here](https://github.com/coasys/ad4m/blob/main/executor/src/core/PerspectivesController.ts), and a local database of links for each `Perspective` via the `db`; code [here](https://github.com/coasys/ad4m/blob/main/executor/src/core/db.ts).

It exposes functions for core AD4M Operations via: [`GraphQL.ts`](https://github.com/coasys/ad4m/blob/rust-refactor/executor/src/core/graphQL-interface/GraphQL.ts); which routes function calls (expected to be coming from rust-executor) to their appropriate controllers for handling.

## Building

```
pnpm install
pnpm run build
```

## Testing

```
pnpm run test
```
