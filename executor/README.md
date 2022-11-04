# AD4M Executor (= run-time)

## What's AD4M?

AD4M (_Agent-Centric Distributed Application Meta-Ontology_) is an abstract interface definition that tries to distill the quintessence of agent-centric software architecture such that applications can choose to become interoprable by building components congruent to this interface. It introduces a simple and powerful ontology consisting of three entities

1. **Agent** (=user/human)
2. **Language** (=space of expressions / combining storage and UI on an abstract level)
3. **Perspective** (=what an agent perceives / spaces of links (triplets) between expressions)

See [the AD4M repository](https://github.com/perspect3vism/ad4m/blob/main/README.md) for a more detailed description as well as TypeScript classes and interfaces.

## Why a Node.js Executor For AD4M?

Using this as the local back-end / middleware, all that's left to do in order to build an AD4M based / compatible application is writing a UI that connects to this AD4M executor via it's [GraphQL interface](src/core/graphQL-interface/GraphQL.ts) and potentially adding custom AD4M Languages. See [Perspect3ve](https://github.com/perspect3vism/perspect3ve) for a working example.

So this package can be used for starting an AD4M node.js runtime.

`main.js` will listen for calls from the host on the event `init`. Upon receiving this event it will spawn the AD4M runtime; this runtime is interfaced with and managed via a locally running graphql server. Information on the topology of this server can be found [here](./src/core/graphQL-interface/GraphQL.ts).

More information about the AD4M ontology and scope can be found [here](https://github.com/perspect3vism/perspect3ve/tree/master/src/ad4m).

## Building

```
yarn install
yarn run build
```

## Testing

```
yarn run test
```
