## Node.js Executor For Ad4m

This package can be used for starting an AD4M node.js runtime.

`main.js` will listen for calls from the host on the event `init`. Upon receiving this event it will spawn the AD4M runtime; this runtime is interfaced with and managed via a locally running graphql server. Information on the topology of this server can be found [here](./src/core/graphQL-interface/GraphQL.ts).

More information about the AD4M ontology and scope can be found [here](https://github.com/perspect3vism/perspect3ve/tree/master/src/ad4m).

### Building

```
npm i
npm run build
```