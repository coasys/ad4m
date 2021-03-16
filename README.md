## Node.js Executor For Ad4m

This package can be used for starting an AD4M node.js runtime.

`main.js` will listen for calls from the host on the event `init`. Upon receiving this event it will spawn the AD4M runtime; this runtime is interfaced with and managed via a locally running graphql server. Information on the topology of this server can be found [here](./src/core/graphQL-interface/GraphQL.ts).

More information about the AD4M ontology and scope can be found [here](https://github.com/perspect3vism/perspect3ve/tree/master/src/ad4m).

### Building

```
npm i
npm run build
```

Running the above command will output `main.js` file inside the `build` directory. Further steps are required to get this running the mobile application.

### Status

This project in its current state is most definitely not working. Whilst the project will compile and run in a tradional node runtime; testing it inside both android & iOS has not been completed as of yet. Once we can confirm the execution of this package in android and iOS; we can proceed to add the languages required to make this package valid middleware for Junto's backend communication.