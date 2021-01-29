## Node.js Executor For Acai

This package is intended to be used inside the Junto mobile application as a method of communication with the Acai ontology and thus Junto's Acai languages & user perspectives.

`main.js` will listen for calls from the host on the event `init`. Upon receiving this event it will spawn the Acai runtime; this runtime is interfaced with and managed via a locally running graphql server. Information on the topology of this server can be found [here](./src/core/graphQL-interface/GraphQL.ts).

[LiquidCore](https://github.com/LiquidPlayer/LiquidCore) is the technology used in order to run this node.js package in both android & iOS. Information about how to build for different platforms as well as the method of communication with said runtime can be found on the projects README. 

More information about the Acai ontology and scope can be found [here](https://github.com/lucksus/perspectivism).

### Building

```
npm i
npm run build
```

Running the above command will output `main.js` file inside the `build` directory. Further steps are required to get this running the mobile application.

### Status

This project in its current state is most definitely not working. Whilst the project will compile and run in a tradional node runtime; testing it inside both android & iOS has not been completed as of yet. Once we can confirm the execution of this package in android and iOS; we can proceed to add the languages required to make this package valid middleware for Junto's backend communication.