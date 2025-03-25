[![Project](https://img.shields.io/badge/Project-AD4M-brightgreen.svg)](http://ad4m.dev/)
[![Docs](https://img.shields.io/badge/Docs-AD4M-blue.svg)](http://docs.ad4m.dev/)
[![License: CAL 1.0](https://img.shields.io/badge/License-CAL%201.0-blue.svg)](https://github.com/holochain/cryptographic-autonomy-license)
[![Discord](https://img.shields.io/discord/894280941516652564?label=Discord)](https://discord.com/invite/fYGVM66jEz)
[![Twitter Follow](https://img.shields.io/twitter/follow/ad4m_layer?style=social)](https://x.com/ad4m_layer)

# AD4M: Agent-Centric Distributed Application Meta-ontology

<div align="center">
  <img src="/docs/public/images/ad4m-spanning-layer.jpg" alt="AD4M Banner">
</div>

## Vision

AD4M is a revolutionary spanning layer that extends the internet stack to enable true collective intelligence in a fully distributed way. Just as TCP/IP created a universal protocol for machines to communicate, AD4M creates a universal protocol for agents (humans and their devices) to make meaning together.

This new layer is needed because:
- The current web is fragmented into data silos and walled gardens
- We lack a universal way to connect meaning across platforms and protocols
- Collective intelligence requires sovereignty and interoperability
- The future of human collaboration needs agent-centric architecture

AD4M solves these challenges by:
- Creating a semantic overlay across all existing protocols
- Enabling any storage or communication method through pluggable Languages
- Treating all data as agent-authored expressions with verifiable provenance
- Building meaning through shared perspectives and social DNA
- Providing a foundation for truly distributed collective intelligence

Think of AD4M as the missing piece in the internet stack – one that transcends mere data exchange to enable meaningful collaboration between sovereign agents, regardless of the underlying protocols or platforms they use.

## Architecture & Execution Strategy

AD4M represents a sophisticated agent-centric node – a "second brain" that runs on the user's local machine. Unlike traditional web applications that rely on central servers, AD4M puts powerful server capabilities directly in the hands of users:

### Local-First Sovereign Node

Each AD4M instance is a full-featured data node that:
- Runs entirely on the user's machine
- Maintains the agent's digital identity and keys
- Stores and manages their semantic data
- Connects to other agents through various protocols
- Acts as their sovereign compute environment

### Technical Sophistication

AD4M integrates several powerful technologies into a cohesive whole:
- [Holochain](https://github.com/holochain/holochain): For distributed hash tables and p2p networking 
- [Deno & V8](https://github.com/denoland/deno): For secure JavaScript/TypeScript execution
- [Scryer-Prolog](https://github.com/mthom/scryer-prolog): For semantic reasoning and queries
- [Juniper](https://github.com/graphql-rust/juniper): For GraphQL API capabilities
- [Kalosm](https://github.com/floneum/floneum): For AI model inference with Candle
- [rustql](https://github.com/rust-corpus/rustql): For local data persistence

This complexity is necessary to provide a rich, sovereign computing environment – but it's all packaged to run smoothly on personal devices.

### Self-Recursive Bootstrap

AD4M achieves extensibility through a clever self-recursive design:
1. The three core concepts (Agents, Languages, Perspectives) are themselves implemented as Languages
2. This means the very foundations of AD4M can be extended and evolved
3. New implementations of these core Languages can be created and adopted
4. The system becomes an evolvable, living network

This architectural pattern enables AD4M to grow into a true "global brain" – a distributed intelligence layer that can adapt and evolve without central coordination.

## Key Concepts

### 1. Languages: Universal Protocol Adapters

Languages in AD4M are pluggable protocols that define how information is stored and shared. They create a spanning layer across all existing web protocols and storage systems:

```typescript
// Languages can wrap any protocol or storage system
const ipfsLanguage = "QmIPFSHash";   // Store on IPFS
const solidLanguage = "QmSolidHash"; // Store on Solid pods
const webLanguage = "https";         // Regular web URLs

// Create and share data through any Language
const expression = await ad4m.expression.create(
  { text: "Hello World!" },
  ipfsLanguage
);
// Returns: QmIPFSHash://unique-address
```

### 2. Expressions: Agent-Authored Data

Every piece of data in AD4M is an Expression – a cryptographically signed statement by an agent. This creates a web of verifiable claims rather than "objective" data:

```typescript
// Expressions are always signed by their author
const expression = await ad4m.expression.get("QmHash123://post789");
console.log(expression);
/* {
  author: "did:key:z6Mk...",     // Who made this claim
  timestamp: "2024-03-21...",    // When it was made
  data: { text: "Hello!" },      // The actual content
  proof: {                       // Cryptographic proof
    signature: "...",
    valid: true
  }
} */
```

### 3. Perspectives: Semantic Meaning-Making

Perspectives are agent-centric semantic graphs that give meaning to Expressions through links. They enable:
- Personal and shared views of information
- Semantic relationships between any pieces of data
- Collaborative meaning-making in shared spaces

```typescript
// Create semantic relationships between any expressions
await perspective.add({
  source: "did:key:alice",              // Subject
  predicate: "foaf://knows",            // Relationship type
  target: "did:key:bob"                 // Object
});

// Query based on meaning
const friends = await perspective.get({
  predicate: "foaf://knows"             // Find all friendship links
});
```

### 4. Social DNA: Collective Intelligence Patterns

Social DNA defines interaction patterns and social contracts that can be shared and reused across applications. It includes:
- Subject Classes: Define semantic object types
- Flows: Define possible state transitions
- Collections: Define relationship patterns
- Shared semantics for social applications

```typescript
// Define a reusable social pattern
@ModelOptions({ name: "Post" })
class Post extends Ad4mModel {
  @Property({ through: "social://content" })
  content: string;
  
  @Collection({ through: "social://comments" })
  comments: string[];
  
  @Property({ through: "social://state" })
  state: "draft" | "published" | "archived";
}

// Use in any application
const post = await perspective.createSubject(Post);
await post.publish("Hello World!");
```

These concepts work together to create a new kind of internet – one where meaning flows freely between sovereign agents while maintaining cryptographic verifiability and semantic richness.

## Getting Started

### Prerequisites

#### Core Dependencies
- **Rust** (1.84.0 or later)
  ```bash
  rustup install 1.84.0
  rustup default 1.84.0
  rustup target add wasm32-unknown-unknown
  ```
- **Go** (1.22.0 or later)
  ```bash
  # Follow instructions at https://go.dev/doc/install
  ```

#### Platform-Specific Dependencies

**macOS**:
```bash
brew install protobuf cmake
```

**Linux (Ubuntu/Debian)**:
```bash
sudo apt-get update
sudo apt-get install -y \
  libgtk-3-dev webkit2gtk-4.0 libappindicator3-dev \
  librsvg2-dev patchelf protobuf-compiler cmake \
  fuse libfuse2 mesa-utils mesa-vulkan-drivers \
  libsoup-3.0-dev javascriptcoregtk-4.1-dev \
  webkit2gtk-4.1-dev librust-alsa-sys-dev
```

**Windows**:
```bash
choco install strawberryperl protoc cmake curl cygwin gnuwin32-m4 msys2 make mingw
```

### Installation

1. Clone the repository:
```bash
git clone https://github.com/coasys/ad4m.git
cd ad4m
```

2. Install dependencies:
```bash
pnpm install
```

3. Build all packages project:
```bash
pnpm run build
```

4. Create a UI bundle for the Ad4m Launcher
```bash
pnpm run package-ad4m
```

Find the launcher bundle in `/target/release/bundle`.

## Project Structure

```
ad4m/
├── core/                   # Core AD4M implementation and TypeScript client
├── rust-executor/         # Rust implementation of the AD4M executor
├── rust-client/          # Rust implementation of the AD4M client
├── executor/             # JavaScript executor implementation
├── bootstrap-languages/  # Core Languages required for AD4M to function
├── cli/                 # Command line interface tools
├── connect/            # Library for connecting apps to AD4M
├── dapp/              # DApp server implementation
├── ui/               # Tauri-based system tray application
├── docs/            # Documentation and guides
├── tests/           # Integration tests
└── test-runner/    # Test automation framework
```

Key Components:
- **core**: Core types, `Ad4mClient`, and GraphQL schema. Published as `@coasys/ad4m` npm package.
- **rust-executor**: Main AD4M executor with GraphQL server, Deno runtime, Holochain integration, AI model inference and Prolog engine.
- **rust-client**: Rust implementation of `Ad4mClient`. Published as `ad4m-client` on crates.io.
- **executor**: Core JavaScript code managing agent state, perspectives, languages, and expressions.
- **bootstrap-languages**: Essential languages for AD4M functionality (like agent identity, language publishing).
- **cli**: Command line tools for interacting with AD4M. Published as `ad4m` on crates.io.
- **connect**: Helper library for apps to connect to AD4M executors with capability management.
- **dapp**: UI for blockchain integration through MetaMask.
- **ui**: System tray application (AD4M Launcher) for managing AD4M executors.

## Documentation
- [Intro and Vision](https://docs.ad4m.dev/)
- [Core Concepts](https://docs.ad4m.dev/concepts)
- [Developer Guides](https://docs.ad4m.dev/developer-guides)
- [API Reference](https://docs.ad4m.dev/jsdoc)

## Tools & Development

### AD4M CLI

Install the command line tools:
```bash
cargo install ad4m
```

Basic usage:
```bash
# Initialize AD4M
ad4m-executor init

# Start the executor
ad4m-executor run

# Create a perspective
ad4m perspectives create

# Query links
ad4m perspectives query-links <uuid>
```

### AD4M Launcher

For a graphical interface, install the [AD4M Launcher](https://github.com/coasys/ad4m-launcher).

## Contributing

We welcome contributions! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

### Development Process

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Run tests: `npm test`
5. Submit a pull request

## Community

- [Discord](https://discord.gg/fYGVM66jEz) - Join our community chat
- [Twitter](https://twitter.com/coasys_) - Follow us for updates
- [Blog](https://blog.coasys.org) - Read about our vision and progress

## License

AD4M is licensed under the [Cryptographic Autonomy License 1.0](LICENSE).

This license ensures:
- The right to run the software
- Access to source code
- The right to modify and distribute
- Protection of user autonomy and data sovereignty

## Acknowledgments

AD4M is developed by [Coasys](https://coasys.org) and builds upon ideas from:
- The Semantic Web
- Agent-centric computing
- Holochain
- Solid
