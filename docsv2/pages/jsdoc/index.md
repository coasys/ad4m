
<a name="readmemd"></a>

@perspect3vism/ad4m / [Exports](#modulesmd)

# AD4M
*The **A**gent-Centric **D**istributed **A**pplication **M**eta-ontology* 
or just: 
***A**gent-Centric **DA**pp **M**eta-ontology* 
* A new **meta-ontology** for interoperable, decentralized application design
* A **spanning-layer** to enable seamless integration between Holochain DNAs, blockchains, linked-data structures/ontologies and centralized back-ends
* The basis for turning distinct, monolithic and siloed apps into a global, open and interoperable **sense-making network**
---

## Ok, let's go...
To build an app/UI against Ad4m, you need to make sure that an 
[ad4m-executor](https://github.com/perspect3vism/ad4m-executor) is running
on the user's machine.

The easiest way to get that is to use ad4m-cli:
```sh
npm install -g @perspect3vism/ad4m-cli
ad4m executor run &
```

Then use `Ad4mClient` to connect to and work with the running ad4m-executor like this:
```
npm install --save @perspect3vism/ad4m
npm install --save-exact @apollo/client@3.7.10
npm install --save graphql-ws
npm install --save ws
```

In your code:
```js
import { Ad4mClient } from '@perspect3vism/ad4m'
import { ApolloClient, InMemoryCache } from "@apollo/client/core";
import { GraphQLWsLink } from "@apollo/client/link/subscriptions";
import { createClient } from 'graphql-ws';
import Websocket from "ws";

const wsLink = new GraphQLWsLink(createClient({
    url: `ws://localhost:4000/graphql`,
    webSocketImpl: Websocket
}));

const apolloClient = new ApolloClient({
    link: wsLink,
    cache: new InMemoryCache(),
    defaultOptions: {
        watchQuery: {
            fetchPolicy: 'network-only',
            nextFetchPolicy: 'network-only'
        },
    }
});

ad4mClient = new Ad4mClient(apolloClient)
```

### Unlocking / initializing the agent
You can't do much with the Ad4m runtime as long as the agent is not initialized.
So first get the agent status to see if we either need to create new DID or unlock
an existing keystore.

```js
const { isInitialized, isUnlocked, did } = await ad4mClient.agent.status()
```

If `isInitialized` is `false` (and then `did` is empty) we need to create or import
a DID and keys. `generate()` will create a new DID with method `key` and lock the
keystore with the given passphrase.

```js
const { did } = await ad4mClient.agent.generate("passphrase")
```

In following runs of the exectuor, `ad4mClient.agent.status()` will return a `did`
and `isInitialized` true, but if `isUnlocked` is false, we need to unlock the keystore
providing the passphrase:
```js
const { isUnlocked, did } = await ad4mClient.agent.unlock("passphrase")
```

### Languages
For creating an expression we need to select a language that we create an expression in:
```js
const languages = await ad4mClient.languages.all()
const noteIpfsAddress = languages.find(l => l.name === 'note-ipfs').address
```
### Creating an Expression

```js
const exprAddress = await ad4mClient.expression.create("A new text note", noteIpfsAddress)
```

### Creating a Perspective and linking that new Expression
```js
const perspectiveHandle = await ad4mClient.perspective.add("A new perspective on apps...")
await ad4mClient.perspective.addLink(
    perspectiveHandle.uuid,
    new Link({
        source: 'root',
        target: exprAddress
    })
)
```

### Publishing that local Perspective by turning it into a Neighbourhood
The back-bone of a Neighbourhood is a *LinkLanguage* - a Language that enables the sharing
and thus synchronizing of links (see `LinksAdapter` in [Language.ts](src/language/Language.ts)). 
While there can and should be many different implementations
with different trade-offs and features (like membranes etc.),
there currently is one fully implemented and Holochain based LinkLanguage with the name *Social Context*.

It is deployed on the current test network (Language Language v0.0.5) under the address:
`QmZ1mkoY8nLvpxY3Mizx8UkUiwUzjxJxsqSTPPdH8sHxCQ`.

#### Creating our unique LinkLanguage clone through templating
But we should not just use this publicly known Language as the back-bone for our new Neighbourhood,
since we need a unique clone.
So what we want is to use this existing Language as a template and create a new copy with the same code
but different UUID and/name in order to create a fresh space for our new Neighbourhood.

What parameters can we adjust when using it as template?
Let's have a look at the Language's meta information:

```js
const socialContextMeta = await ad4mClient.languages.meta("QmZ1mkoY8nLvpxY3Mizx8UkUiwUzjxJxsqSTPPdH8sHxCQ") 

console.log(socialContextMeta)
```

Which should yield something like this:
```
 {
  name: 'social-context',
  address: 'QmZ1mkoY8nLvpxY3Mizx8UkUiwUzjxJxsqSTPPdH8sHxCQ',
  description: 'Holochain based LinkLanguage. First full implementation of a LinkLanguage, for collaborative Neighbourhoods where every agent can add links. No membrane. Basic template for all custom Neighbourhoods in this first iteration of the Perspect3vism test network.',
  author: 'did:key:zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n',
  templated: false,
  templateSourceLanguageAddress: null,
  templateAppliedParams: null,
  possibleTemplateParams: [ 'uuid', 'name', 'description' ],
  sourceCodeLink: 'https://github.com/juntofoundation/Social-Context'
}
```

The field `possibleTemplateParams` tells us that we can set a `UUID` and override `name` and `description`.
Let's leave description but change the name.
The function `languages.applyTemplateAndPublish()` takes an object as JSON as second parameter like so:

```js
const uniqueLinkLanguage = await ad4mClient.languages.applyTemplateAndPublish("QmZ1mkoY8nLvpxY3Mizx8UkUiwUzjxJxsqSTPPdH8sHxCQ", JSON.stringify({"uuid": "84a329-77384c-1510fb", "name": "Social Context clone for demo Neighbourhood"}));
```
And then use this new LinkLanguage in our Neighbourhood:
```js
const meta = new Perspective()
const neighbourhoodUrl = await ad4mClient.neighbourhood.publishFromPerspective(
    perspectiveHandle.uuid,
    uniqueLinkLanguage.address,
    meta
)
console.log(neighbourhoodUrl) // => neighbourhood://Qm123456789abcdef
```

### Joining a Neighbourhood (on another node/agent)
Assume everything above happened on Alice's agent.
Alice now shares the Neighbourhood's URL with Bob.
This is what Bob does to join the Neigbourhood, access it as a (local) Perspective
and retrieve the Expression Alice created and linked there:
```js
const joinedNeighbourhood = await ad4mClient.neighbourhood.joinFromUrl(neighbourhoodUrl)
const links = await ad4mClient.perspective.queryLinks(joinedNeighbourhood.uuid, new LinkQuery({source: 'a'}))
links.forEach(async link => {
    const address = link.data.target
    const expression = await ad4mClient.expression.get(address)
    const data = JSON.parse(expression.data)
    console.log(data) //=> "A new text note"
})
```

## Building from source
Run:
```
npm i && npm run build
```

---

## Wait, what?! 
The central claim of AD4M is that any single- but also specifically multi-user application can be bootstrapped out of a meta-ontology consisting of 3 quintessential ontological units:
* Agents
* Languages
* and Perspectives

This is a *meta*-ontology since it doesn't make any assumptions about the specific ontologies implemented in those bootstrapped apps. But since apps bootstrapped from it share the same meta-ontology, they are mutualy interoperable.

![](https://i.imgur.com/MXa0ozg.png)

### Agents...
...represent humans with their devices, which is what the internet actually is. Technically **represented as Decentralized Identifiers - DIDs**.

### Languages...
...encapsulate the actual technology used to communicate, like Holochain or IPFS, but what they provide to the high-level layers is this: **Languages define Expressions**, which are the atoms of what Agents communicate. Expressions are always created, and thus signed, by an agent. Expressions are referenced via a URL of the kind `<language>://<language specific expression address>`. That URL and the Expression itself is the only objective part in AD4M. 

### Perspectives...
...belong to a specific agent. They represent context and association between expressions. They consist of a list of RDF/semantic web like triplets (subject-predicate-object) called `links` because all three items are just URLs pointing to expressions. Perspectives are like Solid's pods, but they are agent-centric. There is no such thing as a Perspective that does not belong to an agent. It is like the canvas on which an agent perceives and onto which they create anything. To the next layer above (either the very general UI built in Perspectivism - or any other special purpose UI), they are like a database scope.

---
### Bootstrapping

Any AD4M implementation will have to include at least 3 reflexive system Languages to enable the dynamic bootstrapping of apps and interconnected sense-making networks:
* A Language of Agents, i.e. where the expressions represent agents, and which uses DIDs as the expression URLs.
* A Language of Languages, i.e. a way to talk about Languages so Languages can be created by users and shared.
* A Language of Perspectives which implies the concept of **Shared Perspectives** a.k.a. **Neighbourhoods**, i.e. a way to share an otherwise local and private Perspective with others which constitutes the basic building block of any collaboration context.

Having these Languages means Agents can author expressions that represent Agents, Languages and Perspectives. These expressions get linked from inside Perspectives. That way we can model primitives like friends-lists (Perspective including agent expressions), app-stores (Perspective including Languages) and more.

### How do I build an app on/with AD4M?

Building an AD4M app actually means extending the AD4M ecosystem with the
* Languages
* and link-ontologies

needed for the app's domain - and then creating expressions from those Languages and linking them inside Perspectives.

The latter means creating RDF/semantic web style triplets that associate expressions in order to represent app specific semantics - not too different to how Solid style linked-data would work.

# Classes


<a name="classesad4mclientad4mclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [Ad4mClient](#modulesad4mclientmd) / Ad4mClient

## Class: Ad4mClient

[Ad4mClient](#modulesad4mclientmd).Ad4mClient

Client for the Ad4m interface wrapping GraphQL queryies
for convenient use in user facing code.

Aggregates the six sub-clients:
AgentClient, ExpressionClient, LanguageClient,
NeighbourhoodClient, PerspectiveClient and RuntimeClient
for the respective functionality.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#agentClient](##agentclient)
- [#apolloClient](##apolloclient)
- [#expressionClient](##expressionclient)
- [#languageClient](##languageclient)
- [#neighbourhoodClient](##neighbourhoodclient)
- [#perspectiveClient](##perspectiveclient)
- [#runtimeClient](##runtimeclient)

#### Accessors

- [agent](#agent)
- [expression](#expression)
- [languages](#languages)
- [neighbourhood](#neighbourhood)
- [perspective](#perspective)
- [runtime](#runtime)

### Constructors

#### constructor

• **new Ad4mClient**(`client`, `subscribe?`)

##### Parameters

| Name | Type | Default value |
| :------ | :------ | :------ |
| `client` | `ApolloClient`<`any`\> | `undefined` |
| `subscribe` | `boolean` | `true` |

##### Defined in

[Ad4mClient.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L28)

### Properties

#### #agentClient

• `Private` **#agentClient**: [`AgentClient`](#classesagent_agentclientagentclientmd)

##### Defined in

[Ad4mClient.ts:20](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L20)

___

#### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

##### Defined in

[Ad4mClient.ts:19](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L19)

___

#### #expressionClient

• `Private` **#expressionClient**: [`ExpressionClient`](#classesexpression_expressionclientexpressionclientmd)

##### Defined in

[Ad4mClient.ts:21](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L21)

___

#### #languageClient

• `Private` **#languageClient**: [`LanguageClient`](#classeslanguage_languageclientlanguageclientmd)

##### Defined in

[Ad4mClient.ts:22](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L22)

___

#### #neighbourhoodClient

• `Private` **#neighbourhoodClient**: [`NeighbourhoodClient`](#classesneighbourhood_neighbourhoodclientneighbourhoodclientmd)

##### Defined in

[Ad4mClient.ts:23](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L23)

___

#### #perspectiveClient

• `Private` **#perspectiveClient**: [`PerspectiveClient`](#classesperspectives_perspectiveclientperspectiveclientmd)

##### Defined in

[Ad4mClient.ts:24](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L24)

___

#### #runtimeClient

• `Private` **#runtimeClient**: [`RuntimeClient`](#classesruntime_runtimeclientruntimeclientmd)

##### Defined in

[Ad4mClient.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L25)

### Accessors

#### agent

• `get` **agent**(): [`AgentClient`](#classesagent_agentclientagentclientmd)

##### Returns

[`AgentClient`](#classesagent_agentclientagentclientmd)

##### Defined in

[Ad4mClient.ts:40](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L40)

___

#### expression

• `get` **expression**(): [`ExpressionClient`](#classesexpression_expressionclientexpressionclientmd)

##### Returns

[`ExpressionClient`](#classesexpression_expressionclientexpressionclientmd)

##### Defined in

[Ad4mClient.ts:44](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L44)

___

#### languages

• `get` **languages**(): [`LanguageClient`](#classeslanguage_languageclientlanguageclientmd)

##### Returns

[`LanguageClient`](#classeslanguage_languageclientlanguageclientmd)

##### Defined in

[Ad4mClient.ts:48](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L48)

___

#### neighbourhood

• `get` **neighbourhood**(): [`NeighbourhoodClient`](#classesneighbourhood_neighbourhoodclientneighbourhoodclientmd)

##### Returns

[`NeighbourhoodClient`](#classesneighbourhood_neighbourhoodclientneighbourhoodclientmd)

##### Defined in

[Ad4mClient.ts:52](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L52)

___

#### perspective

• `get` **perspective**(): [`PerspectiveClient`](#classesperspectives_perspectiveclientperspectiveclientmd)

##### Returns

[`PerspectiveClient`](#classesperspectives_perspectiveclientperspectiveclientmd)

##### Defined in

[Ad4mClient.ts:56](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L56)

___

#### runtime

• `get` **runtime**(): [`RuntimeClient`](#classesruntime_runtimeclientruntimeclientmd)

##### Returns

[`RuntimeClient`](#classesruntime_runtimeclientruntimeclientmd)

##### Defined in

[Ad4mClient.ts:60](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Ad4mClient.ts#L60)


<a name="classesliteralliteralmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [Literal](#modulesliteralmd) / Literal

## Class: Literal

[Literal](#modulesliteralmd).Literal

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#literal](##literal)
- [#url](##url)

#### Methods

- [get](#get)
- [toUrl](#tourl)
- [from](#from)
- [fromUrl](#fromurl)

### Constructors

#### constructor

• **new Literal**()

### Properties

#### #literal

• `Private` `Optional` **#literal**: `any`

##### Defined in

[Literal.ts:10](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Literal.ts#L10)

___

#### #url

• `Private` `Optional` **#url**: `string`

##### Defined in

[Literal.ts:11](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Literal.ts#L11)

### Methods

#### get

▸ **get**(): `any`

##### Returns

`any`

##### Defined in

[Literal.ts:49](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Literal.ts#L49)

___

#### toUrl

▸ **toUrl**(): `string`

##### Returns

`string`

##### Defined in

[Literal.ts:27](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Literal.ts#L27)

___

#### from

▸ `Static` **from**(`literal`): [`Literal`](#classesliteralliteralmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `literal` | `any` |

##### Returns

[`Literal`](#classesliteralliteralmd)

##### Defined in

[Literal.ts:21](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Literal.ts#L21)

___

#### fromUrl

▸ `Static` **fromUrl**(`url`): [`Literal`](#classesliteralliteralmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

[`Literal`](#classesliteralliteralmd)

##### Defined in

[Literal.ts:13](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Literal.ts#L13)


<a name="classessmartliteralsmartliteralmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [SmartLiteral](#modulessmartliteralmd) / SmartLiteral

## Class: SmartLiteral

[SmartLiteral](#modulessmartliteralmd).SmartLiteral

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#base](##base)
- [#perspective](##perspective)

#### Accessors

- [base](#base)

#### Methods

- [get](#get)
- [set](#set)
- [create](#create)
- [getAllSmartLiterals](#getallsmartliterals)
- [isSmartLiteralBase](#issmartliteralbase)

### Constructors

#### constructor

• **new SmartLiteral**(`perspective`, `base`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd) |
| `base` | `string` |

##### Defined in

[SmartLiteral.ts:23](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L23)

### Properties

#### #base

• `Private` **#base**: `string`

##### Defined in

[SmartLiteral.ts:21](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L21)

___

#### #perspective

• `Private` **#perspective**: [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)

##### Defined in

[SmartLiteral.ts:20](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L20)

### Accessors

#### base

• `get` **base**(): `string`

##### Returns

`string`

##### Defined in

[SmartLiteral.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L28)

### Methods

#### get

▸ **get**(): `Promise`<`any`\>

##### Returns

`Promise`<`any`\>

##### Defined in

[SmartLiteral.ts:54](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L54)

___

#### set

▸ **set**(`content`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `any` |

##### Returns

`Promise`<`void`\>

##### Defined in

[SmartLiteral.ts:67](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L67)

___

#### create

▸ `Static` **create**(`perspective`, `literal`): `Promise`<[`SmartLiteral`](#classessmartliteralsmartliteralmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd) |
| `literal` | `any` |

##### Returns

`Promise`<[`SmartLiteral`](#classessmartliteralsmartliteralmd)\>

##### Defined in

[SmartLiteral.ts:32](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L32)

___

#### getAllSmartLiterals

▸ `Static` **getAllSmartLiterals**(`perspective`): `Promise`<[`SmartLiteral`](#classessmartliteralsmartliteralmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd) |

##### Returns

`Promise`<[`SmartLiteral`](#classessmartliteralsmartliteralmd)[]\>

##### Defined in

[SmartLiteral.ts:47](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L47)

___

#### isSmartLiteralBase

▸ `Static` **isSmartLiteralBase**(`perspective`, `base`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd) |
| `base` | `string` |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[SmartLiteral.ts:39](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L39)


<a name="classesagent_agentagentmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / Agent

## Class: Agent

[agent/Agent](#modulesagent_agentmd).Agent

AD4M's representation of an Agent

AD4M Agents are build around DIDs, which are used to identify and authenticate the Agent.
Conceptually, an Agent is regarded as something that can speak and that can listen.

Agents speak by creating Expressions in AD4M Languages which are signed by the Agent's DID key,
And they also speak (broadcast) by putting semantic statements into their public "Agent Perspective".
They listen (can receive messages) through their "direct message Language".

These three aspects are represented by the three fields of this class.

This class is used as format for the Expressions in the Agent language.
Since AD4M treats DID URIs as addresses for the Agent Language,
DIDs are resolved to Expressions that are objects of this class.
Thus, this is how agents see (other) agents.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [did](#did)
- [directMessageLanguage](#directmessagelanguage)
- [perspective](#perspective)

### Constructors

#### constructor

• **new Agent**(`did`, `perspective?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `perspective?` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Defined in

[agent/Agent.ts:42](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L42)

### Properties

#### did

• **did**: `string`

The DID of the Agent
All epxressions authored by them are signed with the keys mentioned
in the DID document behind this DID URI.

##### Defined in

[agent/Agent.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L28)

___

#### directMessageLanguage

• `Optional` **directMessageLanguage**: `string`

Address of the Language by which the Agent will receive DMs

##### Defined in

[agent/Agent.ts:40](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L40)

___

#### perspective

• `Optional` **perspective**: [`Perspective`](#classesperspectives_perspectiveperspectivemd)

The Perspective that holds the public-facing semantics/statements of the Agent
Holds and shares a Perspective that links all information
this agent wants to offer as public-facing semantics.
This should be used for any kind of user profile information.

##### Defined in

[agent/Agent.ts:36](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L36)


<a name="classesagent_agentagentexpressionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / AgentExpression

## Class: AgentExpression

[agent/Agent](#modulesagent_agentmd).AgentExpression

### Hierarchy

- `any`

  ↳ **`AgentExpression`**

### Table of contents

#### Constructors

- [constructor](#constructor)

### Constructors

#### constructor

• **new AgentExpression**()

##### Inherited from

ExpressionGeneric(Agent).constructor


<a name="classesagent_agentagentsignaturemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / AgentSignature

## Class: AgentSignature

[agent/Agent](#modulesagent_agentmd).AgentSignature

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [publicKey](#publickey)
- [signature](#signature)

### Constructors

#### constructor

• **new AgentSignature**(`signature`, `publicKey`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `signature` | `string` |
| `publicKey` | `string` |

##### Defined in

[agent/Agent.ts:137](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L137)

### Properties

#### publicKey

• **publicKey**: `string`

##### Defined in

[agent/Agent.ts:135](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L135)

___

#### signature

• **signature**: `string`

##### Defined in

[agent/Agent.ts:132](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L132)


<a name="classesagent_agentappsmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / Apps

## Class: Apps

[agent/Agent](#modulesagent_agentmd).Apps

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [auth](#auth)
- [requestId](#requestid)
- [revoked](#revoked)
- [token](#token)

### Constructors

#### constructor

• **new Apps**(`requestId`, `auth`, `token`, `revoked?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |
| `auth` | [`AuthInfo`](#classesagent_agentauthinfomd) |
| `token` | `string` |
| `revoked?` | `boolean` |

##### Defined in

[agent/Agent.ts:217](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L217)

### Properties

#### auth

• **auth**: [`AuthInfo`](#classesagent_agentauthinfomd)

##### Defined in

[agent/Agent.ts:215](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L215)

___

#### requestId

• **requestId**: `string`

##### Defined in

[agent/Agent.ts:206](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L206)

___

#### revoked

• `Optional` **revoked**: `boolean`

##### Defined in

[agent/Agent.ts:212](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L212)

___

#### token

• **token**: `string`

##### Defined in

[agent/Agent.ts:209](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L209)


<a name="classesagent_agentauthinfomd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / AuthInfo

## Class: AuthInfo

[agent/Agent](#modulesagent_agentmd).AuthInfo

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [appDesc](#appdesc)
- [appIconPath](#appiconpath)
- [appName](#appname)
- [appUrl](#appurl)
- [capabilities](#capabilities)

### Constructors

#### constructor

• **new AuthInfo**(`appName`, `appDesc`, `appUrl`, `capabilities`, `appIconPath?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `appName` | `string` |
| `appDesc` | `string` |
| `appUrl` | `string` |
| `capabilities` | [`Capability`](#classesagent_agentcapabilitymd)[] |
| `appIconPath?` | `string` |

##### Defined in

[agent/Agent.ts:188](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L188)

### Properties

#### appDesc

• **appDesc**: `string`

##### Defined in

[agent/Agent.ts:177](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L177)

___

#### appIconPath

• `Optional` **appIconPath**: `string`

##### Defined in

[agent/Agent.ts:183](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L183)

___

#### appName

• **appName**: `string`

##### Defined in

[agent/Agent.ts:174](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L174)

___

#### appUrl

• **appUrl**: `string`

##### Defined in

[agent/Agent.ts:180](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L180)

___

#### capabilities

• **capabilities**: [`Capability`](#classesagent_agentcapabilitymd)[]

##### Defined in

[agent/Agent.ts:186](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L186)


<a name="classesagent_agentauthinfoinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / AuthInfoInput

## Class: AuthInfoInput

[agent/Agent](#modulesagent_agentmd).AuthInfoInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [appDesc](#appdesc)
- [appDomain](#appdomain)
- [appIconPath](#appiconpath)
- [appName](#appname)
- [appUrl](#appurl)
- [capabilities](#capabilities)

### Constructors

#### constructor

• **new AuthInfoInput**(`appName`, `appDesc`, `appDomain`, `appUrl?`, `appIconPath?`, `capabilities?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `appName` | `string` |
| `appDesc` | `string` |
| `appDomain` | `string` |
| `appUrl?` | `string` |
| `appIconPath?` | `string` |
| `capabilities?` | [`CapabilityInput`](#classesagent_agentcapabilityinputmd)[] |

##### Defined in

[agent/Agent.ts:278](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L278)

### Properties

#### appDesc

• **appDesc**: `string`

##### Defined in

[agent/Agent.ts:264](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L264)

___

#### appDomain

• **appDomain**: `string`

##### Defined in

[agent/Agent.ts:267](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L267)

___

#### appIconPath

• `Optional` **appIconPath**: `string`

##### Defined in

[agent/Agent.ts:273](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L273)

___

#### appName

• **appName**: `string`

##### Defined in

[agent/Agent.ts:261](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L261)

___

#### appUrl

• `Optional` **appUrl**: `string`

##### Defined in

[agent/Agent.ts:270](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L270)

___

#### capabilities

• `Optional` **capabilities**: [`CapabilityInput`](#classesagent_agentcapabilityinputmd)[]

##### Defined in

[agent/Agent.ts:276](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L276)


<a name="classesagent_agentcapabilitymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / Capability

## Class: Capability

[agent/Agent](#modulesagent_agentmd).Capability

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [can](#can)
- [with](#with)

### Constructors

#### constructor

• **new Capability**(`withF`, `can`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `withF` | [`Resource`](#classesagent_agentresourcemd) |
| `can` | `string`[] |

##### Defined in

[agent/Agent.ts:165](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L165)

### Properties

#### can

• **can**: `string`[]

##### Defined in

[agent/Agent.ts:163](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L163)

___

#### with

• **with**: [`Resource`](#classesagent_agentresourcemd)

##### Defined in

[agent/Agent.ts:160](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L160)


<a name="classesagent_agentcapabilityinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / CapabilityInput

## Class: CapabilityInput

[agent/Agent](#modulesagent_agentmd).CapabilityInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [can](#can)
- [with](#with)

### Constructors

#### constructor

• **new CapabilityInput**(`withF`, `can`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `withF` | [`ResourceInput`](#classesagent_agentresourceinputmd) |
| `can` | `string`[] |

##### Defined in

[agent/Agent.ts:252](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L252)

### Properties

#### can

• **can**: `string`[]

##### Defined in

[agent/Agent.ts:250](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L250)

___

#### with

• **with**: [`ResourceInput`](#classesagent_agentresourceinputmd)

##### Defined in

[agent/Agent.ts:247](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L247)


<a name="classesagent_agententanglementproofmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / EntanglementProof

## Class: EntanglementProof

[agent/Agent](#modulesagent_agentmd).EntanglementProof

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [deviceKey](#devicekey)
- [deviceKeySignedByDid](#devicekeysignedbydid)
- [deviceKeyType](#devicekeytype)
- [did](#did)
- [didSignedByDeviceKey](#didsignedbydevicekey)
- [didSigningKeyId](#didsigningkeyid)

### Constructors

#### constructor

• **new EntanglementProof**(`did`, `didSigningKeyId`, `deviceKeyType`, `deviceKey`, `deviceKeySignedByDid`, `didSignedByDeviceKey?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `didSigningKeyId` | `string` |
| `deviceKeyType` | `string` |
| `deviceKey` | `string` |
| `deviceKeySignedByDid` | `string` |
| `didSignedByDeviceKey?` | `string` |

##### Defined in

[agent/Agent.ts:75](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L75)

### Properties

#### deviceKey

• **deviceKey**: `string`

##### Defined in

[agent/Agent.ts:67](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L67)

___

#### deviceKeySignedByDid

• **deviceKeySignedByDid**: `string`

##### Defined in

[agent/Agent.ts:70](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L70)

___

#### deviceKeyType

• **deviceKeyType**: `string`

##### Defined in

[agent/Agent.ts:64](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L64)

___

#### did

• **did**: `string`

##### Defined in

[agent/Agent.ts:58](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L58)

___

#### didSignedByDeviceKey

• `Optional` **didSignedByDeviceKey**: `string`

##### Defined in

[agent/Agent.ts:73](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L73)

___

#### didSigningKeyId

• **didSigningKeyId**: `string`

##### Defined in

[agent/Agent.ts:61](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L61)


<a name="classesagent_agententanglementproofinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / EntanglementProofInput

## Class: EntanglementProofInput

[agent/Agent](#modulesagent_agentmd).EntanglementProofInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [deviceKey](#devicekey)
- [deviceKeySignedByDid](#devicekeysignedbydid)
- [deviceKeyType](#devicekeytype)
- [did](#did)
- [didSignedByDeviceKey](#didsignedbydevicekey)
- [didSigningKeyId](#didsigningkeyid)

### Constructors

#### constructor

• **new EntanglementProofInput**(`did`, `didSigningKeyId`, `deviceKeyType`, `deviceKey`, `deviceKeySignedByDid`, `didSignedByDeviceKey`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `didSigningKeyId` | `string` |
| `deviceKeyType` | `string` |
| `deviceKey` | `string` |
| `deviceKeySignedByDid` | `string` |
| `didSignedByDeviceKey` | `string` |

##### Defined in

[agent/Agent.ts:112](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L112)

### Properties

#### deviceKey

• **deviceKey**: `string`

##### Defined in

[agent/Agent.ts:104](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L104)

___

#### deviceKeySignedByDid

• **deviceKeySignedByDid**: `string`

##### Defined in

[agent/Agent.ts:107](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L107)

___

#### deviceKeyType

• **deviceKeyType**: `string`

##### Defined in

[agent/Agent.ts:101](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L101)

___

#### did

• **did**: `string`

##### Defined in

[agent/Agent.ts:95](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L95)

___

#### didSignedByDeviceKey

• **didSignedByDeviceKey**: `string`

##### Defined in

[agent/Agent.ts:110](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L110)

___

#### didSigningKeyId

• **didSigningKeyId**: `string`

##### Defined in

[agent/Agent.ts:98](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L98)


<a name="classesagent_agentresourcemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / Resource

## Class: Resource

[agent/Agent](#modulesagent_agentmd).Resource

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [domain](#domain)
- [pointers](#pointers)

### Constructors

#### constructor

• **new Resource**(`domain`, `pointers`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `domain` | `string` |
| `pointers` | `string`[] |

##### Defined in

[agent/Agent.ts:151](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L151)

### Properties

#### domain

• **domain**: `string`

##### Defined in

[agent/Agent.ts:146](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L146)

___

#### pointers

• **pointers**: `string`[]

##### Defined in

[agent/Agent.ts:149](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L149)


<a name="classesagent_agentresourceinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/Agent](#modulesagent_agentmd) / ResourceInput

## Class: ResourceInput

[agent/Agent](#modulesagent_agentmd).ResourceInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [domain](#domain)
- [pointers](#pointers)

### Constructors

#### constructor

• **new ResourceInput**(`domain`, `pointers`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `domain` | `string` |
| `pointers` | `string`[] |

##### Defined in

[agent/Agent.ts:238](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L238)

### Properties

#### domain

• **domain**: `string`

##### Defined in

[agent/Agent.ts:233](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L233)

___

#### pointers

• **pointers**: `string`[]

##### Defined in

[agent/Agent.ts:236](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/Agent.ts#L236)


<a name="classesagent_agentclientagentclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/AgentClient](#modulesagent_agentclientmd) / AgentClient

## Class: AgentClient

[agent/AgentClient](#modulesagent_agentclientmd).AgentClient

Provides access to all functions regarding the local agent,
such as generating, locking, unlocking, importing the DID keystore,
as well as updating the publicly shared Agent expression.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#agentStatusChangedCallbacks](##agentstatuschangedcallbacks)
- [#apolloClient](##apolloclient)
- [#appsChangedCallback](##appschangedcallback)
- [#updatedCallbacks](##updatedcallbacks)

#### Methods

- [addAgentStatusChangedListener](#addagentstatuschangedlistener)
- [addAppChangedListener](#addappchangedlistener)
- [addEntanglementProofs](#addentanglementproofs)
- [addUpdatedListener](#addupdatedlistener)
- [byDID](#bydid)
- [deleteEntanglementProofs](#deleteentanglementproofs)
- [entanglementProofPreFlight](#entanglementproofpreflight)
- [generate](#generate)
- [generateJwt](#generatejwt)
- [getApps](#getapps)
- [getEntanglementProofs](#getentanglementproofs)
- [import](#import)
- [isLocked](#islocked)
- [lock](#lock)
- [me](#me)
- [mutatePublicPerspective](#mutatepublicperspective)
- [permitCapability](#permitcapability)
- [removeApp](#removeapp)
- [requestCapability](#requestcapability)
- [revokeToken](#revoketoken)
- [signMessage](#signmessage)
- [status](#status)
- [subscribeAgentStatusChanged](#subscribeagentstatuschanged)
- [subscribeAgentUpdated](#subscribeagentupdated)
- [subscribeAppsChanged](#subscribeappschanged)
- [unlock](#unlock)
- [updateDirectMessageLanguage](#updatedirectmessagelanguage)
- [updatePublicPerspective](#updatepublicperspective)

### Constructors

#### constructor

• **new AgentClient**(`client`, `subscribe?`)

##### Parameters

| Name | Type | Default value |
| :------ | :------ | :------ |
| `client` | `ApolloClient`<`any`\> | `undefined` |
| `subscribe` | `boolean` | `true` |

##### Defined in

[agent/AgentClient.ts:93](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L93)

### Properties

#### #agentStatusChangedCallbacks

• `Private` **#agentStatusChangedCallbacks**: [`AgentStatusChangedCallback`](#agentstatuschangedcallback)[]

##### Defined in

[agent/AgentClient.ts:91](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L91)

___

#### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

##### Defined in

[agent/AgentClient.ts:88](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L88)

___

#### #appsChangedCallback

• `Private` **#appsChangedCallback**: [`AgentAppsUpdatedCallback`](#agentappsupdatedcallback)[]

##### Defined in

[agent/AgentClient.ts:89](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L89)

___

#### #updatedCallbacks

• `Private` **#updatedCallbacks**: [`AgentUpdatedCallback`](#agentupdatedcallback)[]

##### Defined in

[agent/AgentClient.ts:90](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L90)

### Methods

#### addAgentStatusChangedListener

▸ **addAgentStatusChangedListener**(`listener`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `listener` | `any` |

##### Returns

`void`

##### Defined in

[agent/AgentClient.ts:398](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L398)

___

#### addAppChangedListener

▸ **addAppChangedListener**(`listener`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `listener` | `any` |

##### Returns

`void`

##### Defined in

[agent/AgentClient.ts:355](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L355)

___

#### addEntanglementProofs

▸ **addEntanglementProofs**(`proofs`): `Promise`<[`EntanglementProof`](#classesagent_agententanglementproofmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `proofs` | [`EntanglementProofInput`](#classesagent_agententanglementproofinputmd)[] |

##### Returns

`Promise`<[`EntanglementProof`](#classesagent_agententanglementproofmd)[]\>

##### Defined in

[agent/AgentClient.ts:289](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L289)

___

#### addUpdatedListener

▸ **addUpdatedListener**(`listener`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `listener` | `any` |

##### Returns

`void`

##### Defined in

[agent/AgentClient.ts:351](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L351)

___

#### byDID

▸ **byDID**(`did`): `Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |

##### Returns

`Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Defined in

[agent/AgentClient.ts:200](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L200)

___

#### deleteEntanglementProofs

▸ **deleteEntanglementProofs**(`proofs`): `Promise`<[`EntanglementProof`](#classesagent_agententanglementproofmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `proofs` | [`EntanglementProofInput`](#classesagent_agententanglementproofinputmd)[] |

##### Returns

`Promise`<[`EntanglementProof`](#classesagent_agententanglementproofmd)[]\>

##### Defined in

[agent/AgentClient.ts:305](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L305)

___

#### entanglementProofPreFlight

▸ **entanglementProofPreFlight**(`deviceKey`, `deviceKeyType`): `Promise`<[`EntanglementProof`](#classesagent_agententanglementproofmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `deviceKey` | `string` |
| `deviceKeyType` | `string` |

##### Returns

`Promise`<[`EntanglementProof`](#classesagent_agententanglementproofmd)\>

##### Defined in

[agent/AgentClient.ts:334](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L334)

___

#### generate

▸ **generate**(`passphrase`): `Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |

##### Returns

`Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Defined in

[agent/AgentClient.ts:136](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L136)

___

#### generateJwt

▸ **generateJwt**(`requestId`, `rand`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |
| `rand` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[agent/AgentClient.ts:449](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L449)

___

#### getApps

▸ **getApps**(): `Promise`<[`Apps`](#classesagent_agentappsmd)[]\>

##### Returns

`Promise`<[`Apps`](#classesagent_agentappsmd)[]\>

##### Defined in

[agent/AgentClient.ts:463](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L463)

___

#### getEntanglementProofs

▸ **getEntanglementProofs**(): `Promise`<`string`[]\>

##### Returns

`Promise`<`string`[]\>

##### Defined in

[agent/AgentClient.ts:321](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L321)

___

#### import

▸ **import**(`args`): `Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `args` | [`InitializeArgs`](#interfacesagent_agentclientinitializeargsmd) |

##### Returns

`Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Defined in

[agent/AgentClient.ts:152](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L152)

___

#### isLocked

▸ **isLocked**(): `Promise`<`boolean`\>

##### Returns

`Promise`<`boolean`\>

##### Defined in

[agent/AgentClient.ts:504](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L504)

___

#### lock

▸ **lock**(`passphrase`): `Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |

##### Returns

`Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Defined in

[agent/AgentClient.ts:172](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L172)

___

#### me

▸ **me**(): `Promise`<[`Agent`](#classesagent_agentagentmd)\>

Returns the Agent expression of the local agent as it is shared
publicly via the AgentLanguage.

I.e. this is the users profile.

##### Returns

`Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Defined in

[agent/AgentClient.ts:112](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L112)

___

#### mutatePublicPerspective

▸ **mutatePublicPerspective**(`mutations`): `Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `mutations` | [`LinkMutations`](#classeslinks_linkslinkmutationsmd) |

##### Returns

`Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Defined in

[agent/AgentClient.ts:239](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L239)

___

#### permitCapability

▸ **permitCapability**(`auth`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `auth` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[agent/AgentClient.ts:435](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L435)

___

#### removeApp

▸ **removeApp**(`requestId`): `Promise`<[`Apps`](#classesagent_agentappsmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |

##### Returns

`Promise`<[`Apps`](#classesagent_agentappsmd)[]\>

##### Defined in

[agent/AgentClient.ts:476](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L476)

___

#### requestCapability

▸ **requestCapability**(`authInfo`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `authInfo` | [`AuthInfoInput`](#classesagent_agentauthinfoinputmd) |

##### Returns

`Promise`<`string`\>

##### Defined in

[agent/AgentClient.ts:421](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L421)

___

#### revokeToken

▸ **revokeToken**(`requestId`): `Promise`<[`Apps`](#classesagent_agentappsmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |

##### Returns

`Promise`<[`Apps`](#classesagent_agentappsmd)[]\>

##### Defined in

[agent/AgentClient.ts:490](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L490)

___

#### signMessage

▸ **signMessage**(`message`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `message` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[agent/AgentClient.ts:517](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L517)

___

#### status

▸ **status**(): `Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Returns

`Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Defined in

[agent/AgentClient.ts:123](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L123)

___

#### subscribeAgentStatusChanged

▸ **subscribeAgentStatusChanged**(): `void`

##### Returns

`void`

##### Defined in

[agent/AgentClient.ts:402](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L402)

___

#### subscribeAgentUpdated

▸ **subscribeAgentUpdated**(): `void`

##### Returns

`void`

##### Defined in

[agent/AgentClient.ts:359](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L359)

___

#### subscribeAppsChanged

▸ **subscribeAppsChanged**(): `void`

##### Returns

`void`

##### Defined in

[agent/AgentClient.ts:378](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L378)

___

#### unlock

▸ **unlock**(`passphrase`): `Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |

##### Returns

`Promise`<[`AgentStatus`](#classesagent_agentstatusagentstatusmd)\>

##### Defined in

[agent/AgentClient.ts:186](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L186)

___

#### updateDirectMessageLanguage

▸ **updateDirectMessageLanguage**(`directMessageLanguage`): `Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `directMessageLanguage` | `string` |

##### Returns

`Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Defined in

[agent/AgentClient.ts:270](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L270)

___

#### updatePublicPerspective

▸ **updatePublicPerspective**(`perspective`): `Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveInput`](#classesperspectives_perspectiveperspectiveinputmd) |

##### Returns

`Promise`<[`Agent`](#classesagent_agentagentmd)\>

##### Defined in

[agent/AgentClient.ts:214](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L214)


<a name="classesagent_agentresolverdefaultmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/AgentResolver](#modulesagent_agentresolvermd) / default

## Class: default

[agent/AgentResolver](#modulesagent_agentresolvermd).default

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Methods

- [agent](#agent)
- [agentAddEntanglementProofs](#agentaddentanglementproofs)
- [agentAppsChanged](#agentappschanged)
- [agentByDID](#agentbydid)
- [agentDeleteEntanglementProofs](#agentdeleteentanglementproofs)
- [agentEntanglementProofPreFlight](#agententanglementproofpreflight)
- [agentGenerate](#agentgenerate)
- [agentGenerateJwt](#agentgeneratejwt)
- [agentGetApps](#agentgetapps)
- [agentGetEntanglementProofs](#agentgetentanglementproofs)
- [agentImport](#agentimport)
- [agentIsLocked](#agentislocked)
- [agentLock](#agentlock)
- [agentPermitCapability](#agentpermitcapability)
- [agentRemoveApp](#agentremoveapp)
- [agentRequestCapability](#agentrequestcapability)
- [agentRevokeToken](#agentrevoketoken)
- [agentSignMessage](#agentsignmessage)
- [agentStatus](#agentstatus)
- [agentStatusChanged](#agentstatuschanged)
- [agentUnlock](#agentunlock)
- [agentUpdateDirectMessageLanguage](#agentupdatedirectmessagelanguage)
- [agentUpdatePublicPerspective](#agentupdatepublicperspective)
- [agentUpdated](#agentupdated)

### Constructors

#### constructor

• **new default**()

### Methods

#### agent

▸ **agent**(): [`Agent`](#classesagent_agentagentmd)

##### Returns

[`Agent`](#classesagent_agentagentmd)

##### Defined in

[agent/AgentResolver.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L25)

___

#### agentAddEntanglementProofs

▸ **agentAddEntanglementProofs**(`proofs`): [`EntanglementProof`](#classesagent_agententanglementproofmd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `proofs` | [`EntanglementProofInput`](#classesagent_agententanglementproofinputmd)[] |

##### Returns

[`EntanglementProof`](#classesagent_agententanglementproofmd)[]

##### Defined in

[agent/AgentResolver.ts:129](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L129)

___

#### agentAppsChanged

▸ **agentAppsChanged**(): ``null``

##### Returns

``null``

##### Defined in

[agent/AgentResolver.ts:220](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L220)

___

#### agentByDID

▸ **agentByDID**(`did`): [`Agent`](#classesagent_agentagentmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |

##### Returns

[`Agent`](#classesagent_agentagentmd)

##### Defined in

[agent/AgentResolver.ts:92](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L92)

___

#### agentDeleteEntanglementProofs

▸ **agentDeleteEntanglementProofs**(`proofs`): [`EntanglementProof`](#classesagent_agententanglementproofmd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `proofs` | [`EntanglementProofInput`](#classesagent_agententanglementproofinputmd)[] |

##### Returns

[`EntanglementProof`](#classesagent_agententanglementproofmd)[]

##### Defined in

[agent/AgentResolver.ts:146](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L146)

___

#### agentEntanglementProofPreFlight

▸ **agentEntanglementProofPreFlight**(`deviceKey`, `deviceKeyType`): [`EntanglementProof`](#classesagent_agententanglementproofmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `deviceKey` | `string` |
| `deviceKeyType` | `string` |

##### Returns

[`EntanglementProof`](#classesagent_agententanglementproofmd)

##### Defined in

[agent/AgentResolver.ts:177](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L177)

___

#### agentGenerate

▸ **agentGenerate**(`passphrase`, `pubSub`): [`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |
| `pubSub` | `any` |

##### Returns

[`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Defined in

[agent/AgentResolver.ts:35](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L35)

___

#### agentGenerateJwt

▸ **agentGenerateJwt**(`requestId`, `rand`): `String`

##### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |
| `rand` | `string` |

##### Returns

`String`

##### Defined in

[agent/AgentResolver.ts:201](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L201)

___

#### agentGetApps

▸ **agentGetApps**(): []

##### Returns

[]

##### Defined in

[agent/AgentResolver.ts:214](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L214)

___

#### agentGetEntanglementProofs

▸ **agentGetEntanglementProofs**(): [`EntanglementProof`](#classesagent_agententanglementproofmd)[]

##### Returns

[`EntanglementProof`](#classesagent_agententanglementproofmd)[]

##### Defined in

[agent/AgentResolver.ts:163](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L163)

___

#### agentImport

▸ **agentImport**(`did`, `didDocument`, `keystore`, `passphrase`): [`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `didDocument` | `string` |
| `keystore` | `string` |
| `passphrase` | `string` |

##### Returns

[`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Defined in

[agent/AgentResolver.ts:50](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L50)

___

#### agentIsLocked

▸ **agentIsLocked**(): `Boolean`

##### Returns

`Boolean`

##### Defined in

[agent/AgentResolver.ts:209](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L209)

___

#### agentLock

▸ **agentLock**(`passphrase`, `pubSub`): [`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |
| `pubSub` | `any` |

##### Returns

[`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Defined in

[agent/AgentResolver.ts:65](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L65)

___

#### agentPermitCapability

▸ **agentPermitCapability**(`auth`): `String`

##### Parameters

| Name | Type |
| :------ | :------ |
| `auth` | `string` |

##### Returns

`String`

##### Defined in

[agent/AgentResolver.ts:196](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L196)

___

#### agentRemoveApp

▸ **agentRemoveApp**(`requestId`, `pubSub`): []

##### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |
| `pubSub` | `any` |

##### Returns

[]

##### Defined in

[agent/AgentResolver.ts:226](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L226)

___

#### agentRequestCapability

▸ **agentRequestCapability**(`authInfo`): `String`

##### Parameters

| Name | Type |
| :------ | :------ |
| `authInfo` | [`AuthInfoInput`](#classesagent_agentauthinfoinputmd) |

##### Returns

`String`

##### Defined in

[agent/AgentResolver.ts:191](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L191)

___

#### agentRevokeToken

▸ **agentRevokeToken**(`requestId`): `any`[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |

##### Returns

`any`[]

##### Defined in

[agent/AgentResolver.ts:232](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L232)

___

#### agentSignMessage

▸ **agentSignMessage**(`message`): [`AgentSignature`](#classesagent_agentagentsignaturemd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `message` | `string` |

##### Returns

[`AgentSignature`](#classesagent_agentagentsignaturemd)

##### Defined in

[agent/AgentResolver.ts:258](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L258)

___

#### agentStatus

▸ **agentStatus**(): [`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Returns

[`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Defined in

[agent/AgentResolver.ts:30](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L30)

___

#### agentStatusChanged

▸ **agentStatusChanged**(): [`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Returns

[`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Defined in

[agent/AgentResolver.ts:124](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L124)

___

#### agentUnlock

▸ **agentUnlock**(`passphrase`, `pubSub`): [`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |
| `pubSub` | `any` |

##### Returns

[`AgentStatus`](#classesagent_agentstatusagentstatusmd)

##### Defined in

[agent/AgentResolver.ts:78](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L78)

___

#### agentUpdateDirectMessageLanguage

▸ **agentUpdateDirectMessageLanguage**(`directMessageLanguage`, `pubSub`): [`Agent`](#classesagent_agentagentmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `directMessageLanguage` | `string` |
| `pubSub` | `any` |

##### Returns

[`Agent`](#classesagent_agentagentmd)

##### Defined in

[agent/AgentResolver.ts:108](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L108)

___

#### agentUpdatePublicPerspective

▸ **agentUpdatePublicPerspective**(`perspective`, `pubSub`): [`Agent`](#classesagent_agentagentmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveInput`](#classesperspectives_perspectiveperspectiveinputmd) |
| `pubSub` | `any` |

##### Returns

[`Agent`](#classesagent_agentagentmd)

##### Defined in

[agent/AgentResolver.ts:97](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L97)

___

#### agentUpdated

▸ **agentUpdated**(): [`Agent`](#classesagent_agentagentmd)

##### Returns

[`Agent`](#classesagent_agentagentmd)

##### Defined in

[agent/AgentResolver.ts:119](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L119)


<a name="classesagent_agentstatusagentstatusmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/AgentStatus](#modulesagent_agentstatusmd) / AgentStatus

## Class: AgentStatus

[agent/AgentStatus](#modulesagent_agentstatusmd).AgentStatus

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [did](#did)
- [didDocument](#diddocument)
- [error](#error)
- [isInitialized](#isinitialized)
- [isUnlocked](#isunlocked)

### Constructors

#### constructor

• **new AgentStatus**(`obj?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `obj?` | `object` |

##### Defined in

[agent/AgentStatus.ts:20](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentStatus.ts#L20)

### Properties

#### did

• `Optional` **did**: `string`

##### Defined in

[agent/AgentStatus.ts:12](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentStatus.ts#L12)

___

#### didDocument

• `Optional` **didDocument**: `string`

##### Defined in

[agent/AgentStatus.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentStatus.ts#L15)

___

#### error

• `Optional` **error**: `string`

##### Defined in

[agent/AgentStatus.ts:18](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentStatus.ts#L18)

___

#### isInitialized

• **isInitialized**: `Boolean`

##### Defined in

[agent/AgentStatus.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentStatus.ts#L6)

___

#### isUnlocked

• **isUnlocked**: `Boolean`

##### Defined in

[agent/AgentStatus.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentStatus.ts#L9)


<a name="classesexpression_expressionexpressionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [expression/Expression](#modulesexpression_expressionmd) / Expression

## Class: Expression

[expression/Expression](#modulesexpression_expressionmd).Expression

### Hierarchy

- `any`

  ↳ **`Expression`**

### Table of contents

#### Constructors

- [constructor](#constructor)

### Constructors

#### constructor

• **new Expression**()

##### Inherited from

ExpressionGeneric(Object).constructor


<a name="classesexpression_expressionexpressionproofmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [expression/Expression](#modulesexpression_expressionmd) / ExpressionProof

## Class: ExpressionProof

[expression/Expression](#modulesexpression_expressionmd).ExpressionProof

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [invalid](#invalid)
- [key](#key)
- [signature](#signature)
- [valid](#valid)

### Constructors

#### constructor

• **new ExpressionProof**(`sig`, `k`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `sig` | `string` |
| `k` | `string` |

##### Defined in

[expression/Expression.ts:20](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L20)

### Properties

#### invalid

• `Optional` **invalid**: `boolean`

##### Defined in

[expression/Expression.ts:18](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L18)

___

#### key

• **key**: `string`

##### Defined in

[expression/Expression.ts:12](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L12)

___

#### signature

• **signature**: `string`

##### Defined in

[expression/Expression.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L9)

___

#### valid

• `Optional` **valid**: `boolean`

##### Defined in

[expression/Expression.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L15)


<a name="classesexpression_expressionexpressionproofinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [expression/Expression](#modulesexpression_expressionmd) / ExpressionProofInput

## Class: ExpressionProofInput

[expression/Expression](#modulesexpression_expressionmd).ExpressionProofInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [invalid](#invalid)
- [key](#key)
- [signature](#signature)
- [valid](#valid)

### Constructors

#### constructor

• **new ExpressionProofInput**()

### Properties

#### invalid

• `Optional` **invalid**: `boolean`

##### Defined in

[expression/Expression.ts:38](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L38)

___

#### key

• **key**: `string`

##### Defined in

[expression/Expression.ts:32](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L32)

___

#### signature

• **signature**: `string`

##### Defined in

[expression/Expression.ts:29](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L29)

___

#### valid

• `Optional` **valid**: `boolean`

##### Defined in

[expression/Expression.ts:35](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L35)


<a name="classesexpression_expressionexpressionrenderedmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [expression/Expression](#modulesexpression_expressionmd) / ExpressionRendered

## Class: ExpressionRendered

[expression/Expression](#modulesexpression_expressionmd).ExpressionRendered

### Hierarchy

- `any`

  ↳ **`ExpressionRendered`**

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [icon](#icon)
- [language](#language)

### Constructors

#### constructor

• **new ExpressionRendered**()

##### Inherited from

ExpressionGeneric(String).constructor

### Properties

#### icon

• **icon**: [`Icon`](#classeslanguage_iconiconmd)

##### Defined in

[expression/Expression.ts:94](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L94)

___

#### language

• **language**: [`LanguageRef`](#classeslanguage_languagereflanguagerefmd)

##### Defined in

[expression/Expression.ts:91](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L91)


<a name="classesexpression_expressionclientexpressionclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [expression/ExpressionClient](#modulesexpression_expressionclientmd) / ExpressionClient

## Class: ExpressionClient

[expression/ExpressionClient](#modulesexpression_expressionclientmd).ExpressionClient

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#apolloClient](##apolloclient)

#### Methods

- [create](#create)
- [get](#get)
- [getMany](#getmany)
- [getRaw](#getraw)
- [interact](#interact)
- [interactions](#interactions)

### Constructors

#### constructor

• **new ExpressionClient**(`client`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `client` | `ApolloClient`<`any`\> |

##### Defined in

[expression/ExpressionClient.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionClient.ts#L9)

### Properties

#### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

##### Defined in

[expression/ExpressionClient.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionClient.ts#L7)

### Methods

#### create

▸ **create**(`content`, `languageAddress`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `any` |
| `languageAddress` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[expression/ExpressionClient.ts:65](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionClient.ts#L65)

___

#### get

▸ **get**(`url`): `Promise`<[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

`Promise`<[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)\>

##### Defined in

[expression/ExpressionClient.ts:13](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionClient.ts#L13)

___

#### getMany

▸ **getMany**(`urls`): `Promise`<[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `urls` | `string`[] |

##### Returns

`Promise`<[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)[]\>

##### Defined in

[expression/ExpressionClient.ts:34](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionClient.ts#L34)

___

#### getRaw

▸ **getRaw**(`url`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[expression/ExpressionClient.ts:55](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionClient.ts#L55)

___

#### interact

▸ **interact**(`url`, `interactionCall`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |
| `interactionCall` | [`InteractionCall`](#classeslanguage_languageinteractioncallmd) |

##### Returns

`Promise`<`string`\>

##### Defined in

[expression/ExpressionClient.ts:90](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionClient.ts#L90)

___

#### interactions

▸ **interactions**(`url`): `Promise`<[`InteractionMeta`](#classeslanguage_languageinteractionmetamd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

`Promise`<[`InteractionMeta`](#classeslanguage_languageinteractionmetamd)[]\>

##### Defined in

[expression/ExpressionClient.ts:76](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionClient.ts#L76)


<a name="classesexpression_expressionrefexpressionrefmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [expression/ExpressionRef](#modulesexpression_expressionrefmd) / ExpressionRef

## Class: ExpressionRef

[expression/ExpressionRef](#modulesexpression_expressionrefmd).ExpressionRef

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [expression](#expression)
- [language](#language)

### Constructors

#### constructor

• **new ExpressionRef**(`lang`, `expr`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `lang` | [`LanguageRef`](#classeslanguage_languagereflanguagerefmd) |
| `expr` | `string` |

##### Defined in

[expression/ExpressionRef.ts:14](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionRef.ts#L14)

### Properties

#### expression

• **expression**: `string`

##### Defined in

[expression/ExpressionRef.ts:12](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionRef.ts#L12)

___

#### language

• **language**: [`LanguageRef`](#classeslanguage_languagereflanguagerefmd)

##### Defined in

[expression/ExpressionRef.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionRef.ts#L9)


<a name="classesexpression_expressionresolverdefaultmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [expression/ExpressionResolver](#modulesexpression_expressionresolvermd) / default

## Class: default

[expression/ExpressionResolver](#modulesexpression_expressionresolvermd).default

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Methods

- [expression](#expression)
- [expressionCreate](#expressioncreate)
- [expressionInteract](#expressioninteract)
- [expressionInteractions](#expressioninteractions)
- [expressionMany](#expressionmany)
- [expressionRaw](#expressionraw)

### Constructors

#### constructor

• **new default**()

### Methods

#### expression

▸ **expression**(`url`): [`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)

##### Defined in

[expression/ExpressionResolver.ts:16](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionResolver.ts#L16)

___

#### expressionCreate

▸ **expressionCreate**(`content`, `languageAddress`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `string` |
| `languageAddress` | `string` |

##### Returns

`string`

##### Defined in

[expression/ExpressionResolver.ts:39](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionResolver.ts#L39)

___

#### expressionInteract

▸ **expressionInteract**(`url`, `interactionCall`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |
| `interactionCall` | [`InteractionCall`](#classeslanguage_languageinteractioncallmd) |

##### Returns

`string`

##### Defined in

[expression/ExpressionResolver.ts:57](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionResolver.ts#L57)

___

#### expressionInteractions

▸ **expressionInteractions**(`url`): [`InteractionMeta`](#classeslanguage_languageinteractionmetamd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

[`InteractionMeta`](#classeslanguage_languageinteractionmetamd)[]

##### Defined in

[expression/ExpressionResolver.ts:47](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionResolver.ts#L47)

___

#### expressionMany

▸ **expressionMany**(`urls`): [`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `urls` | `string`[] |

##### Returns

[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)[]

##### Defined in

[expression/ExpressionResolver.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionResolver.ts#L25)

___

#### expressionRaw

▸ **expressionRaw**(`url`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

`string`

##### Defined in

[expression/ExpressionResolver.ts:30](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionResolver.ts#L30)


<a name="classeslanguage_iconiconmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Icon](#moduleslanguage_iconmd) / Icon

## Class: Icon

[language/Icon](#moduleslanguage_iconmd).Icon

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [code](#code)

### Constructors

#### constructor

• **new Icon**(`code`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `code` | `string` |

##### Defined in

[language/Icon.ts:8](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Icon.ts#L8)

### Properties

#### code

• **code**: `string`

##### Defined in

[language/Icon.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Icon.ts#L6)


<a name="classeslanguage_languageinteractioncallmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / InteractionCall

## Class: InteractionCall

[language/Language](#moduleslanguage_languagemd).InteractionCall

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [name](#name)
- [parametersStringified](#parametersstringified)

#### Accessors

- [parameters](#parameters)

### Constructors

#### constructor

• **new InteractionCall**(`name`, `parameters`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `name` | `string` |
| `parameters` | `object` |

##### Defined in

[language/Language.ts:244](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L244)

### Properties

#### name

• **name**: `string`

##### Defined in

[language/Language.ts:236](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L236)

___

#### parametersStringified

• **parametersStringified**: `string`

##### Defined in

[language/Language.ts:238](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L238)

### Accessors

#### parameters

• `get` **parameters**(): `object`

##### Returns

`object`

##### Defined in

[language/Language.ts:240](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L240)


<a name="classeslanguage_languageinteractionmetamd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / InteractionMeta

## Class: InteractionMeta

[language/Language](#moduleslanguage_languagemd).InteractionMeta

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [label](#label)
- [name](#name)
- [parameters](#parameters)

### Constructors

#### constructor

• **new InteractionMeta**()

### Properties

#### label

• **label**: `string`

##### Defined in

[language/Language.ts:218](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L218)

___

#### name

• **name**: `string`

##### Defined in

[language/Language.ts:221](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L221)

___

#### parameters

• **parameters**: [`InteractionParameter`](#classeslanguage_languageinteractionparametermd)[]

##### Defined in

[language/Language.ts:224](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L224)


<a name="classeslanguage_languageinteractionparametermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / InteractionParameter

## Class: InteractionParameter

[language/Language](#moduleslanguage_languagemd).InteractionParameter

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [name](#name)
- [type](#type)

### Constructors

#### constructor

• **new InteractionParameter**()

### Properties

#### name

• **name**: `string`

##### Defined in

[language/Language.ts:209](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L209)

___

#### type

• **type**: `string`

##### Defined in

[language/Language.ts:212](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L212)


<a name="classeslanguage_languageonlineagentmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / OnlineAgent

## Class: OnlineAgent

[language/Language](#moduleslanguage_languagemd).OnlineAgent

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [did](#did)
- [status](#status)

### Constructors

#### constructor

• **new OnlineAgent**()

### Properties

#### did

• **did**: `string`

##### Defined in

[language/Language.ts:253](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L253)

___

#### status

• **status**: [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)

##### Defined in

[language/Language.ts:255](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L255)


<a name="classeslanguage_languageclientlanguageclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageClient](#moduleslanguage_languageclientmd) / LanguageClient

## Class: LanguageClient

[language/LanguageClient](#moduleslanguage_languageclientmd).LanguageClient

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#apolloClient](##apolloclient)

#### Methods

- [all](#all)
- [applyTemplateAndPublish](#applytemplateandpublish)
- [byAddress](#byaddress)
- [byFilter](#byfilter)
- [meta](#meta)
- [publish](#publish)
- [remove](#remove)
- [source](#source)
- [writeSettings](#writesettings)

### Constructors

#### constructor

• **new LanguageClient**(`apolloClient`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `apolloClient` | `ApolloClient`<`any`\> |

##### Defined in

[language/LanguageClient.ts:31](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L31)

### Properties

#### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

##### Defined in

[language/LanguageClient.ts:29](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L29)

### Methods

#### all

▸ **all**(): `Promise`<[`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)[]\>

##### Returns

`Promise`<[`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)[]\>

##### Defined in

[language/LanguageClient.ts:59](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L59)

___

#### applyTemplateAndPublish

▸ **applyTemplateAndPublish**(`sourceLanguageHash`, `templateData`): `Promise`<[`LanguageRef`](#classeslanguage_languagereflanguagerefmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `sourceLanguageHash` | `string` |
| `templateData` | `string` |

##### Returns

`Promise`<[`LanguageRef`](#classeslanguage_languagereflanguagerefmd)\>

##### Defined in

[language/LanguageClient.ts:76](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L76)

___

#### byAddress

▸ **byAddress**(`address`): `Promise`<[`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

`Promise`<[`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)\>

##### Defined in

[language/LanguageClient.ts:35](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L35)

___

#### byFilter

▸ **byFilter**(`filter`): `Promise`<[`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `filter` | `string` |

##### Returns

`Promise`<[`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)[]\>

##### Defined in

[language/LanguageClient.ts:47](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L47)

___

#### meta

▸ **meta**(`address`): `Promise`<[`LanguageMeta`](#classeslanguage_languagemetalanguagemetamd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

`Promise`<[`LanguageMeta`](#classeslanguage_languagemetalanguagemetamd)\>

##### Defined in

[language/LanguageClient.ts:114](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L114)

___

#### publish

▸ **publish**(`languagePath`, `languageMeta`): `Promise`<[`LanguageMeta`](#classeslanguage_languagemetalanguagemetamd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `languagePath` | `string` |
| `languageMeta` | [`LanguageMetaInput`](#classeslanguage_languagemetalanguagemetainputmd) |

##### Returns

`Promise`<[`LanguageMeta`](#classeslanguage_languagemetalanguagemetamd)\>

##### Defined in

[language/LanguageClient.ts:95](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L95)

___

#### remove

▸ **remove**(`address`): `Promise`<`Boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

`Promise`<`Boolean`\>

##### Defined in

[language/LanguageClient.ts:146](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L146)

___

#### source

▸ **source**(`address`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[language/LanguageClient.ts:131](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L131)

___

#### writeSettings

▸ **writeSettings**(`languageAddress`, `settings`): `Promise`<`Boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `languageAddress` | `string` |
| `settings` | `string` |

##### Returns

`Promise`<`Boolean`\>

##### Defined in

[language/LanguageClient.ts:63](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageClient.ts#L63)


<a name="classeslanguage_languagecontextdnamd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageContext](#moduleslanguage_languagecontextmd) / Dna

## Class: Dna

[language/LanguageContext](#moduleslanguage_languagecontextmd).Dna

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [file](#file)
- [nick](#nick)
- [zomeCalls](#zomecalls)

### Constructors

#### constructor

• **new Dna**()

### Properties

#### file

• **file**: `Buffer`

##### Defined in

[language/LanguageContext.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L25)

___

#### nick

• **nick**: `string`

##### Defined in

[language/LanguageContext.ts:26](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L26)

___

#### zomeCalls

• **zomeCalls**: [`string`, `string`][]

##### Defined in

[language/LanguageContext.ts:27](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L27)


<a name="classeslanguage_languagehandlelanguagehandlemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageHandle](#moduleslanguage_languagehandlemd) / LanguageHandle

## Class: LanguageHandle

[language/LanguageHandle](#moduleslanguage_languagehandlemd).LanguageHandle

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [address](#address)
- [constructorIcon](#constructoricon)
- [icon](#icon)
- [name](#name)
- [settings](#settings)
- [settingsIcon](#settingsicon)

### Constructors

#### constructor

• **new LanguageHandle**()

### Properties

#### address

• **address**: `string`

##### Defined in

[language/LanguageHandle.ts:10](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageHandle.ts#L10)

___

#### constructorIcon

• `Optional` **constructorIcon**: [`Icon`](#classeslanguage_iconiconmd)

##### Defined in

[language/LanguageHandle.ts:19](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageHandle.ts#L19)

___

#### icon

• `Optional` **icon**: [`Icon`](#classeslanguage_iconiconmd)

##### Defined in

[language/LanguageHandle.ts:16](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageHandle.ts#L16)

___

#### name

• **name**: `string`

##### Defined in

[language/LanguageHandle.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageHandle.ts#L7)

___

#### settings

• `Optional` **settings**: `string`

##### Defined in

[language/LanguageHandle.ts:13](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageHandle.ts#L13)

___

#### settingsIcon

• `Optional` **settingsIcon**: [`Icon`](#classeslanguage_iconiconmd)

##### Defined in

[language/LanguageHandle.ts:22](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageHandle.ts#L22)


<a name="classeslanguage_languagemetalanguageexpressionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageMeta](#moduleslanguage_languagemetamd) / LanguageExpression

## Class: LanguageExpression

[language/LanguageMeta](#moduleslanguage_languagemetamd).LanguageExpression

### Hierarchy

- `any`

  ↳ **`LanguageExpression`**

### Table of contents

#### Constructors

- [constructor](#constructor)

### Constructors

#### constructor

• **new LanguageExpression**()

##### Inherited from

ExpressionGeneric(LanguageMetaInternal).constructor


<a name="classeslanguage_languagemetalanguagelanguageinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageMeta](#moduleslanguage_languagemetamd) / LanguageLanguageInput

## Class: LanguageLanguageInput

[language/LanguageMeta](#moduleslanguage_languagemetamd).LanguageLanguageInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [bundle](#bundle)
- [meta](#meta)

### Constructors

#### constructor

• **new LanguageLanguageInput**()

### Properties

#### bundle

• **bundle**: `string`

##### Defined in

[language/LanguageMeta.ts:68](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L68)

___

#### meta

• **meta**: [`LanguageMetaInternal`](#classeslanguage_languagemetalanguagemetainternalmd)

##### Defined in

[language/LanguageMeta.ts:69](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L69)


<a name="classeslanguage_languagemetalanguagemetamd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageMeta](#moduleslanguage_languagemetamd) / LanguageMeta

## Class: LanguageMeta

[language/LanguageMeta](#moduleslanguage_languagemetamd).LanguageMeta

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [address](#address)
- [author](#author)
- [description](#description)
- [name](#name)
- [possibleTemplateParams](#possibletemplateparams)
- [sourceCodeLink](#sourcecodelink)
- [templateAppliedParams](#templateappliedparams)
- [templateSourceLanguageAddress](#templatesourcelanguageaddress)
- [templated](#templated)

### Constructors

#### constructor

• **new LanguageMeta**()

### Properties

#### address

• **address**: `string`

##### Defined in

[language/LanguageMeta.ts:10](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L10)

___

#### author

• **author**: `string`

##### Defined in

[language/LanguageMeta.ts:16](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L16)

___

#### description

• **description**: `string`

##### Defined in

[language/LanguageMeta.ts:13](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L13)

___

#### name

• **name**: `string`

##### Defined in

[language/LanguageMeta.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L7)

___

#### possibleTemplateParams

• `Optional` **possibleTemplateParams**: `string`[]

##### Defined in

[language/LanguageMeta.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L28)

___

#### sourceCodeLink

• `Optional` **sourceCodeLink**: `string`

##### Defined in

[language/LanguageMeta.ts:31](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L31)

___

#### templateAppliedParams

• `Optional` **templateAppliedParams**: `string`

##### Defined in

[language/LanguageMeta.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L25)

___

#### templateSourceLanguageAddress

• `Optional` **templateSourceLanguageAddress**: `string`

##### Defined in

[language/LanguageMeta.ts:22](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L22)

___

#### templated

• **templated**: `boolean`

##### Defined in

[language/LanguageMeta.ts:19](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L19)


<a name="classeslanguage_languagemetalanguagemetainputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageMeta](#moduleslanguage_languagemetamd) / LanguageMetaInput

## Class: LanguageMetaInput

[language/LanguageMeta](#moduleslanguage_languagemetamd).LanguageMetaInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [description](#description)
- [name](#name)
- [possibleTemplateParams](#possibletemplateparams)
- [sourceCodeLink](#sourcecodelink)

### Constructors

#### constructor

• **new LanguageMetaInput**(`name?`, `description?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `name?` | `string` |
| `description?` | `string` |

##### Defined in

[language/LanguageMeta.ts:48](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L48)

### Properties

#### description

• **description**: `string`

##### Defined in

[language/LanguageMeta.ts:40](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L40)

___

#### name

• **name**: `string`

##### Defined in

[language/LanguageMeta.ts:37](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L37)

___

#### possibleTemplateParams

• `Optional` **possibleTemplateParams**: `string`[]

##### Defined in

[language/LanguageMeta.ts:43](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L43)

___

#### sourceCodeLink

• `Optional` **sourceCodeLink**: `string`

##### Defined in

[language/LanguageMeta.ts:46](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L46)


<a name="classeslanguage_languagemetalanguagemetainternalmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageMeta](#moduleslanguage_languagemetamd) / LanguageMetaInternal

## Class: LanguageMetaInternal

[language/LanguageMeta](#moduleslanguage_languagemetamd).LanguageMetaInternal

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [address](#address)
- [description](#description)
- [name](#name)
- [possibleTemplateParams](#possibletemplateparams)
- [sourceCodeLink](#sourcecodelink)
- [templateAppliedParams](#templateappliedparams)
- [templateSourceLanguageAddress](#templatesourcelanguageaddress)

### Constructors

#### constructor

• **new LanguageMetaInternal**()

### Properties

#### address

• **address**: `string`

##### Defined in

[language/LanguageMeta.ts:57](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L57)

___

#### description

• **description**: `string`

##### Defined in

[language/LanguageMeta.ts:58](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L58)

___

#### name

• **name**: `string`

##### Defined in

[language/LanguageMeta.ts:56](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L56)

___

#### possibleTemplateParams

• `Optional` **possibleTemplateParams**: `string`[]

##### Defined in

[language/LanguageMeta.ts:61](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L61)

___

#### sourceCodeLink

• `Optional` **sourceCodeLink**: `string`

##### Defined in

[language/LanguageMeta.ts:62](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L62)

___

#### templateAppliedParams

• `Optional` **templateAppliedParams**: `string`

##### Defined in

[language/LanguageMeta.ts:60](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L60)

___

#### templateSourceLanguageAddress

• `Optional` **templateSourceLanguageAddress**: `string`

##### Defined in

[language/LanguageMeta.ts:59](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageMeta.ts#L59)


<a name="classeslanguage_languagereflanguagerefmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageRef](#moduleslanguage_languagerefmd) / LanguageRef

## Class: LanguageRef

[language/LanguageRef](#moduleslanguage_languagerefmd).LanguageRef

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [address](#address)
- [name](#name)

### Constructors

#### constructor

• **new LanguageRef**(`address?`, `name?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `address?` | `string` |
| `name?` | `string` |

##### Defined in

[language/LanguageRef.ts:14](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageRef.ts#L14)

### Properties

#### address

• **address**: `string`

##### Defined in

[language/LanguageRef.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageRef.ts#L9)

___

#### name

• **name**: `string`

##### Defined in

[language/LanguageRef.ts:12](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageRef.ts#L12)


<a name="classeslanguage_languageresolverdefaultmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageResolver](#moduleslanguage_languageresolvermd) / default

## Class: default

[language/LanguageResolver](#moduleslanguage_languageresolvermd).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Methods

- [language](#language)
- [languageApplyTemplateAndPublish](#languageapplytemplateandpublish)
- [languageMeta](#languagemeta)
- [languagePublish](#languagepublish)
- [languageRemove](#languageremove)
- [languageSource](#languagesource)
- [languageWriteSettings](#languagewritesettings)
- [languages](#languages)

### Constructors

#### constructor

• **new default**()

### Methods

#### language

▸ **language**(`address`): [`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

[`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)

##### Defined in

[language/LanguageResolver.ts:16](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageResolver.ts#L16)

___

#### languageApplyTemplateAndPublish

▸ **languageApplyTemplateAndPublish**(`sourceLanguageHash`, `templateData`): [`LanguageRef`](#classeslanguage_languagereflanguagerefmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `sourceLanguageHash` | `string` |
| `templateData` | `string` |

##### Returns

[`LanguageRef`](#classeslanguage_languagereflanguagerefmd)

##### Defined in

[language/LanguageResolver.ts:49](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageResolver.ts#L49)

___

#### languageMeta

▸ **languageMeta**(`address`): [`LanguageMeta`](#classeslanguage_languagemetalanguagemetamd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

[`LanguageMeta`](#classeslanguage_languagemetalanguagemetamd)

##### Defined in

[language/LanguageResolver.ts:75](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageResolver.ts#L75)

___

#### languagePublish

▸ **languagePublish**(`languagePath`, `languageMeta`): [`LanguageMeta`](#classeslanguage_languagemetalanguagemetamd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `languagePath` | `string` |
| `languageMeta` | [`LanguageMetaInput`](#classeslanguage_languagemetalanguagemetainputmd) |

##### Returns

[`LanguageMeta`](#classeslanguage_languagemetalanguagemetamd)

##### Defined in

[language/LanguageResolver.ts:57](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageResolver.ts#L57)

___

#### languageRemove

▸ **languageRemove**(`address`): `Boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

`Boolean`

##### Defined in

[language/LanguageResolver.ts:95](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageResolver.ts#L95)

___

#### languageSource

▸ **languageSource**(`address`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

`string`

##### Defined in

[language/LanguageResolver.ts:90](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageResolver.ts#L90)

___

#### languageWriteSettings

▸ **languageWriteSettings**(`languageAddress`, `settings`): `Boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `languageAddress` | `string` |
| `settings` | `string` |

##### Returns

`Boolean`

##### Defined in

[language/LanguageResolver.ts:41](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageResolver.ts#L41)

___

#### languages

▸ **languages**(`filter`): [`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `filter` | `string` |

##### Returns

[`LanguageHandle`](#classeslanguage_languagehandlelanguagehandlemd)[]

##### Defined in

[language/LanguageResolver.ts:29](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageResolver.ts#L29)


<a name="classeslinks_linkslinkmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [links/Links](#moduleslinks_linksmd) / Link

## Class: Link

[links/Links](#moduleslinks_linksmd).Link

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [predicate](#predicate)
- [source](#source)
- [target](#target)

### Constructors

#### constructor

• **new Link**(`obj`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `obj` | `any` |

##### Defined in

[links/Links.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L15)

### Properties

#### predicate

• `Optional` **predicate**: `string`

##### Defined in

[links/Links.ts:13](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L13)

___

#### source

• **source**: `string`

##### Defined in

[links/Links.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L7)

___

#### target

• **target**: `string`

##### Defined in

[links/Links.ts:10](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L10)


<a name="classeslinks_linkslinkexpressionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [links/Links](#moduleslinks_linksmd) / LinkExpression

## Class: LinkExpression

[links/Links](#moduleslinks_linksmd).LinkExpression

### Hierarchy

- `any`

  ↳ **`LinkExpression`**

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Methods

- [hash](#hash)

### Constructors

#### constructor

• **new LinkExpression**()

##### Inherited from

ExpressionGeneric(Link).constructor

### Methods

#### hash

▸ **hash**(): `number`

##### Returns

`number`

##### Defined in

[links/Links.ts:58](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L58)


<a name="classeslinks_linkslinkexpressioninputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [links/Links](#moduleslinks_linksmd) / LinkExpressionInput

## Class: LinkExpressionInput

[links/Links](#moduleslinks_linksmd).LinkExpressionInput

### Hierarchy

- `any`

  ↳ **`LinkExpressionInput`**

### Table of contents

#### Constructors

- [constructor](#constructor)

### Constructors

#### constructor

• **new LinkExpressionInput**()

##### Inherited from

ExpressionGenericInput(LinkInput).constructor


<a name="classeslinks_linkslinkexpressionmutationsmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [links/Links](#moduleslinks_linksmd) / LinkExpressionMutations

## Class: LinkExpressionMutations

[links/Links](#moduleslinks_linksmd).LinkExpressionMutations

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [additions](#additions)
- [removals](#removals)

### Constructors

#### constructor

• **new LinkExpressionMutations**(`additions`, `removals`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `additions` | [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[] |
| `removals` | [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[] |

##### Defined in

[links/Links.ts:38](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L38)

### Properties

#### additions

• **additions**: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Defined in

[links/Links.ts:33](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L33)

___

#### removals

• **removals**: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Defined in

[links/Links.ts:36](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L36)


<a name="classeslinks_linkslinkexpressionupdatedmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [links/Links](#moduleslinks_linksmd) / LinkExpressionUpdated

## Class: LinkExpressionUpdated

[links/Links](#moduleslinks_linksmd).LinkExpressionUpdated

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [newLink](#newlink)
- [oldLink](#oldlink)

### Constructors

#### constructor

• **new LinkExpressionUpdated**(`oldLink`, `newLink`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `oldLink` | [`LinkExpression`](#classeslinks_linkslinkexpressionmd) |
| `newLink` | [`LinkExpression`](#classeslinks_linkslinkexpressionmd) |

##### Defined in

[links/Links.ts:94](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L94)

### Properties

#### newLink

• **newLink**: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Defined in

[links/Links.ts:92](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L92)

___

#### oldLink

• **oldLink**: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Defined in

[links/Links.ts:89](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L89)


<a name="classeslinks_linkslinkinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [links/Links](#moduleslinks_linksmd) / LinkInput

## Class: LinkInput

[links/Links](#moduleslinks_linksmd).LinkInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [predicate](#predicate)
- [source](#source)
- [target](#target)

### Constructors

#### constructor

• **new LinkInput**()

### Properties

#### predicate

• `Optional` **predicate**: `string`

##### Defined in

[links/Links.ts:53](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L53)

___

#### source

• **source**: `string`

##### Defined in

[links/Links.ts:47](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L47)

___

#### target

• **target**: `string`

##### Defined in

[links/Links.ts:50](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L50)


<a name="classeslinks_linkslinkmutationsmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [links/Links](#moduleslinks_linksmd) / LinkMutations

## Class: LinkMutations

[links/Links](#moduleslinks_linksmd).LinkMutations

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [additions](#additions)
- [removals](#removals)

### Constructors

#### constructor

• **new LinkMutations**()

### Properties

#### additions

• **additions**: [`LinkInput`](#classeslinks_linkslinkinputmd)[]

##### Defined in

[links/Links.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L25)

___

#### removals

• **removals**: [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd)[]

##### Defined in

[links/Links.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L28)


<a name="classesneighbourhood_neighbourhoodneighbourhoodmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [neighbourhood/Neighbourhood](#modulesneighbourhood_neighbourhoodmd) / Neighbourhood

## Class: Neighbourhood

[neighbourhood/Neighbourhood](#modulesneighbourhood_neighbourhoodmd).Neighbourhood

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [linkLanguage](#linklanguage)
- [meta](#meta)

### Constructors

#### constructor

• **new Neighbourhood**(`linkLanguage`, `meta`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `linkLanguage` | `string` |
| `meta` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Defined in

[neighbourhood/Neighbourhood.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/Neighbourhood.ts#L15)

### Properties

#### linkLanguage

• **linkLanguage**: `string`

##### Defined in

[neighbourhood/Neighbourhood.ts:10](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/Neighbourhood.ts#L10)

___

#### meta

• **meta**: [`Perspective`](#classesperspectives_perspectiveperspectivemd)

##### Defined in

[neighbourhood/Neighbourhood.ts:13](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/Neighbourhood.ts#L13)


<a name="classesneighbourhood_neighbourhoodneighbourhoodexpressionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [neighbourhood/Neighbourhood](#modulesneighbourhood_neighbourhoodmd) / NeighbourhoodExpression

## Class: NeighbourhoodExpression

[neighbourhood/Neighbourhood](#modulesneighbourhood_neighbourhoodmd).NeighbourhoodExpression

### Hierarchy

- `any`

  ↳ **`NeighbourhoodExpression`**

### Table of contents

#### Constructors

- [constructor](#constructor)

### Constructors

#### constructor

• **new NeighbourhoodExpression**()

##### Inherited from

ExpressionGeneric(Neighbourhood).constructor


<a name="classesneighbourhood_neighbourhoodclientneighbourhoodclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [neighbourhood/NeighbourhoodClient](#modulesneighbourhood_neighbourhoodclientmd) / NeighbourhoodClient

## Class: NeighbourhoodClient

[neighbourhood/NeighbourhoodClient](#modulesneighbourhood_neighbourhoodclientmd).NeighbourhoodClient

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#apolloClient](##apolloclient)
- [#signalHandlers](##signalhandlers)

#### Methods

- [addSignalHandler](#addsignalhandler)
- [dispatchSignal](#dispatchsignal)
- [hasTelepresenceAdapter](#hastelepresenceadapter)
- [joinFromUrl](#joinfromurl)
- [onlineAgents](#onlineagents)
- [otherAgents](#otheragents)
- [publishFromPerspective](#publishfromperspective)
- [removeSignalHandler](#removesignalhandler)
- [sendBroadcast](#sendbroadcast)
- [sendBroadcastU](#sendbroadcastu)
- [sendSignal](#sendsignal)
- [sendSignalU](#sendsignalu)
- [setOnlineStatus](#setonlinestatus)
- [setOnlineStatusU](#setonlinestatusu)
- [subscribeToSignals](#subscribetosignals)

### Constructors

#### constructor

• **new NeighbourhoodClient**(`client`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `client` | `ApolloClient`<`any`\> |

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:14](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L14)

### Properties

#### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:11](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L11)

___

#### #signalHandlers

• `Private` **#signalHandlers**: `Map`<`string`, [`TelepresenceSignalCallback`](#telepresencesignalcallback)[]\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:12](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L12)

### Methods

#### addSignalHandler

▸ **addSignalHandler**(`perspectiveUUID`, `handler`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `handler` | [`TelepresenceSignalCallback`](#telepresencesignalcallback) |

##### Returns

`Promise`<`void`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:253](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L253)

___

#### dispatchSignal

▸ **dispatchSignal**(`perspectiveUUID`, `signal`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `signal` | `any` |

##### Returns

`void`

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:218](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L218)

___

#### hasTelepresenceAdapter

▸ **hasTelepresenceAdapter**(`perspectiveUUID`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:77](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L77)

___

#### joinFromUrl

▸ **joinFromUrl**(`url`): `Promise`<[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

`Promise`<[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:40](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L40)

___

#### onlineAgents

▸ **onlineAgents**(`perspectiveUUID`): `Promise`<[`OnlineAgent`](#classeslanguage_languageonlineagentmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

##### Returns

`Promise`<[`OnlineAgent`](#classeslanguage_languageonlineagentmd)[]\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:87](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L87)

___

#### otherAgents

▸ **otherAgents**(`perspectiveUUID`): `Promise`<`string`[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:67](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L67)

___

#### publishFromPerspective

▸ **publishFromPerspective**(`perspectiveUUID`, `linkLanguage`, `meta`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `linkLanguage` | `string` |
| `meta` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`string`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:18](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L18)

___

#### removeSignalHandler

▸ **removeSignalHandler**(`perspectiveUUID`, `handler`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `handler` | [`TelepresenceSignalCallback`](#telepresencesignalcallback) |

##### Returns

`void`

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:263](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L263)

___

#### sendBroadcast

▸ **sendBroadcast**(`perspectiveUUID`, `payload`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `payload` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:184](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L184)

___

#### sendBroadcastU

▸ **sendBroadcastU**(`perspectiveUUID`, `payload`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `payload` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:201](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L201)

___

#### sendSignal

▸ **sendSignal**(`perspectiveUUID`, `remoteAgentDid`, `payload`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `remoteAgentDid` | `string` |
| `payload` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:146](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L146)

___

#### sendSignalU

▸ **sendSignalU**(`perspectiveUUID`, `remoteAgentDid`, `payload`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `remoteAgentDid` | `string` |
| `payload` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:165](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L165)

___

#### setOnlineStatus

▸ **setOnlineStatus**(`perspectiveUUID`, `status`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `status` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:112](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L112)

___

#### setOnlineStatusU

▸ **setOnlineStatusU**(`perspectiveUUID`, `status`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `status` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:129](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L129)

___

#### subscribeToSignals

▸ **subscribeToSignals**(`perspectiveUUID`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

##### Returns

`Promise`<`void`\>

##### Defined in

[neighbourhood/NeighbourhoodClient.ts:225](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodClient.ts#L225)


<a name="classesneighbourhood_neighbourhoodproxyneighbourhoodproxymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [neighbourhood/NeighbourhoodProxy](#modulesneighbourhood_neighbourhoodproxymd) / NeighbourhoodProxy

## Class: NeighbourhoodProxy

[neighbourhood/NeighbourhoodProxy](#modulesneighbourhood_neighbourhoodproxymd).NeighbourhoodProxy

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#client](##client)
- [#pID](##pid)

#### Methods

- [addSignalHandler](#addsignalhandler)
- [hasTelepresenceAdapter](#hastelepresenceadapter)
- [onlineAgents](#onlineagents)
- [otherAgents](#otheragents)
- [removeSignalHandler](#removesignalhandler)
- [sendBroadcast](#sendbroadcast)
- [sendBroadcastU](#sendbroadcastu)
- [sendSignal](#sendsignal)
- [sendSignalU](#sendsignalu)
- [setOnlineStatus](#setonlinestatus)
- [setOnlineStatusU](#setonlinestatusu)

### Constructors

#### constructor

• **new NeighbourhoodProxy**(`client`, `pID`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `client` | [`NeighbourhoodClient`](#classesneighbourhood_neighbourhoodclientneighbourhoodclientmd) |
| `pID` | `string` |

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:10](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L10)

### Properties

#### #client

• `Private` **#client**: [`NeighbourhoodClient`](#classesneighbourhood_neighbourhoodclientneighbourhoodclientmd)

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L7)

___

#### #pID

• `Private` **#pID**: `string`

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:8](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L8)

### Methods

#### addSignalHandler

▸ **addSignalHandler**(`handler`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `handler` | (`payload`: [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)) => `void` |

##### Returns

`Promise`<`void`\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:51](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L51)

___

#### hasTelepresenceAdapter

▸ **hasTelepresenceAdapter**(): `Promise`<`boolean`\>

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:19](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L19)

___

#### onlineAgents

▸ **onlineAgents**(): `Promise`<[`OnlineAgent`](#classeslanguage_languageonlineagentmd)[]\>

##### Returns

`Promise`<[`OnlineAgent`](#classeslanguage_languageonlineagentmd)[]\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:23](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L23)

___

#### otherAgents

▸ **otherAgents**(): `Promise`<`string`[]\>

##### Returns

`Promise`<`string`[]\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L15)

___

#### removeSignalHandler

▸ **removeSignalHandler**(`handler`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `handler` | (`payload`: [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)) => `void` |

##### Returns

`void`

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:55](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L55)

___

#### sendBroadcast

▸ **sendBroadcast**(`payload`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:43](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L43)

___

#### sendBroadcastU

▸ **sendBroadcastU**(`payload`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:47](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L47)

___

#### sendSignal

▸ **sendSignal**(`remoteAgentDid`, `payload`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `remoteAgentDid` | `string` |
| `payload` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:35](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L35)

___

#### sendSignalU

▸ **sendSignalU**(`remoteAgentDid`, `payload`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `remoteAgentDid` | `string` |
| `payload` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:39](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L39)

___

#### setOnlineStatus

▸ **setOnlineStatus**(`status`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:27](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L27)

___

#### setOnlineStatusU

▸ **setOnlineStatusU**(`status`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[neighbourhood/NeighbourhoodProxy.ts:31](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodProxy.ts#L31)


<a name="classesneighbourhood_neighbourhoodresolverdefaultmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [neighbourhood/NeighbourhoodResolver](#modulesneighbourhood_neighbourhoodresolvermd) / default

## Class: default

[neighbourhood/NeighbourhoodResolver](#modulesneighbourhood_neighbourhoodresolvermd).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Methods

- [neighbourhoodHasTelepresenceAdapter](#neighbourhoodhastelepresenceadapter)
- [neighbourhoodJoinFromUrl](#neighbourhoodjoinfromurl)
- [neighbourhoodOnlineAgents](#neighbourhoodonlineagents)
- [neighbourhoodOtherAgents](#neighbourhoodotheragents)
- [neighbourhoodPublishFromPerspective](#neighbourhoodpublishfromperspective)
- [neighbourhoodSendBroadcast](#neighbourhoodsendbroadcast)
- [neighbourhoodSendBroadcastU](#neighbourhoodsendbroadcastu)
- [neighbourhoodSendSignal](#neighbourhoodsendsignal)
- [neighbourhoodSendSignalU](#neighbourhoodsendsignalu)
- [neighbourhoodSetOnlineStatus](#neighbourhoodsetonlinestatus)
- [neighbourhoodSetOnlineStatusU](#neighbourhoodsetonlinestatusu)
- [neighbourhoodSignal](#neighbourhoodsignal)

### Constructors

#### constructor

• **new default**()

### Methods

#### neighbourhoodHasTelepresenceAdapter

▸ **neighbourhoodHasTelepresenceAdapter**(`perspectiveUUID`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

##### Returns

`boolean`

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:53](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L53)

___

#### neighbourhoodJoinFromUrl

▸ **neighbourhoodJoinFromUrl**(`url`, `pubSub`): [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |
| `pubSub` | `any` |

##### Returns

[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:37](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L37)

___

#### neighbourhoodOnlineAgents

▸ **neighbourhoodOnlineAgents**(`perspectiveUUID`): [`OnlineAgent`](#classeslanguage_languageonlineagentmd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

##### Returns

[`OnlineAgent`](#classeslanguage_languageonlineagentmd)[]

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:58](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L58)

___

#### neighbourhoodOtherAgents

▸ **neighbourhoodOtherAgents**(`perspectiveUUID`): `string`[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

##### Returns

`string`[]

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:48](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L48)

___

#### neighbourhoodPublishFromPerspective

▸ **neighbourhoodPublishFromPerspective**(`perspectiveUUID`, `linkLanguage`, `meta`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `linkLanguage` | `string` |
| `meta` | [`PerspectiveInput`](#classesperspectives_perspectiveperspectiveinputmd) |

##### Returns

`string`

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L28)

___

#### neighbourhoodSendBroadcast

▸ **neighbourhoodSendBroadcast**(`perspectiveUUID`, `signal`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `signal` | [`PerspectiveInput`](#classesperspectives_perspectiveperspectiveinputmd) |

##### Returns

`boolean`

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:86](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L86)

___

#### neighbourhoodSendBroadcastU

▸ **neighbourhoodSendBroadcastU**(`perspectiveUUID`, `signal`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `signal` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`boolean`

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:91](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L91)

___

#### neighbourhoodSendSignal

▸ **neighbourhoodSendSignal**(`perspectiveUUID`, `recipient`, `signal`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `recipient` | `string` |
| `signal` | [`PerspectiveInput`](#classesperspectives_perspectiveperspectiveinputmd) |

##### Returns

`boolean`

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:76](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L76)

___

#### neighbourhoodSendSignalU

▸ **neighbourhoodSendSignalU**(`perspectiveUUID`, `recipient`, `signal`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `recipient` | `string` |
| `signal` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`boolean`

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:81](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L81)

___

#### neighbourhoodSetOnlineStatus

▸ **neighbourhoodSetOnlineStatus**(`perspectiveUUID`, `status`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `status` | [`PerspectiveInput`](#classesperspectives_perspectiveperspectiveinputmd) |

##### Returns

`boolean`

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:66](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L66)

___

#### neighbourhoodSetOnlineStatusU

▸ **neighbourhoodSetOnlineStatusU**(`perspectiveUUID`, `status`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `status` | [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd) |

##### Returns

`boolean`

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:71](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L71)

___

#### neighbourhoodSignal

▸ **neighbourhoodSignal**(`pID`): [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `pID` | `string` |

##### Returns

[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)

##### Defined in

[neighbourhood/NeighbourhoodResolver.ts:96](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/neighbourhood/NeighbourhoodResolver.ts#L96)


<a name="classesperspectives_linkquerylinkquerymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/LinkQuery](#modulesperspectives_linkquerymd) / LinkQuery

## Class: LinkQuery

[perspectives/LinkQuery](#modulesperspectives_linkquerymd).LinkQuery

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [fromDate](#fromdate)
- [limit](#limit)
- [predicate](#predicate)
- [source](#source)
- [target](#target)
- [untilDate](#untildate)

#### Methods

- [isMatch](#ismatch)

### Constructors

#### constructor

• **new LinkQuery**(`obj`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `obj` | `object` |

##### Defined in

[perspectives/LinkQuery.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/LinkQuery.ts#L25)

### Properties

#### fromDate

• `Optional` **fromDate**: `Date`

##### Defined in

[perspectives/LinkQuery.ts:17](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/LinkQuery.ts#L17)

___

#### limit

• `Optional` **limit**: `number`

##### Defined in

[perspectives/LinkQuery.ts:23](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/LinkQuery.ts#L23)

___

#### predicate

• `Optional` **predicate**: `string`

##### Defined in

[perspectives/LinkQuery.ts:14](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/LinkQuery.ts#L14)

___

#### source

• `Optional` **source**: `string`

##### Defined in

[perspectives/LinkQuery.ts:8](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/LinkQuery.ts#L8)

___

#### target

• `Optional` **target**: `string`

##### Defined in

[perspectives/LinkQuery.ts:11](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/LinkQuery.ts#L11)

___

#### untilDate

• `Optional` **untilDate**: `Date`

##### Defined in

[perspectives/LinkQuery.ts:20](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/LinkQuery.ts#L20)

### Methods

#### isMatch

▸ **isMatch**(`l`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `l` | [`Link`](#classeslinks_linkslinkmd) |

##### Returns

`boolean`

##### Defined in

[perspectives/LinkQuery.ts:51](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/LinkQuery.ts#L51)


<a name="classesperspectives_perspectiveperspectivemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/Perspective](#modulesperspectives_perspectivemd) / Perspective

## Class: Perspective

[perspectives/Perspective](#modulesperspectives_perspectivemd).Perspective

A Perspective represents subjective meaning, encoded through
associations between expressions, a.k.a. Links, that is a graph
over the objective Expressions of any subset of Languages.

This type represents the clean onotological concept of a Perspective.
An instance of this class can be regarded as an immutable snapshot of 
a mutable perspective.

The types PerspectiveProxy and PerspectiveHandle are used when dealing 
with an instantiated mutable perspective as is done through most of 
the GraphQL mutations.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [links](#links)

#### Methods

- [get](#get)
- [getSingleTarget](#getsingletarget)

### Constructors

#### constructor

• **new Perspective**(`links?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `links?` | [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[] |

##### Defined in

[perspectives/Perspective.ts:24](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/Perspective.ts#L24)

### Properties

#### links

• **links**: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

The content of the perspective, a list/graph of links

##### Defined in

[perspectives/Perspective.ts:22](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/Perspective.ts#L22)

### Methods

#### get

▸ **get**(`query`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

Convenience function for filtering links just like with PerspectiveProxy

##### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](#classesperspectives_linkquerylinkquerymd) |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Defined in

[perspectives/Perspective.ts:33](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/Perspective.ts#L33)

___

#### getSingleTarget

▸ **getSingleTarget**(`query`): `string` \| `void`

Convenience function to get the target of the first link that matches the given query
This makes sense when the query is expected to return only one link
and the target of that link is what you are looking for.

##### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](#classesperspectives_linkquerylinkquerymd) |

##### Returns

`string` \| `void`

##### Defined in

[perspectives/Perspective.ts:81](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/Perspective.ts#L81)


<a name="classesperspectives_perspectiveperspectiveexpressionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/Perspective](#modulesperspectives_perspectivemd) / PerspectiveExpression

## Class: PerspectiveExpression

[perspectives/Perspective](#modulesperspectives_perspectivemd).PerspectiveExpression

### Hierarchy

- `any`

  ↳ **`PerspectiveExpression`**

### Table of contents

#### Constructors

- [constructor](#constructor)

### Constructors

#### constructor

• **new PerspectiveExpression**()

##### Inherited from

ExpressionGeneric(Perspective).constructor


<a name="classesperspectives_perspectiveperspectiveinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/Perspective](#modulesperspectives_perspectivemd) / PerspectiveInput

## Class: PerspectiveInput

[perspectives/Perspective](#modulesperspectives_perspectivemd).PerspectiveInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [links](#links)

### Constructors

#### constructor

• **new PerspectiveInput**()

### Properties

#### links

• **links**: [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd)[]

##### Defined in

[perspectives/Perspective.ts:95](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/Perspective.ts#L95)


<a name="classesperspectives_perspectiveperspectiveunsignedinputmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/Perspective](#modulesperspectives_perspectivemd) / PerspectiveUnsignedInput

## Class: PerspectiveUnsignedInput

[perspectives/Perspective](#modulesperspectives_perspectivemd).PerspectiveUnsignedInput

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [links](#links)

#### Methods

- [fromLink](#fromlink)

### Constructors

#### constructor

• **new PerspectiveUnsignedInput**(`links?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `links?` | [`LinkInput`](#classeslinks_linkslinkinputmd)[] |

##### Defined in

[perspectives/Perspective.ts:103](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/Perspective.ts#L103)

### Properties

#### links

• **links**: [`LinkInput`](#classeslinks_linkslinkinputmd)[]

##### Defined in

[perspectives/Perspective.ts:101](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/Perspective.ts#L101)

### Methods

#### fromLink

▸ `Static` **fromLink**(`link`): [`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`Link`](#classeslinks_linkslinkmd) |

##### Returns

[`PerspectiveUnsignedInput`](#classesperspectives_perspectiveperspectiveunsignedinputmd)

##### Defined in

[perspectives/Perspective.ts:110](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/Perspective.ts#L110)


<a name="classesperspectives_perspectiveclientperspectiveclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/PerspectiveClient](#modulesperspectives_perspectiveclientmd) / PerspectiveClient

## Class: PerspectiveClient

[perspectives/PerspectiveClient](#modulesperspectives_perspectiveclientmd).PerspectiveClient

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#apolloClient](##apolloclient)
- [#expressionClient](##expressionclient)
- [#neighbourhoodClient](##neighbourhoodclient)
- [#perspectiveAddedCallbacks](##perspectiveaddedcallbacks)
- [#perspectiveRemovedCallbacks](##perspectiveremovedcallbacks)
- [#perspectiveSyncStateChangeCallbacks](##perspectivesyncstatechangecallbacks)
- [#perspectiveUpdatedCallbacks](##perspectiveupdatedcallbacks)

#### Methods

- [add](#add)
- [addLink](#addlink)
- [addLinkExpression](#addlinkexpression)
- [addLinks](#addlinks)
- [addPerspectiveAddedListener](#addperspectiveaddedlistener)
- [addPerspectiveLinkAddedListener](#addperspectivelinkaddedlistener)
- [addPerspectiveLinkRemovedListener](#addperspectivelinkremovedlistener)
- [addPerspectiveLinkUpdatedListener](#addperspectivelinkupdatedlistener)
- [addPerspectiveRemovedListener](#addperspectiveremovedlistener)
- [addPerspectiveSyncStateChangeListener](#addperspectivesyncstatechangelistener)
- [addPerspectiveSyncedListener](#addperspectivesyncedlistener)
- [addPerspectiveUpdatedListener](#addperspectiveupdatedlistener)
- [all](#all)
- [byUUID](#byuuid)
- [createExpression](#createexpression)
- [getExpression](#getexpression)
- [getNeighbourhoodProxy](#getneighbourhoodproxy)
- [linkMutations](#linkmutations)
- [publishSnapshotByUUID](#publishsnapshotbyuuid)
- [queryLinks](#querylinks)
- [queryProlog](#queryprolog)
- [remove](#remove)
- [removeLink](#removelink)
- [removeLinks](#removelinks)
- [setExpressionClient](#setexpressionclient)
- [setNeighbourhoodClient](#setneighbourhoodclient)
- [snapshotByUUID](#snapshotbyuuid)
- [subscribePerspectiveAdded](#subscribeperspectiveadded)
- [subscribePerspectiveRemoved](#subscribeperspectiveremoved)
- [subscribePerspectiveUpdated](#subscribeperspectiveupdated)
- [update](#update)
- [updateLink](#updatelink)

### Constructors

#### constructor

• **new PerspectiveClient**(`client`, `subscribe?`)

##### Parameters

| Name | Type | Default value |
| :------ | :------ | :------ |
| `client` | `ApolloClient`<`any`\> | `undefined` |
| `subscribe` | `boolean` | `true` |

##### Defined in

[perspectives/PerspectiveClient.ts:53](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L53)

### Properties

#### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

##### Defined in

[perspectives/PerspectiveClient.ts:45](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L45)

___

#### #expressionClient

• `Private` `Optional` **#expressionClient**: [`ExpressionClient`](#classesexpression_expressionclientexpressionclientmd)

##### Defined in

[perspectives/PerspectiveClient.ts:50](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L50)

___

#### #neighbourhoodClient

• `Private` `Optional` **#neighbourhoodClient**: [`NeighbourhoodClient`](#classesneighbourhood_neighbourhoodclientneighbourhoodclientmd)

##### Defined in

[perspectives/PerspectiveClient.ts:51](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L51)

___

#### #perspectiveAddedCallbacks

• `Private` **#perspectiveAddedCallbacks**: [`PerspectiveHandleCallback`](#perspectivehandlecallback)[]

##### Defined in

[perspectives/PerspectiveClient.ts:46](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L46)

___

#### #perspectiveRemovedCallbacks

• `Private` **#perspectiveRemovedCallbacks**: [`UuidCallback`](#uuidcallback)[]

##### Defined in

[perspectives/PerspectiveClient.ts:48](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L48)

___

#### #perspectiveSyncStateChangeCallbacks

• `Private` **#perspectiveSyncStateChangeCallbacks**: [`SyncStateChangeCallback`](#syncstatechangecallback)[]

##### Defined in

[perspectives/PerspectiveClient.ts:49](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L49)

___

#### #perspectiveUpdatedCallbacks

• `Private` **#perspectiveUpdatedCallbacks**: [`PerspectiveHandleCallback`](#perspectivehandlecallback)[]

##### Defined in

[perspectives/PerspectiveClient.ts:47](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L47)

### Methods

#### add

▸ **add**(`name`): `Promise`<[`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `name` | `string` |

##### Returns

`Promise`<[`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:144](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L144)

___

#### addLink

▸ **addLink**(`uuid`, `link`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`Link`](#classeslinks_linkslinkmd) |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:177](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L177)

___

#### addLinkExpression

▸ **addLinkExpression**(`uuid`, `link`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:230](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L230)

___

#### addLinks

▸ **addLinks**(`uuid`, `links`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `links` | [`Link`](#classeslinks_linkslinkmd)[] |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Defined in

[perspectives/PerspectiveClient.ts:189](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L189)

___

#### addPerspectiveAddedListener

▸ **addPerspectiveAddedListener**(`cb`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`PerspectiveHandleCallback`](#perspectivehandlecallback) |

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:287](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L287)

___

#### addPerspectiveLinkAddedListener

▸ **addPerspectiveLinkAddedListener**(`uuid`, `cb`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `String` |
| `cb` | [`LinkCallback`](#linkcallback)[] |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveClient.ts:365](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L365)

___

#### addPerspectiveLinkRemovedListener

▸ **addPerspectiveLinkRemovedListener**(`uuid`, `cb`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `String` |
| `cb` | [`LinkCallback`](#linkcallback)[] |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveClient.ts:382](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L382)

___

#### addPerspectiveLinkUpdatedListener

▸ **addPerspectiveLinkUpdatedListener**(`uuid`, `cb`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `String` |
| `cb` | [`LinkCallback`](#linkcallback)[] |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveClient.ts:399](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L399)

___

#### addPerspectiveRemovedListener

▸ **addPerspectiveRemovedListener**(`cb`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`UuidCallback`](#uuidcallback) |

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:346](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L346)

___

#### addPerspectiveSyncStateChangeListener

▸ **addPerspectiveSyncStateChangeListener**(`uuid`, `cb`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `String` |
| `cb` | [`SyncStateChangeCallback`](#syncstatechangecallback)[] |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveClient.ts:329](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L329)

___

#### addPerspectiveSyncedListener

▸ **addPerspectiveSyncedListener**(`cb`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`SyncStateChangeCallback`](#syncstatechangecallback) |

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:325](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L325)

___

#### addPerspectiveUpdatedListener

▸ **addPerspectiveUpdatedListener**(`cb`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`PerspectiveHandleCallback`](#perspectivehandlecallback) |

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:306](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L306)

___

#### all

▸ **all**(): `Promise`<[`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)[]\>

##### Returns

`Promise`<[`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)[]\>

##### Defined in

[perspectives/PerspectiveClient.ts:75](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L75)

___

#### byUUID

▸ **byUUID**(`uuid`): `Promise`<[`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

`Promise`<[`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:87](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L87)

___

#### createExpression

▸ **createExpression**(`content`, `languageAddress`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `any` |
| `languageAddress` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[perspectives/PerspectiveClient.ts:282](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L282)

___

#### getExpression

▸ **getExpression**(`expressionURI`): `Promise`<[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `expressionURI` | `string` |

##### Returns

`Promise`<[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:278](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L278)

___

#### getNeighbourhoodProxy

▸ **getNeighbourhoodProxy**(`uuid`): [`NeighbourhoodProxy`](#classesneighbourhood_neighbourhoodproxyneighbourhoodproxymd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

[`NeighbourhoodProxy`](#classesneighbourhood_neighbourhoodproxyneighbourhoodproxymd)

##### Defined in

[perspectives/PerspectiveClient.ts:423](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L423)

___

#### linkMutations

▸ **linkMutations**(`uuid`, `mutations`): `Promise`<[`LinkExpressionMutations`](#classeslinks_linkslinkexpressionmutationsmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `mutations` | [`LinkMutations`](#classeslinks_linkslinkmutationsmd) |

##### Returns

`Promise`<[`LinkExpressionMutations`](#classeslinks_linkslinkexpressionmutationsmd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:213](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L213)

___

#### publishSnapshotByUUID

▸ **publishSnapshotByUUID**(`uuid`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[perspectives/PerspectiveClient.ts:111](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L111)

___

#### queryLinks

▸ **queryLinks**(`uuid`, `query`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `query` | [`LinkQuery`](#classesperspectives_linkquerylinkquerymd) |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Defined in

[perspectives/PerspectiveClient.ts:121](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L121)

___

#### queryProlog

▸ **queryProlog**(`uuid`, `query`): `Promise`<`any`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `query` | `string` |

##### Returns

`Promise`<`any`\>

##### Defined in

[perspectives/PerspectiveClient.ts:133](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L133)

___

#### remove

▸ **remove**(`uuid`): `Promise`<{ `perspectiveRemove`: `boolean`  }\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

`Promise`<{ `perspectiveRemove`: `boolean`  }\>

##### Defined in

[perspectives/PerspectiveClient.ts:168](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L168)

___

#### removeLink

▸ **removeLink**(`uuid`, `link`): `Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |

##### Returns

`Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

##### Defined in

[perspectives/PerspectiveClient.ts:265](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L265)

___

#### removeLinks

▸ **removeLinks**(`uuid`, `links`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `links` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd)[] |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Defined in

[perspectives/PerspectiveClient.ts:201](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L201)

___

#### setExpressionClient

▸ **setExpressionClient**(`client`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `client` | [`ExpressionClient`](#classesexpression_expressionclientexpressionclientmd) |

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:67](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L67)

___

#### setNeighbourhoodClient

▸ **setNeighbourhoodClient**(`client`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `client` | [`NeighbourhoodClient`](#classesneighbourhood_neighbourhoodclientneighbourhoodclientmd) |

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:71](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L71)

___

#### snapshotByUUID

▸ **snapshotByUUID**(`uuid`): `Promise`<[`Perspective`](#classesperspectives_perspectiveperspectivemd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

`Promise`<[`Perspective`](#classesperspectives_perspectiveperspectivemd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:100](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L100)

___

#### subscribePerspectiveAdded

▸ **subscribePerspectiveAdded**(): `void`

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:291](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L291)

___

#### subscribePerspectiveRemoved

▸ **subscribePerspectiveRemoved**(): `void`

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:350](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L350)

___

#### subscribePerspectiveUpdated

▸ **subscribePerspectiveUpdated**(): `void`

##### Returns

`void`

##### Defined in

[perspectives/PerspectiveClient.ts:310](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L310)

___

#### update

▸ **update**(`uuid`, `name`): `Promise`<[`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `name` | `string` |

##### Returns

`Promise`<[`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:156](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L156)

___

#### updateLink

▸ **updateLink**(`uuid`, `oldLink`, `newLink`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `oldLink` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |
| `newLink` | [`LinkInput`](#classeslinks_linkslinkinputmd) |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Defined in

[perspectives/PerspectiveClient.ts:242](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L242)


<a name="classesperspectives_perspectivediffperspectivediffmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/PerspectiveDiff](#modulesperspectives_perspectivediffmd) / PerspectiveDiff

## Class: PerspectiveDiff

[perspectives/PerspectiveDiff](#modulesperspectives_perspectivediffmd).PerspectiveDiff

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [additions](#additions)
- [removals](#removals)

### Constructors

#### constructor

• **new PerspectiveDiff**()

### Properties

#### additions

• **additions**: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Defined in

[perspectives/PerspectiveDiff.ts:8](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveDiff.ts#L8)

___

#### removals

• **removals**: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Defined in

[perspectives/PerspectiveDiff.ts:11](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveDiff.ts#L11)


<a name="classesperspectives_perspectivediffperspectivediffexpressionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/PerspectiveDiff](#modulesperspectives_perspectivediffmd) / PerspectiveDiffExpression

## Class: PerspectiveDiffExpression

[perspectives/PerspectiveDiff](#modulesperspectives_perspectivediffmd).PerspectiveDiffExpression

### Hierarchy

- `any`

  ↳ **`PerspectiveDiffExpression`**

### Table of contents

#### Constructors

- [constructor](#constructor)

### Constructors

#### constructor

• **new PerspectiveDiffExpression**()

##### Inherited from

ExpressionGeneric(PerspectiveDiff).constructor


<a name="classesperspectives_perspectivehandleperspectivehandlemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/PerspectiveHandle](#modulesperspectives_perspectivehandlemd) / PerspectiveHandle

## Class: PerspectiveHandle

[perspectives/PerspectiveHandle](#modulesperspectives_perspectivehandlemd).PerspectiveHandle

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [name](#name)
- [neighbourhood](#neighbourhood)
- [sharedUrl](#sharedurl)
- [state](#state)
- [uuid](#uuid)

### Constructors

#### constructor

• **new PerspectiveHandle**(`uuid?`, `name?`, `state?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid?` | `string` |
| `name?` | `string` |
| `state?` | [`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd) |

##### Defined in

[perspectives/PerspectiveHandle.ts:30](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L30)

### Properties

#### name

• **name**: `string`

##### Defined in

[perspectives/PerspectiveHandle.ts:20](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L20)

___

#### neighbourhood

• `Optional` **neighbourhood**: [`Neighbourhood`](#classesneighbourhood_neighbourhoodneighbourhoodmd)

##### Defined in

[perspectives/PerspectiveHandle.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L28)

___

#### sharedUrl

• `Optional` **sharedUrl**: `string`

##### Defined in

[perspectives/PerspectiveHandle.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L25)

___

#### state

• **state**: [`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd)

##### Defined in

[perspectives/PerspectiveHandle.ts:22](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L22)

___

#### uuid

• **uuid**: `string`

##### Defined in

[perspectives/PerspectiveHandle.ts:18](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L18)


<a name="classesperspectives_perspectiveproxyperspectiveproxymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/PerspectiveProxy](#modulesperspectives_perspectiveproxymd) / PerspectiveProxy

## Class: PerspectiveProxy

[perspectives/PerspectiveProxy](#modulesperspectives_perspectiveproxymd).PerspectiveProxy

Perspective UI proxy object

Convenience object for UIs to interact with a perspective.
It is created by some of the methods in the PerspectiveClient class and includes
a reference to the PerspectiveClient object that created it.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#client](##client)
- [#handle](##handle)
- [#perspectiveLinkAddedCallbacks](##perspectivelinkaddedcallbacks)
- [#perspectiveLinkRemovedCallbacks](##perspectivelinkremovedcallbacks)
- [#perspectiveLinkUpdatedCallbacks](##perspectivelinkupdatedcallbacks)
- [#perspectiveSyncStateChangeCallbacks](##perspectivesyncstatechangecallbacks)

#### Accessors

- [name](#name)
- [neighbourhood](#neighbourhood)
- [sharedUrl](#sharedurl)
- [state](#state)
- [uuid](#uuid)

#### Methods

- [add](#add)
- [addLinkExpression](#addlinkexpression)
- [addLinks](#addlinks)
- [addListener](#addlistener)
- [addSdna](#addsdna)
- [addSyncStateChangeListener](#addsyncstatechangelistener)
- [availableFlows](#availableflows)
- [createExpression](#createexpression)
- [createSubject](#createsubject)
- [ensureSDNASubjectClass](#ensuresdnasubjectclass)
- [executeAction](#executeaction)
- [expressionsInFlowState](#expressionsinflowstate)
- [flowActions](#flowactions)
- [flowState](#flowstate)
- [get](#get)
- [getAllSubjectInstances](#getallsubjectinstances)
- [getAllSubjectProxies](#getallsubjectproxies)
- [getExpression](#getexpression)
- [getNeighbourhoodProxy](#getneighbourhoodproxy)
- [getSdna](#getsdna)
- [getSingleTarget](#getsingletarget)
- [getSubjectProxy](#getsubjectproxy)
- [infer](#infer)
- [isSubjectInstance](#issubjectinstance)
- [linkMutations](#linkmutations)
- [loadSnapshot](#loadsnapshot)
- [remove](#remove)
- [removeLinks](#removelinks)
- [removeListener](#removelistener)
- [runFlowAction](#runflowaction)
- [sdnaFlows](#sdnaflows)
- [setSdna](#setsdna)
- [setSingleTarget](#setsingletarget)
- [snapshot](#snapshot)
- [startFlow](#startflow)
- [stringOrTemplateObjectToSubjectClass](#stringortemplateobjecttosubjectclass)
- [subjectClasses](#subjectclasses)
- [subjectClassesByTemplate](#subjectclassesbytemplate)
- [update](#update)

### Constructors

#### constructor

• **new PerspectiveProxy**(`handle`, `ad4m`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `handle` | [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd) |
| `ad4m` | [`PerspectiveClient`](#classesperspectives_perspectiveclientperspectiveclientmd) |

##### Defined in

[perspectives/PerspectiveProxy.ts:34](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L34)

### Properties

#### #client

• `Private` **#client**: [`PerspectiveClient`](#classesperspectives_perspectiveclientperspectiveclientmd)

##### Defined in

[perspectives/PerspectiveProxy.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L28)

___

#### #handle

• `Private` **#handle**: [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Defined in

[perspectives/PerspectiveProxy.ts:27](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L27)

___

#### #perspectiveLinkAddedCallbacks

• `Private` **#perspectiveLinkAddedCallbacks**: [`LinkCallback`](#linkcallback)[]

##### Defined in

[perspectives/PerspectiveProxy.ts:29](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L29)

___

#### #perspectiveLinkRemovedCallbacks

• `Private` **#perspectiveLinkRemovedCallbacks**: [`LinkCallback`](#linkcallback)[]

##### Defined in

[perspectives/PerspectiveProxy.ts:30](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L30)

___

#### #perspectiveLinkUpdatedCallbacks

• `Private` **#perspectiveLinkUpdatedCallbacks**: [`LinkCallback`](#linkcallback)[]

##### Defined in

[perspectives/PerspectiveProxy.ts:31](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L31)

___

#### #perspectiveSyncStateChangeCallbacks

• `Private` **#perspectiveSyncStateChangeCallbacks**: [`SyncStateChangeCallback`](#syncstatechangecallback)[]

##### Defined in

[perspectives/PerspectiveProxy.ts:32](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L32)

### Accessors

#### name

• `get` **name**(): `string`

Given name of the perspective

##### Returns

`string`

##### Defined in

[perspectives/PerspectiveProxy.ts:98](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L98)

___

#### neighbourhood

• `get` **neighbourhood**(): `void` \| [`Neighbourhood`](#classesneighbourhood_neighbourhoodneighbourhoodmd)

If the perspective is shared as a Neighbourhood, this is the Neighbourhood Expression

##### Returns

`void` \| [`Neighbourhood`](#classesneighbourhood_neighbourhoodneighbourhoodmd)

##### Defined in

[perspectives/PerspectiveProxy.ts:108](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L108)

___

#### sharedUrl

• `get` **sharedUrl**(): `string` \| `void`

If the perspective is shared as a Neighbourhood, this is the Neighbourhood URL

##### Returns

`string` \| `void`

##### Defined in

[perspectives/PerspectiveProxy.ts:103](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L103)

___

#### state

• `get` **state**(): [`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd)

Returns the state of the perspective *

##### Returns

[`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd)

##### Defined in

[perspectives/PerspectiveProxy.ts:113](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L113)

___

#### uuid

• `get` **uuid**(): `string`

Unique ID of the perspective

##### Returns

`string`

##### Defined in

[perspectives/PerspectiveProxy.ts:93](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L93)

### Methods

#### add

▸ **add**(`link`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

Adds a link to this perspective

##### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`Link`](#classeslinks_linkslinkmd) |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Defined in

[perspectives/PerspectiveProxy.ts:128](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L128)

___

#### addLinkExpression

▸ **addLinkExpression**(`link`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

Adds a linkExpression to this perspective

##### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Defined in

[perspectives/PerspectiveProxy.ts:148](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L148)

___

#### addLinks

▸ **addLinks**(`links`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

Adds multiple links to this perspective *

##### Parameters

| Name | Type |
| :------ | :------ |
| `links` | [`Link`](#classeslinks_linkslinkmd)[] |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:133](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L133)

___

#### addListener

▸ **addListener**(`type`, `cb`): `Promise`<`void`\>

Adds a link listener

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `type` | `PerspectiveListenerTypes` | Can be 'link-added' or 'link-removed' |
| `cb` | [`LinkCallback`](#linkcallback) | Callback function that is called when a link is added to the perspective |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:172](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L172)

___

#### addSdna

▸ **addSdna**(`sdnaCode`): `Promise`<`void`\>

Adds the given Social DNA code to the perspective's SDNA code

##### Parameters

| Name | Type |
| :------ | :------ |
| `sdnaCode` | `string` |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:339](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L339)

___

#### addSyncStateChangeListener

▸ **addSyncStateChangeListener**(`cb`): `Promise`<`void`\>

Adds a sync state listener

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `cb` | [`SyncStateChangeCallback`](#syncstatechangecallback) | Callback function that is called when the sync state of the perspective changes |

##### Returns

`Promise`<`void`\>

A function that can be called to remove the listener

##### Defined in

[perspectives/PerspectiveProxy.ts:186](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L186)

___

#### availableFlows

▸ **availableFlows**(`exprAddr`): `Promise`<`string`[]\>

Returns all Social DNA flows that can be started from the given expression

##### Parameters

| Name | Type |
| :------ | :------ |
| `exprAddr` | `string` |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:276](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L276)

___

#### createExpression

▸ **createExpression**(`content`, `languageAddress`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `any` |
| `languageAddress` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:164](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L164)

___

#### createSubject

▸ **createSubject**<`T`\>(`subjectClass`, `exprAddr`): `Promise`<`T`\>

Creates a new subject instance by running its (SDNA defined) constructor,
which means adding links around the given expression address so that it
conforms to the given subject class.

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `subjectClass` | `T` | Either a string with the name of the subject class, or an object with the properties of the subject class. In the latter case, the first subject class that matches the given properties will be used. |
| `exprAddr` | `string` | The address of the expression to be turned into a subject instance |

##### Returns

`Promise`<`T`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:379](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L379)

___

#### ensureSDNASubjectClass

▸ **ensureSDNASubjectClass**(`jsClass`): `Promise`<`void`\>

Takes a JS class (its constructor) and assumes that it was decorated by
the

**`Subject Class`**

etc. decorators. It then tests if there is a subject class
already present in the perspective's SDNA that matches the given class.
If there is no such class, it gets the JS class's SDNA by calling its
static generateSDNA() function and adds it to the perspective's SDNA.

##### Parameters

| Name | Type |
| :------ | :------ |
| `jsClass` | `any` |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:533](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L533)

___

#### executeAction

▸ **executeAction**(`actions`, `expression`, `parameters`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `actions` | `any` |
| `expression` | `any` |
| `parameters` | `Parameter`[] |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:47](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L47)

___

#### expressionsInFlowState

▸ **expressionsInFlowState**(`flowName`, `flowState`): `Promise`<`string`[]\>

Returns all expressions in the given state of given Social DNA flow

##### Parameters

| Name | Type |
| :------ | :------ |
| `flowName` | `string` |
| `flowState` | `number` |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:290](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L290)

___

#### flowActions

▸ **flowActions**(`flowName`, `exprAddr`): `Promise`<`string`[]\>

Returns available action names, with regard to Social DNA flow and expression's flow state

##### Parameters

| Name | Type |
| :------ | :------ |
| `flowName` | `string` |
| `exprAddr` | `string` |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:302](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L302)

___

#### flowState

▸ **flowState**(`flowName`, `exprAddr`): `Promise`<`number`\>

Returns the given expression's flow state with regard to given Social DNA flow

##### Parameters

| Name | Type |
| :------ | :------ |
| `flowName` | `string` |
| `exprAddr` | `string` |

##### Returns

`Promise`<`number`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:296](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L296)

___

#### get

▸ **get**(`query`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

Returns all the links of this perspective that matches the LinkQuery

##### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](#classesperspectives_linkquerylinkquerymd) |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:118](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L118)

___

#### getAllSubjectInstances

▸ **getAllSubjectInstances**<`T`\>(`subjectClass`): `Promise`<`T`[]\>

Returns all subject instances of the given subject class as proxy objects.

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `subjectClass` | `T` | Either a string with the name of the subject class, or an object with the properties of the subject class. In the latter case, all subject classes that match the given properties will be used. |

##### Returns

`Promise`<`T`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:427](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L427)

___

#### getAllSubjectProxies

▸ **getAllSubjectProxies**<`T`\>(`subjectClass`): `Promise`<`T`[]\>

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type |
| :------ | :------ |
| `subjectClass` | `T` |

##### Returns

`Promise`<`T`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:444](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L444)

___

#### getExpression

▸ **getExpression**(`expressionURI`): `Promise`<[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `expressionURI` | `string` |

##### Returns

`Promise`<[`ExpressionRendered`](#classesexpression_expressionexpressionrenderedmd)\>

##### Defined in

[perspectives/PerspectiveProxy.ts:160](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L160)

___

#### getNeighbourhoodProxy

▸ **getNeighbourhoodProxy**(): [`NeighbourhoodProxy`](#classesneighbourhood_neighbourhoodproxyneighbourhoodproxymd)

##### Returns

[`NeighbourhoodProxy`](#classesneighbourhood_neighbourhoodproxyneighbourhoodproxymd)

##### Defined in

[perspectives/PerspectiveProxy.ts:541](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L541)

___

#### getSdna

▸ **getSdna**(): `Promise`<`string`[]\>

Returns the perspective's Social DNA code 
This will return all SDNA code elements in an array.

##### Returns

`Promise`<`string`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:329](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L329)

___

#### getSingleTarget

▸ **getSingleTarget**(`query`): `Promise`<`string` \| `void`\>

Convenience function to get the target of the first link that matches the given query
This makes sense when the query is expected to return only one link
and the target of that link is what you are looking for.

Works best together with

**`Member`**

setSingelTarget()

##### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](#classesperspectives_linkquerylinkquerymd) |

##### Returns

`Promise`<`string` \| `void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:238](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L238)

___

#### getSubjectProxy

▸ **getSubjectProxy**<`T`\>(`base`, `subjectClass`): `Promise`<`T`\>

For an existing subject instance (existing in the perspective's links)
this function returns a proxy object that can be used to access the subject's
properties and methods.

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `base` | `string` | URI of the subject's root expression |
| `subjectClass` | `T` | Either a string with the name of the subject class, or an object with the properties of the subject class. In the latter case, the first subject class that matches the given properties will be used. |

##### Returns

`Promise`<`T`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:412](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L412)

___

#### infer

▸ **infer**(`query`): `Promise`<`any`\>

Runs a Prolog query on the perspective's Prolog engine

##### Parameters

| Name | Type |
| :------ | :------ |
| `query` | `string` |

##### Returns

`Promise`<`any`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:123](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L123)

___

#### isSubjectInstance

▸ **isSubjectInstance**<`T`\>(`expression`, `subjectClass`): `Promise`<`boolean`\>

Checks if the given expression is a subject instance of the given subject class

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `expression` | `string` | The expression to be checked |
| `subjectClass` | `T` | Either a string with the name of the subject class, or an object with the properties of the subject class. In the latter case, the first subject class that matches the given properties will be used. |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:397](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L397)

___

#### linkMutations

▸ **linkMutations**(`mutations`): `Promise`<[`LinkExpressionMutations`](#classeslinks_linkslinkexpressionmutationsmd)\>

Adds and removes multiple links from this perspective *

##### Parameters

| Name | Type |
| :------ | :------ |
| `mutations` | [`LinkMutations`](#classeslinks_linkslinkmutationsmd) |

##### Returns

`Promise`<[`LinkExpressionMutations`](#classeslinks_linkslinkexpressionmutationsmd)\>

##### Defined in

[perspectives/PerspectiveProxy.ts:143](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L143)

___

#### loadSnapshot

▸ **loadSnapshot**(`snapshot`): `Promise`<`void`\>

Take and load all the links from the given snapshot

##### Parameters

| Name | Type |
| :------ | :------ |
| `snapshot` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:219](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L219)

___

#### remove

▸ **remove**(`link`): `Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |

##### Returns

`Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

##### Defined in

[perspectives/PerspectiveProxy.ts:156](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L156)

___

#### removeLinks

▸ **removeLinks**(`links`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

Removes multiple links from this perspective *

##### Parameters

| Name | Type |
| :------ | :------ |
| `links` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd)[] |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:138](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L138)

___

#### removeListener

▸ **removeListener**(`type`, `cb`): `Promise`<`void`\>

Removes a previously added link listener

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `type` | `PerspectiveListenerTypes` | Can be 'link-added' or 'link-removed' |
| `cb` | [`LinkCallback`](#linkcallback) | Callback function that is called when a link is added to the perspective |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:194](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L194)

___

#### runFlowAction

▸ **runFlowAction**(`flowName`, `exprAddr`, `actionName`): `Promise`<`void`\>

Runs given Social DNA flow action

##### Parameters

| Name | Type |
| :------ | :------ |
| `flowName` | `string` |
| `exprAddr` | `string` |
| `actionName` | `string` |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:308](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L308)

___

#### sdnaFlows

▸ **sdnaFlows**(): `Promise`<`string`[]\>

Returns all the Social DNA flows defined in this perspective

##### Returns

`Promise`<`string`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:270](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L270)

___

#### setSdna

▸ **setSdna**(`sdnaCode`): `Promise`<`void`\>

Set the perspective's Social DNA code to the given string. 
This will replace all previous SDNA code elements with the new one.

##### Parameters

| Name | Type |
| :------ | :------ |
| `sdnaCode` | `string` |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:318](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L318)

___

#### setSingleTarget

▸ **setSingleTarget**(`link`): `Promise`<`void`\>

Convenience function to ensure there is only one link with given source and predicate
This function will remove all links with the same source and predicate as the given link,
and then add the given link.
This ensures there is only one target for the given source and predicate.

Works best together with

**`Member`**

getSingleTarget()

##### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`Link`](#classeslinks_linkslinkmd) |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:254](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L254)

___

#### snapshot

▸ **snapshot**(): `Promise`<[`Perspective`](#classesperspectives_perspectiveperspectivemd)\>

Create and return a snapshot of this perspective
A snapshot is a rendered Perspectie object that contains all the links of the perspective.

##### Returns

`Promise`<[`Perspective`](#classesperspectives_perspectiveperspectivemd)\>

##### Defined in

[perspectives/PerspectiveProxy.ts:214](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L214)

___

#### startFlow

▸ **startFlow**(`flowName`, `exprAddr`): `Promise`<`void`\>

Starts the Social DNA flow

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `flowName` | `string` | on the expression |
| `exprAddr` | `string` |  |

##### Returns

`Promise`<`void`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:282](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L282)

___

#### stringOrTemplateObjectToSubjectClass

▸ **stringOrTemplateObjectToSubjectClass**<`T`\>(`subjectClass`): `Promise`<`string`\>

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type |
| :------ | :------ |
| `subjectClass` | `T` |

##### Returns

`Promise`<`string`\>

##### Defined in

[perspectives/PerspectiveProxy.ts:356](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L356)

___

#### subjectClasses

▸ **subjectClasses**(): `Promise`<`string`[]\>

Returns all the Subject classes defined in this perspectives SDNA

##### Returns

`Promise`<`string`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:348](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L348)

___

#### subjectClassesByTemplate

▸ **subjectClassesByTemplate**(`obj`): `Promise`<`string`[]\>

Returns all subject classes that match the given template object.
This function looks at the properties of the template object and
its setters and collections to create a Prolog query that finds
all subject classes that would be converted to a proxy object
with exactly the same properties and collections.

Since there could be multiple subject classes that match the given
criteria, this function returns a list of class names.

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `obj` | `object` | The template object |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[perspectives/PerspectiveProxy.ts:471](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L471)

___

#### update

▸ **update**(`oldLink`, `newLink`): `Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `oldLink` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |
| `newLink` | [`Link`](#classeslinks_linkslinkmd) |

##### Returns

`Promise`<[`LinkExpression`](#classeslinks_linkslinkexpressionmd)\>

##### Defined in

[perspectives/PerspectiveProxy.ts:152](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveProxy.ts#L152)


<a name="classesperspectives_perspectiveresolverdefaultmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/PerspectiveResolver](#modulesperspectives_perspectiveresolvermd) / default

## Class: default

[perspectives/PerspectiveResolver](#modulesperspectives_perspectiveresolvermd).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Methods

- [perspective](#perspective)
- [perspectiveAdd](#perspectiveadd)
- [perspectiveAddLink](#perspectiveaddlink)
- [perspectiveAddLinkExpression](#perspectiveaddlinkexpression)
- [perspectiveAddLinks](#perspectiveaddlinks)
- [perspectiveAdded](#perspectiveadded)
- [perspectiveLinkAdded](#perspectivelinkadded)
- [perspectiveLinkMutations](#perspectivelinkmutations)
- [perspectiveLinkRemoved](#perspectivelinkremoved)
- [perspectiveLinkUpdated](#perspectivelinkupdated)
- [perspectivePublishSnapshot](#perspectivepublishsnapshot)
- [perspectiveQueryLinks](#perspectivequerylinks)
- [perspectiveQueryProlog](#perspectivequeryprolog)
- [perspectiveRemove](#perspectiveremove)
- [perspectiveRemoveLink](#perspectiveremovelink)
- [perspectiveRemoveLinks](#perspectiveremovelinks)
- [perspectiveRemoved](#perspectiveremoved)
- [perspectiveSnapshot](#perspectivesnapshot)
- [perspectiveSyncStateChange](#perspectivesyncstatechange)
- [perspectiveUpdate](#perspectiveupdate)
- [perspectiveUpdateLink](#perspectiveupdatelink)
- [perspectiveUpdated](#perspectiveupdated)
- [perspectives](#perspectives)

### Constructors

#### constructor

• **new default**()

### Methods

#### perspective

▸ **perspective**(`uuid`): [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Defined in

[perspectives/PerspectiveResolver.ts:44](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L44)

___

#### perspectiveAdd

▸ **perspectiveAdd**(`name`, `pubSub`): [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `name` | `string` |
| `pubSub` | `any` |

##### Returns

[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Defined in

[perspectives/PerspectiveResolver.ts:69](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L69)

___

#### perspectiveAddLink

▸ **perspectiveAddLink**(`uuid`, `link`, `pubSub`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkInput`](#classeslinks_linkslinkinputmd) |
| `pubSub` | `any` |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Defined in

[perspectives/PerspectiveResolver.ts:90](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L90)

___

#### perspectiveAddLinkExpression

▸ **perspectiveAddLinkExpression**(`uuid`, `link`, `pubSub`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |
| `pubSub` | `any` |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Defined in

[perspectives/PerspectiveResolver.ts:148](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L148)

___

#### perspectiveAddLinks

▸ **perspectiveAddLinks**(`uuid`, `links`, `pubSub`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `links` | [`LinkInput`](#classeslinks_linkslinkinputmd)[] |
| `pubSub` | `any` |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Defined in

[perspectives/PerspectiveResolver.ts:103](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L103)

___

#### perspectiveAdded

▸ **perspectiveAdded**(): [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Returns

[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Defined in

[perspectives/PerspectiveResolver.ts:173](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L173)

___

#### perspectiveLinkAdded

▸ **perspectiveLinkAdded**(`uuid`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Defined in

[perspectives/PerspectiveResolver.ts:189](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L189)

___

#### perspectiveLinkMutations

▸ **perspectiveLinkMutations**(`uuid`, `mutations`, `pubSub`): [`LinkExpressionMutations`](#classeslinks_linkslinkexpressionmutationsmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `mutations` | [`LinkMutations`](#classeslinks_linkslinkmutationsmd) |
| `pubSub` | `any` |

##### Returns

[`LinkExpressionMutations`](#classeslinks_linkslinkexpressionmutationsmd)

##### Defined in

[perspectives/PerspectiveResolver.ts:141](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L141)

___

#### perspectiveLinkRemoved

▸ **perspectiveLinkRemoved**(`uuid`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Defined in

[perspectives/PerspectiveResolver.ts:194](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L194)

___

#### perspectiveLinkUpdated

▸ **perspectiveLinkUpdated**(`uuid`): [`LinkExpressionUpdated`](#classeslinks_linkslinkexpressionupdatedmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

[`LinkExpressionUpdated`](#classeslinks_linkslinkexpressionupdatedmd)

##### Defined in

[perspectives/PerspectiveResolver.ts:199](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L199)

___

#### perspectivePublishSnapshot

▸ **perspectivePublishSnapshot**(`uuid`): `String`

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

`String`

##### Defined in

[perspectives/PerspectiveResolver.ts:54](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L54)

___

#### perspectiveQueryLinks

▸ **perspectiveQueryLinks**(`uuid`, `query`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `query` | [`LinkQuery`](#classesperspectives_linkquerylinkquerymd) |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Defined in

[perspectives/PerspectiveResolver.ts:59](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L59)

___

#### perspectiveQueryProlog

▸ **perspectiveQueryProlog**(`uuid`, `query`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `query` | `String` |

##### Returns

`string`

##### Defined in

[perspectives/PerspectiveResolver.ts:64](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L64)

___

#### perspectiveRemove

▸ **perspectiveRemove**(`uuid`, `pubSub`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `pubSub` | `any` |

##### Returns

`boolean`

##### Defined in

[perspectives/PerspectiveResolver.ts:83](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L83)

___

#### perspectiveRemoveLink

▸ **perspectiveRemoveLink**(`uuid`, `link`, `pubSub`): `Boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |
| `pubSub` | `any` |

##### Returns

`Boolean`

##### Defined in

[perspectives/PerspectiveResolver.ts:167](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L167)

___

#### perspectiveRemoveLinks

▸ **perspectiveRemoveLinks**(`uuid`, `links`, `pubSub`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `links` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd)[] |
| `pubSub` | `any` |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)[]

##### Defined in

[perspectives/PerspectiveResolver.ts:122](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L122)

___

#### perspectiveRemoved

▸ **perspectiveRemoved**(): `string`

##### Returns

`string`

##### Defined in

[perspectives/PerspectiveResolver.ts:184](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L184)

___

#### perspectiveSnapshot

▸ **perspectiveSnapshot**(`uuid`): [`Perspective`](#classesperspectives_perspectiveperspectivemd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

[`Perspective`](#classesperspectives_perspectiveperspectivemd)

##### Defined in

[perspectives/PerspectiveResolver.ts:49](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L49)

___

#### perspectiveSyncStateChange

▸ **perspectiveSyncStateChange**(`uuid`): [`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

##### Returns

[`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd)

##### Defined in

[perspectives/PerspectiveResolver.ts:204](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L204)

___

#### perspectiveUpdate

▸ **perspectiveUpdate**(`uuid`, `name`, `pubSub`): [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `name` | `string` |
| `pubSub` | `any` |

##### Returns

[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Defined in

[perspectives/PerspectiveResolver.ts:76](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L76)

___

#### perspectiveUpdateLink

▸ **perspectiveUpdateLink**(`uuid`, `oldlink`, `newlink`, `pubSub`): [`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `oldlink` | [`LinkExpressionInput`](#classeslinks_linkslinkexpressioninputmd) |
| `newlink` | [`LinkInput`](#classeslinks_linkslinkinputmd) |
| `pubSub` | `any` |

##### Returns

[`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Defined in

[perspectives/PerspectiveResolver.ts:154](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L154)

___

#### perspectiveUpdated

▸ **perspectiveUpdated**(): [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Returns

[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)

##### Defined in

[perspectives/PerspectiveResolver.ts:179](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L179)

___

#### perspectives

▸ **perspectives**(): [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)[]

##### Returns

[`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)[]

##### Defined in

[perspectives/PerspectiveResolver.ts:30](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L30)


<a name="classesruntime_runtimeclientruntimeclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [runtime/RuntimeClient](#modulesruntime_runtimeclientmd) / RuntimeClient

## Class: RuntimeClient

[runtime/RuntimeClient](#modulesruntime_runtimeclientmd).RuntimeClient

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#apolloClient](##apolloclient)
- [#exceptionOccurredCallbacks](##exceptionoccurredcallbacks)
- [#messageReceivedCallbacks](##messagereceivedcallbacks)

#### Methods

- [addExceptionCallback](#addexceptioncallback)
- [addFriends](#addfriends)
- [addKnownLinkLanguageTemplates](#addknownlinklanguagetemplates)
- [addMessageCallback](#addmessagecallback)
- [addTrustedAgents](#addtrustedagents)
- [deleteTrustedAgents](#deletetrustedagents)
- [friendSendMessage](#friendsendmessage)
- [friendStatus](#friendstatus)
- [friends](#friends)
- [getTrustedAgents](#gettrustedagents)
- [hcAddAgentInfos](#hcaddagentinfos)
- [hcAgentInfos](#hcagentinfos)
- [info](#info)
- [knownLinkLanguageTemplates](#knownlinklanguagetemplates)
- [messageInbox](#messageinbox)
- [messageOutbox](#messageoutbox)
- [openLink](#openlink)
- [quit](#quit)
- [removeFriends](#removefriends)
- [removeKnownLinkLanguageTemplates](#removeknownlinklanguagetemplates)
- [setStatus](#setstatus)
- [subscribeExceptionOccurred](#subscribeexceptionoccurred)
- [subscribeMessageReceived](#subscribemessagereceived)
- [verifyStringSignedByDid](#verifystringsignedbydid)

### Constructors

#### constructor

• **new RuntimeClient**(`client`, `subscribe?`)

##### Parameters

| Name | Type | Default value |
| :------ | :------ | :------ |
| `client` | `ApolloClient`<`any`\> | `undefined` |
| `subscribe` | `boolean` | `true` |

##### Defined in

[runtime/RuntimeClient.ts:28](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L28)

### Properties

#### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

##### Defined in

[runtime/RuntimeClient.ts:24](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L24)

___

#### #exceptionOccurredCallbacks

• `Private` **#exceptionOccurredCallbacks**: [`ExceptionCallback`](#exceptioncallback)[]

##### Defined in

[runtime/RuntimeClient.ts:26](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L26)

___

#### #messageReceivedCallbacks

• `Private` **#messageReceivedCallbacks**: [`MessageCallback`](#messagecallback)[]

##### Defined in

[runtime/RuntimeClient.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L25)

### Methods

#### addExceptionCallback

▸ **addExceptionCallback**(`cb`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`ExceptionCallback`](#exceptioncallback) |

##### Returns

`void`

##### Defined in

[runtime/RuntimeClient.ts:260](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L260)

___

#### addFriends

▸ **addFriends**(`dids`): `Promise`<`string`[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `dids` | `string`[] |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:128](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L128)

___

#### addKnownLinkLanguageTemplates

▸ **addKnownLinkLanguageTemplates**(`addresses`): `Promise`<`string`[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `addresses` | `string`[] |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:99](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L99)

___

#### addMessageCallback

▸ **addMessageCallback**(`cb`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`MessageCallback`](#messagecallback) |

##### Returns

`void`

##### Defined in

[runtime/RuntimeClient.ts:241](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L241)

___

#### addTrustedAgents

▸ **addTrustedAgents**(`agents`): `Promise`<`string`[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `agents` | `string`[] |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:70](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L70)

___

#### deleteTrustedAgents

▸ **deleteTrustedAgents**(`agents`): `Promise`<`string`[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `agents` | `string`[] |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:80](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L80)

___

#### friendSendMessage

▸ **friendSendMessage**(`did`, `message`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `message` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[runtime/RuntimeClient.ts:206](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L206)

___

#### friendStatus

▸ **friendStatus**(`did`): `Promise`<[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |

##### Returns

`Promise`<[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)\>

##### Defined in

[runtime/RuntimeClient.ts:196](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L196)

___

#### friends

▸ **friends**(): `Promise`<`string`[]\>

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:148](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L148)

___

#### getTrustedAgents

▸ **getTrustedAgents**(): `Promise`<`string`[]\>

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:90](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L90)

___

#### hcAddAgentInfos

▸ **hcAddAgentInfos**(`agentInfos`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `agentInfos` | `String` |

##### Returns

`Promise`<`void`\>

##### Defined in

[runtime/RuntimeClient.ts:166](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L166)

___

#### hcAgentInfos

▸ **hcAgentInfos**(): `Promise`<`String`\>

##### Returns

`Promise`<`String`\>

##### Defined in

[runtime/RuntimeClient.ts:157](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L157)

___

#### info

▸ **info**(): `Promise`<[`RuntimeInfo`](#classesruntime_runtimeresolverruntimeinfomd)\>

##### Returns

`Promise`<[`RuntimeInfo`](#classesruntime_runtimeresolverruntimeinfomd)\>

##### Defined in

[runtime/RuntimeClient.ts:39](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L39)

___

#### knownLinkLanguageTemplates

▸ **knownLinkLanguageTemplates**(): `Promise`<`string`[]\>

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:119](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L119)

___

#### messageInbox

▸ **messageInbox**(`filter?`): `Promise`<[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

##### Returns

`Promise`<[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)[]\>

##### Defined in

[runtime/RuntimeClient.ts:216](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L216)

___

#### messageOutbox

▸ **messageOutbox**(`filter?`): `Promise`<[`SentMessage`](#classesruntime_runtimeresolversentmessagemd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

##### Returns

`Promise`<[`SentMessage`](#classesruntime_runtimeresolversentmessagemd)[]\>

##### Defined in

[runtime/RuntimeClient.ts:226](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L226)

___

#### openLink

▸ **openLink**(`url`): `Promise`<`Boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

`Promise`<`Boolean`\>

##### Defined in

[runtime/RuntimeClient.ts:60](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L60)

___

#### quit

▸ **quit**(): `Promise`<`Boolean`\>

##### Returns

`Promise`<`Boolean`\>

##### Defined in

[runtime/RuntimeClient.ts:52](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L52)

___

#### removeFriends

▸ **removeFriends**(`dids`): `Promise`<`string`[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `dids` | `string`[] |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:138](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L138)

___

#### removeKnownLinkLanguageTemplates

▸ **removeKnownLinkLanguageTemplates**(`addresses`): `Promise`<`string`[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `addresses` | `string`[] |

##### Returns

`Promise`<`string`[]\>

##### Defined in

[runtime/RuntimeClient.ts:109](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L109)

___

#### setStatus

▸ **setStatus**(`perspective`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[runtime/RuntimeClient.ts:186](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L186)

___

#### subscribeExceptionOccurred

▸ **subscribeExceptionOccurred**(): `void`

##### Returns

`void`

##### Defined in

[runtime/RuntimeClient.ts:264](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L264)

___

#### subscribeMessageReceived

▸ **subscribeMessageReceived**(): `void`

##### Returns

`void`

##### Defined in

[runtime/RuntimeClient.ts:245](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L245)

___

#### verifyStringSignedByDid

▸ **verifyStringSignedByDid**(`did`, `didSigningKeyId`, `data`, `signedData`): `Promise`<`boolean`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `didSigningKeyId` | `string` |
| `data` | `string` |
| `signedData` | `string` |

##### Returns

`Promise`<`boolean`\>

##### Defined in

[runtime/RuntimeClient.ts:176](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L176)


<a name="classesruntime_runtimeresolverexceptioninfomd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd) / ExceptionInfo

## Class: ExceptionInfo

[runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd).ExceptionInfo

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [addon](#addon)
- [message](#message)
- [title](#title)
- [type](#type)

### Constructors

#### constructor

• **new ExceptionInfo**()

### Properties

#### addon

• `Optional` **addon**: `string`

##### Defined in

[runtime/RuntimeResolver.ts:54](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L54)

___

#### message

• **message**: `string`

##### Defined in

[runtime/RuntimeResolver.ts:50](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L50)

___

#### title

• **title**: `string`

##### Defined in

[runtime/RuntimeResolver.ts:48](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L48)

___

#### type

• **type**: [`ExceptionType`](#enumsexceptionexceptiontypemd)

##### Defined in

[runtime/RuntimeResolver.ts:52](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L52)


<a name="classesruntime_runtimeresolverruntimeinfomd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd) / RuntimeInfo

## Class: RuntimeInfo

[runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd).RuntimeInfo

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [ad4mExecutorVersion](#ad4mexecutorversion)
- [isInitialized](#isinitialized)
- [isUnlocked](#isunlocked)

### Constructors

#### constructor

• **new RuntimeInfo**()

### Properties

#### ad4mExecutorVersion

• **ad4mExecutorVersion**: `string`

##### Defined in

[runtime/RuntimeResolver.ts:38](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L38)

___

#### isInitialized

• **isInitialized**: `Boolean`

##### Defined in

[runtime/RuntimeResolver.ts:40](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L40)

___

#### isUnlocked

• **isUnlocked**: `Boolean`

##### Defined in

[runtime/RuntimeResolver.ts:42](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L42)


<a name="classesruntime_runtimeresolversentmessagemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd) / SentMessage

## Class: SentMessage

[runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd).SentMessage

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [message](#message)
- [recipient](#recipient)

### Constructors

#### constructor

• **new SentMessage**()

### Properties

#### message

• **message**: [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)

##### Defined in

[runtime/RuntimeResolver.ts:32](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L32)

___

#### recipient

• **recipient**: `string`

##### Defined in

[runtime/RuntimeResolver.ts:30](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L30)


<a name="classesruntime_runtimeresolverdefaultmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd) / default

## Class: default

[runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Methods

- [addTrustedAgents](#addtrustedagents)
- [deleteTrustedAgents](#deletetrustedagents)
- [exceptionOccurred](#exceptionoccurred)
- [getTrustedAgents](#gettrustedagents)
- [runtimeAddFriends](#runtimeaddfriends)
- [runtimeAddKnownLinkLanguageTemplates](#runtimeaddknownlinklanguagetemplates)
- [runtimeFriendSendMessage](#runtimefriendsendmessage)
- [runtimeFriendStatus](#runtimefriendstatus)
- [runtimeFriends](#runtimefriends)
- [runtimeHcAddAgentInfos](#runtimehcaddagentinfos)
- [runtimeHcAgentInfos](#runtimehcagentinfos)
- [runtimeInfo](#runtimeinfo)
- [runtimeKnownLinkLanguageTemplates](#runtimeknownlinklanguagetemplates)
- [runtimeMessageInbox](#runtimemessageinbox)
- [runtimeMessageOutbox](#runtimemessageoutbox)
- [runtimeMessageReceived](#runtimemessagereceived)
- [runtimeOpenLink](#runtimeopenlink)
- [runtimeQuit](#runtimequit)
- [runtimeRemoveFriends](#runtimeremovefriends)
- [runtimeRemoveKnownLinkLanguageTemplates](#runtimeremoveknownlinklanguagetemplates)
- [runtimeSetStatus](#runtimesetstatus)
- [runtimeVerifyStringSignedByDid](#runtimeverifystringsignedbydid)

### Constructors

#### constructor

• **new default**()

### Methods

#### addTrustedAgents

▸ **addTrustedAgents**(`agents`): `string`[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `agents` | `string`[] |

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:84](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L84)

___

#### deleteTrustedAgents

▸ **deleteTrustedAgents**(`agents`): `string`[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `agents` | `string`[] |

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:89](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L89)

___

#### exceptionOccurred

▸ **exceptionOccurred**(): [`ExceptionInfo`](#classesruntime_runtimeresolverexceptioninfomd)

##### Returns

[`ExceptionInfo`](#classesruntime_runtimeresolverexceptioninfomd)

##### Defined in

[runtime/RuntimeResolver.ts:185](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L185)

___

#### getTrustedAgents

▸ **getTrustedAgents**(): `string`[]

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:94](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L94)

___

#### runtimeAddFriends

▸ **runtimeAddFriends**(`dids`): `string`[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `dids` | `string`[] |

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:119](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L119)

___

#### runtimeAddKnownLinkLanguageTemplates

▸ **runtimeAddKnownLinkLanguageTemplates**(`addresses`): `string`[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `addresses` | `string`[] |

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:104](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L104)

___

#### runtimeFriendSendMessage

▸ **runtimeFriendSendMessage**(`did`, `message`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `message` | [`PerspectiveInput`](#classesperspectives_perspectiveperspectiveinputmd) |

##### Returns

`boolean`

##### Defined in

[runtime/RuntimeResolver.ts:158](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L158)

___

#### runtimeFriendStatus

▸ **runtimeFriendStatus**(`did`): [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |

##### Returns

[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)

##### Defined in

[runtime/RuntimeResolver.ts:153](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L153)

___

#### runtimeFriends

▸ **runtimeFriends**(): `string`[]

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:114](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L114)

___

#### runtimeHcAddAgentInfos

▸ **runtimeHcAddAgentInfos**(`agentInfos`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `agentInfos` | `any` |

##### Returns

`boolean`

##### Defined in

[runtime/RuntimeResolver.ts:134](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L134)

___

#### runtimeHcAgentInfos

▸ **runtimeHcAgentInfos**(): `String`

##### Returns

`String`

##### Defined in

[runtime/RuntimeResolver.ts:129](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L129)

___

#### runtimeInfo

▸ **runtimeInfo**(): [`RuntimeInfo`](#classesruntime_runtimeresolverruntimeinfomd)

##### Returns

[`RuntimeInfo`](#classesruntime_runtimeresolverruntimeinfomd)

##### Defined in

[runtime/RuntimeResolver.ts:75](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L75)

___

#### runtimeKnownLinkLanguageTemplates

▸ **runtimeKnownLinkLanguageTemplates**(): `string`[]

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:99](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L99)

___

#### runtimeMessageInbox

▸ **runtimeMessageInbox**(`filter?`): [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

##### Returns

[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)[]

##### Defined in

[runtime/RuntimeResolver.ts:166](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L166)

___

#### runtimeMessageOutbox

▸ **runtimeMessageOutbox**(`filter?`): [`SentMessage`](#classesruntime_runtimeresolversentmessagemd)[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

##### Returns

[`SentMessage`](#classesruntime_runtimeresolversentmessagemd)[]

##### Defined in

[runtime/RuntimeResolver.ts:171](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L171)

___

#### runtimeMessageReceived

▸ **runtimeMessageReceived**(): [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)

##### Returns

[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)

##### Defined in

[runtime/RuntimeResolver.ts:180](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L180)

___

#### runtimeOpenLink

▸ **runtimeOpenLink**(`url`): `Boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

`Boolean`

##### Defined in

[runtime/RuntimeResolver.ts:70](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L70)

___

#### runtimeQuit

▸ **runtimeQuit**(): `Boolean`

##### Returns

`Boolean`

##### Defined in

[runtime/RuntimeResolver.ts:65](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L65)

___

#### runtimeRemoveFriends

▸ **runtimeRemoveFriends**(`dids`): `string`[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `dids` | `string`[] |

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:124](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L124)

___

#### runtimeRemoveKnownLinkLanguageTemplates

▸ **runtimeRemoveKnownLinkLanguageTemplates**(`addresses`): `string`[]

##### Parameters

| Name | Type |
| :------ | :------ |
| `addresses` | `string`[] |

##### Returns

`string`[]

##### Defined in

[runtime/RuntimeResolver.ts:109](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L109)

___

#### runtimeSetStatus

▸ **runtimeSetStatus**(`status`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`boolean`

##### Defined in

[runtime/RuntimeResolver.ts:148](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L148)

___

#### runtimeVerifyStringSignedByDid

▸ **runtimeVerifyStringSignedByDid**(`did`, `didSigningKeyId`, `data`, `signedData`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `didSigningKeyId` | `string` |
| `data` | `string` |
| `signedData` | `string` |

##### Returns

`boolean`

##### Defined in

[runtime/RuntimeResolver.ts:139](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeResolver.ts#L139)


<a name="classessubject_sdnadecoratorsperspectiveactionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [subject/SDNADecorators](#modulessubject_sdnadecoratorsmd) / PerspectiveAction

## Class: PerspectiveAction

[subject/SDNADecorators](#modulessubject_sdnadecoratorsmd).PerspectiveAction

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [action](#action)
- [predicate](#predicate)
- [source](#source)
- [target](#target)

### Constructors

#### constructor

• **new PerspectiveAction**()

### Properties

#### action

• **action**: `string`

##### Defined in

[subject/SDNADecorators.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L6)

___

#### predicate

• **predicate**: `string`

##### Defined in

[subject/SDNADecorators.ts:8](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L8)

___

#### source

• **source**: `string`

##### Defined in

[subject/SDNADecorators.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L7)

___

#### target

• **target**: `string`

##### Defined in

[subject/SDNADecorators.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L9)


<a name="classessubject_subjectsubjectmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [subject/Subject](#modulessubject_subjectmd) / Subject

## Class: Subject

[subject/Subject](#modulessubject_subjectmd).Subject

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#baseExpression](##baseexpression)
- [#perspective](##perspective)
- [#subjectClass](##subjectclass)

#### Accessors

- [baseExpression](#baseexpression)

#### Methods

- [init](#init)

### Constructors

#### constructor

• **new Subject**(`perspective`, `baseExpression`, `subjectClass`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd) |
| `baseExpression` | `string` |
| `subjectClass` | `string` |

##### Defined in

[subject/Subject.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/Subject.ts#L9)

### Properties

#### #baseExpression

• `Private` **#baseExpression**: `string`

##### Defined in

[subject/Subject.ts:5](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/Subject.ts#L5)

___

#### #perspective

• `Private` **#perspective**: [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)

##### Defined in

[subject/Subject.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/Subject.ts#L7)

___

#### #subjectClass

• `Private` **#subjectClass**: `string`

##### Defined in

[subject/Subject.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/Subject.ts#L6)

### Accessors

#### baseExpression

• `get` **baseExpression**(): `string`

##### Returns

`string`

##### Defined in

[subject/Subject.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/Subject.ts#L15)

### Methods

#### init

▸ **init**(): `Promise`<`void`\>

##### Returns

`Promise`<`void`\>

##### Defined in

[subject/Subject.ts:19](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/Subject.ts#L19)


<a name="classessubject_subjectentitysubjectentitymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [subject/SubjectEntity](#modulessubject_subjectentitymd) / SubjectEntity

## Class: SubjectEntity

[subject/SubjectEntity](#modulessubject_subjectentitymd).SubjectEntity

### Table of contents

#### Constructors

- [constructor](#constructor)

#### Properties

- [#baseExpression](##baseexpression)
- [#perspective](##perspective)
- [#subjectClass](##subjectclass)
- [author](#author)
- [timestamp](#timestamp)

#### Accessors

- [baseExpression](#baseexpression)

#### Methods

- [get](#get)
- [getData](#getdata)
- [save](#save)
- [setCollectionAdder](#setcollectionadder)
- [setCollectionSetter](#setcollectionsetter)
- [setProperty](#setproperty)
- [update](#update)
- [all](#all)

### Constructors

#### constructor

• **new SubjectEntity**(`perspective`, `baseExpression?`)

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd) |
| `baseExpression?` | `string` |

##### Defined in

[subject/SubjectEntity.ts:27](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L27)

### Properties

#### #baseExpression

• `Private` **#baseExpression**: `string`

##### Defined in

[subject/SubjectEntity.ts:21](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L21)

___

#### #perspective

• `Private` **#perspective**: [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd)

##### Defined in

[subject/SubjectEntity.ts:23](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L23)

___

#### #subjectClass

• `Private` **#subjectClass**: `string`

##### Defined in

[subject/SubjectEntity.ts:22](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L22)

___

#### author

• **author**: `string`

##### Defined in

[subject/SubjectEntity.ts:24](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L24)

___

#### timestamp

• **timestamp**: `string`

##### Defined in

[subject/SubjectEntity.ts:25](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L25)

### Accessors

#### baseExpression

• `get` **baseExpression**(): `string`

##### Returns

`string`

##### Defined in

[subject/SubjectEntity.ts:32](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L32)

### Methods

#### get

▸ **get**(): `Promise`<[`SubjectEntity`](#classessubject_subjectentitysubjectentitymd)\>

##### Returns

`Promise`<[`SubjectEntity`](#classessubject_subjectentitysubjectentitymd)\>

##### Defined in

[subject/SubjectEntity.ts:182](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L182)

___

#### getData

▸ `Private` **getData**(`id?`): `Promise`<[`SubjectEntity`](#classessubject_subjectentitysubjectentitymd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `id?` | `string` |

##### Returns

`Promise`<[`SubjectEntity`](#classessubject_subjectentitysubjectentitymd)\>

##### Defined in

[subject/SubjectEntity.ts:36](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L36)

___

#### save

▸ **save**(): `Promise`<`void`\>

##### Returns

`Promise`<`void`\>

##### Defined in

[subject/SubjectEntity.ts:146](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L146)

___

#### setCollectionAdder

▸ `Private` **setCollectionAdder**(`key`, `value`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `key` | `string` |
| `value` | `any` |

##### Returns

`Promise`<`void`\>

##### Defined in

[subject/SubjectEntity.ts:132](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L132)

___

#### setCollectionSetter

▸ `Private` **setCollectionSetter**(`key`, `value`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `key` | `string` |
| `value` | `any` |

##### Returns

`Promise`<`void`\>

##### Defined in

[subject/SubjectEntity.ts:117](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L117)

___

#### setProperty

▸ `Private` **setProperty**(`key`, `value`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `key` | `string` |
| `value` | `any` |

##### Returns

`Promise`<`void`\>

##### Defined in

[subject/SubjectEntity.ts:99](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L99)

___

#### update

▸ **update**(): `Promise`<`void`\>

##### Returns

`Promise`<`void`\>

##### Defined in

[subject/SubjectEntity.ts:154](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L154)

___

#### all

▸ `Static` **all**(`perspective`): `Promise`<`any`[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](#classesperspectives_perspectiveproxyperspectiveproxymd) |

##### Returns

`Promise`<`any`[]\>

##### Defined in

[subject/SubjectEntity.ts:189](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L189)

# Enums


<a name="enumsexceptionexceptiontypemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [Exception](#modulesexceptionmd) / ExceptionType

## Enumeration: ExceptionType

[Exception](#modulesexceptionmd).ExceptionType

### Table of contents

#### Enumeration Members

- [AgentIsUntrusted](#agentisuntrusted)
- [CapabilityRequested](#capabilityrequested)
- [ExpressionIsNotVerified](#expressionisnotverified)
- [LanguageIsNotLoaded](#languageisnotloaded)

### Enumeration Members

#### AgentIsUntrusted

• **AgentIsUntrusted** = ``2``

##### Defined in

[Exception.ts:4](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Exception.ts#L4)

___

#### CapabilityRequested

• **CapabilityRequested** = ``3``

##### Defined in

[Exception.ts:5](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Exception.ts#L5)

___

#### ExpressionIsNotVerified

• **ExpressionIsNotVerified** = ``1``

##### Defined in

[Exception.ts:3](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Exception.ts#L3)

___

#### LanguageIsNotLoaded

• **LanguageIsNotLoaded** = ``0``

##### Defined in

[Exception.ts:2](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Exception.ts#L2)


<a name="enumsperspectives_perspectivehandleperspectivestatemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [perspectives/PerspectiveHandle](#modulesperspectives_perspectivehandlemd) / PerspectiveState

## Enumeration: PerspectiveState

[perspectives/PerspectiveHandle](#modulesperspectives_perspectivehandlemd).PerspectiveState

### Table of contents

#### Enumeration Members

- [LinkLanguageFailedToInstall](#linklanguagefailedtoinstall)
- [LinkLanguageInstalledButNotSynced](#linklanguageinstalledbutnotsynced)
- [NeighbourhoodJoinInitiated](#neighbourhoodjoininitiated)
- [Private](#private)
- [Synced](#synced)

### Enumeration Members

#### LinkLanguageFailedToInstall

• **LinkLanguageFailedToInstall** = ``"LinkLanguageFailedToInstall"``

##### Defined in

[perspectives/PerspectiveHandle.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L7)

___

#### LinkLanguageInstalledButNotSynced

• **LinkLanguageInstalledButNotSynced** = ``"LinkLanguageInstalledButNotSynced"``

##### Defined in

[perspectives/PerspectiveHandle.ts:8](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L8)

___

#### NeighbourhoodJoinInitiated

• **NeighbourhoodJoinInitiated** = ``"NeighbourhoodJoinInitiated"``

##### Defined in

[perspectives/PerspectiveHandle.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L6)

___

#### Private

• **Private** = ``"Private"``

##### Defined in

[perspectives/PerspectiveHandle.ts:5](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L5)

___

#### Synced

• **Synced** = ``"Synced"``

##### Defined in

[perspectives/PerspectiveHandle.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveHandle.ts#L9)

# Interfaces


<a name="interfacesagent_agentclientinitializeargsmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [agent/AgentClient](#modulesagent_agentclientmd) / InitializeArgs

## Interface: InitializeArgs

[agent/AgentClient](#modulesagent_agentclientmd).InitializeArgs

### Table of contents

#### Properties

- [did](#did)
- [didDocument](#diddocument)
- [keystore](#keystore)
- [passphrase](#passphrase)

### Properties

#### did

• **did**: `string`

##### Defined in

[agent/AgentClient.ts:73](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L73)

___

#### didDocument

• **didDocument**: `string`

##### Defined in

[agent/AgentClient.ts:74](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L74)

___

#### keystore

• **keystore**: `string`

##### Defined in

[agent/AgentClient.ts:75](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L75)

___

#### passphrase

• **passphrase**: `string`

##### Defined in

[agent/AgentClient.ts:76](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L76)


<a name="interfaceslanguage_languagedirectmessageadaptermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / DirectMessageAdapter

## Interface: DirectMessageAdapter

[language/Language](#moduleslanguage_languagemd).DirectMessageAdapter

### Table of contents

#### Methods

- [addMessageCallback](#addmessagecallback)
- [inbox](#inbox)
- [recipient](#recipient)
- [sendInbox](#sendinbox)
- [sendP2P](#sendp2p)
- [setStatus](#setstatus)
- [status](#status)

### Methods

#### addMessageCallback

▸ **addMessageCallback**(`callback`): `any`

##### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`MessageCallback`](#messagecallback) |

##### Returns

`any`

##### Defined in

[language/Language.ts:203](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L203)

___

#### inbox

▸ **inbox**(`filter?`): `Promise`<[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

##### Returns

`Promise`<[`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)[]\>

##### Defined in

[language/Language.ts:202](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L202)

___

#### recipient

▸ **recipient**(): `string`

##### Returns

`string`

##### Defined in

[language/Language.ts:195](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L195)

___

#### sendInbox

▸ **sendInbox**(`message`): `Promise`<`void` \| [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`void` \| [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)\>

##### Defined in

[language/Language.ts:199](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L199)

___

#### sendP2P

▸ **sendP2P**(`message`): `Promise`<`void` \| [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`Perspective`](#classesperspectives_perspectiveperspectivemd) |

##### Returns

`Promise`<`void` \| [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)\>

##### Defined in

[language/Language.ts:198](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L198)

___

#### setStatus

▸ **setStatus**(`status`): `any`

##### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd) |

##### Returns

`any`

##### Defined in

[language/Language.ts:201](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L201)

___

#### status

▸ **status**(): `Promise`<`void` \| [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)\>

##### Returns

`Promise`<`void` \| [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)\>

##### Defined in

[language/Language.ts:197](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L197)


<a name="interfaceslanguage_languageexpressionadaptermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / ExpressionAdapter

## Interface: ExpressionAdapter

[language/Language](#moduleslanguage_languagemd).ExpressionAdapter

Interface for the most common Expression Languages

### Table of contents

#### Properties

- [putAdapter](#putadapter)

#### Methods

- [get](#get)

### Properties

#### putAdapter

• **putAdapter**: [`PublicSharing`](#interfaceslanguage_languagepublicsharingmd) \| [`ReadOnlyLanguage`](#interfaceslanguage_languagereadonlylanguagemd)

Strategy for putting an expression with needs to be different
for those two cases:
1. PublicSharing means that this language supports the creation
   and sharing of Expressions, which is the common use-case
2. ReadOnlyLanguage means that the Language implements a pre-defined
   set of expressions (which can be infinite or finite).
   For example the url-iframe Language which directly maps URLs to
   addresses - meaning every well formed URL is an address in this
   Language. Or a potential Language implementing the verbs/predicates
   of a spec like FOAF.

##### Defined in

[language/Language.ts:106](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L106)

### Methods

#### get

▸ **get**(`address`): `Promise`<[`Expression`](#classesexpression_expressionexpressionmd)\>

Returns an Expression by address, or null if there is no Expression
with that given address

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

`Promise`<[`Expression`](#classesexpression_expressionexpressionmd)\>

##### Defined in

[language/Language.ts:93](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L93)


<a name="interfaceslanguage_languageexpressionuimd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / ExpressionUI

## Interface: ExpressionUI

[language/Language](#moduleslanguage_languagemd).ExpressionUI

UI factories returning web components

### Table of contents

#### Methods

- [constructorIcon](#constructoricon)
- [icon](#icon)

### Methods

#### constructorIcon

▸ **constructorIcon**(): `string`

Returns JS code of a web component used to create new expressions

##### Returns

`string`

##### Defined in

[language/Language.ts:82](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L82)

___

#### icon

▸ **icon**(): `string`

Returns JS code of a web component that renders the given expression

##### Returns

`string`

##### Defined in

[language/Language.ts:80](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L80)


<a name="interfaceslanguage_languagegetalladaptermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / GetAllAdapter

## Interface: GetAllAdapter

[language/Language](#moduleslanguage_languagemd).GetAllAdapter

### Table of contents

#### Methods

- [getAll](#getall)

### Methods

#### getAll

▸ **getAll**(`filter`, `count`, `page`): `Promise`<[`Expression`](#classesexpression_expressionexpressionmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `filter` | `any` |
| `count` | `number` |
| `page` | `number` |

##### Returns

`Promise`<[`Expression`](#classesexpression_expressionexpressionmd)[]\>

##### Defined in

[language/Language.ts:148](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L148)


<a name="interfaceslanguage_languagegetbyauthoradaptermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / GetByAuthorAdapter

## Interface: GetByAuthorAdapter

[language/Language](#moduleslanguage_languagemd).GetByAuthorAdapter

### Table of contents

#### Methods

- [getByAuthor](#getbyauthor)

### Methods

#### getByAuthor

▸ **getByAuthor**(`author`, `count`, `page`): `Promise`<[`Expression`](#classesexpression_expressionexpressionmd)[]\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `author` | `string` |
| `count` | `number` |
| `page` | `number` |

##### Returns

`Promise`<[`Expression`](#classesexpression_expressionexpressionmd)[]\>

##### Defined in

[language/Language.ts:139](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L139)


<a name="interfaceslanguage_languageinteractionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / Interaction

## Interface: Interaction

[language/Language](#moduleslanguage_languagemd).Interaction

### Table of contents

#### Properties

- [label](#label)
- [name](#name)
- [parameters](#parameters)

#### Methods

- [execute](#execute)

### Properties

#### label

• `Readonly` **label**: `string`

##### Defined in

[language/Language.ts:227](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L227)

___

#### name

• `Readonly` **name**: `string`

##### Defined in

[language/Language.ts:228](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L228)

___

#### parameters

• `Readonly` **parameters**: [`InteractionParameter`](#classeslanguage_languageinteractionparametermd)[]

##### Defined in

[language/Language.ts:229](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L229)

### Methods

#### execute

▸ **execute**(`parameters`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `parameters` | `object` |

##### Returns

`Promise`<`string`\>

##### Defined in

[language/Language.ts:230](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L230)


<a name="interfaceslanguage_languagelanguagemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / Language

## Interface: Language

[language/Language](#moduleslanguage_languagemd).Language

Interface of AD4M Languages

Any JavaScript module that implements a create() function that returns an object that implements this interface
is a valid AD4M language.
So the AD4M-internal representation of a language is an object that implements this interface.

Since there are a few different kinds of languages, this interface is split into optional sub-interfaces.
The only required property is the name of the language.

The most usual kind of language is the "Expression Language", which is a language that can be used to create
and share Expressions.
For that, implement the expressionsAdapter and expressionUI interface.

The second most common kind of language is the "Link Language", which is a language that builds the core
of AD4M Neighbourhoods.
For that, implement the linksAdapter interface.

### Table of contents

#### Properties

- [directMessageAdapter](#directmessageadapter)
- [expressionAdapter](#expressionadapter)
- [expressionUI](#expressionui)
- [getAllAdapter](#getalladapter)
- [getByAuthorAdapter](#getbyauthoradapter)
- [languageAdapter](#languageadapter)
- [linksAdapter](#linksadapter)
- [name](#name)
- [settingsUI](#settingsui)
- [teardown](#teardown)
- [telepresenceAdapter](#telepresenceadapter)

#### Methods

- [interactions](#interactions)
- [isImmutableExpression](#isimmutableexpression)

### Properties

#### directMessageAdapter

• `Optional` `Readonly` **directMessageAdapter**: [`DirectMessageAdapter`](#interfaceslanguage_languagedirectmessageadaptermd)

Optional adapter for direct messaging between agents

##### Defined in

[language/Language.ts:65](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L65)

___

#### expressionAdapter

• `Optional` `Readonly` **expressionAdapter**: [`ExpressionAdapter`](#interfaceslanguage_languageexpressionadaptermd)

ExpressionAdapter implements means of getting an Expression
by address and putting an expression

##### Defined in

[language/Language.ts:39](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L39)

___

#### expressionUI

• `Optional` `Readonly` **expressionUI**: [`ExpressionUI`](#interfaceslanguage_languageexpressionuimd)

Interface for getting UI/web components for rendering Expressions of this Language

##### Defined in

[language/Language.ts:42](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L42)

___

#### getAllAdapter

• `Optional` `Readonly` **getAllAdapter**: [`GetAllAdapter`](#interfaceslanguage_languagegetalladaptermd)

Optional adapter for getting all Expressions

##### Defined in

[language/Language.ts:62](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L62)

___

#### getByAuthorAdapter

• `Optional` `Readonly` **getByAuthorAdapter**: [`GetByAuthorAdapter`](#interfaceslanguage_languagegetbyauthoradaptermd)

Optional adapter for getting Expressions by author

##### Defined in

[language/Language.ts:60](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L60)

___

#### languageAdapter

• `Optional` `Readonly` **languageAdapter**: [`LanguageAdapter`](#interfaceslanguage_languagelanguageadaptermd)

Implementation of a Language that defines and stores Languages

##### Defined in

[language/Language.ts:57](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L57)

___

#### linksAdapter

• `Optional` `Readonly` **linksAdapter**: [`LinkSyncAdapter`](#interfaceslanguage_languagelinksyncadaptermd)

Interface of LinkLanguages for the core implementation of Neighbourhoods

##### Defined in

[language/Language.ts:45](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L45)

___

#### name

• `Readonly` **name**: `string`

##### Defined in

[language/Language.ts:27](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L27)

___

#### settingsUI

• `Optional` `Readonly` **settingsUI**: [`SettingsUI`](#interfaceslanguage_languagesettingsuimd)

Interface for providing UI components for the settings of this Language

##### Defined in

[language/Language.ts:68](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L68)

___

#### teardown

• `Optional` `Readonly` **teardown**: () => `void`

##### Type declaration

▸ (): `void`

Optional function to make any cleanup/teardown if your language gets deleting in the ad4m-executor

###### Returns

`void`

##### Defined in

[language/Language.ts:71](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L71)

___

#### telepresenceAdapter

• `Optional` `Readonly` **telepresenceAdapter**: [`TelepresenceAdapter`](#interfaceslanguage_languagetelepresenceadaptermd)

Additional Interface of LinkLanguages that support telepresence features, 
that is: 
 - seeing who is online and getting a status
 - sending/receiveing p2p signals to other online agents without affecting
   the shared Perspective of the Neighbourhood
 (see TelepresenceAdapter for more details)

##### Defined in

[language/Language.ts:54](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L54)

### Methods

#### interactions

▸ **interactions**(`expression`): [`Interaction`](#interfaceslanguage_languageinteractionmd)[]

All available interactions this agent could execute on given expression

##### Parameters

| Name | Type |
| :------ | :------ |
| `expression` | `string` |

##### Returns

[`Interaction`](#interfaceslanguage_languageinteractionmd)[]

##### Defined in

[language/Language.ts:74](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L74)

___

#### isImmutableExpression

▸ `Optional` **isImmutableExpression**(`expression`): `boolean`

Flagging expressions as immutable to enable
expression caching in the ad4m-executor

##### Parameters

| Name | Type |
| :------ | :------ |
| `expression` | `string` |

##### Returns

`boolean`

##### Defined in

[language/Language.ts:32](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L32)


<a name="interfaceslanguage_languagelanguageadaptermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / LanguageAdapter

## Interface: LanguageAdapter

[language/Language](#moduleslanguage_languagemd).LanguageAdapter

### Table of contents

#### Methods

- [getLanguageSource](#getlanguagesource)

### Methods

#### getLanguageSource

▸ **getLanguageSource**(`address`): `Promise`<`string`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

##### Returns

`Promise`<`string`\>

##### Defined in

[language/Language.ts:132](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L132)


<a name="interfaceslanguage_languagelinksyncadaptermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / LinkSyncAdapter

## Interface: LinkSyncAdapter

[language/Language](#moduleslanguage_languagemd).LinkSyncAdapter

Interface for "Link Languages" that facilitate the synchronization
between agents' local Perspectives inside a Neighbourhood.
The assumption is that every version of the shared Perspective
is labeled with a unique revision string.
Changes are committed and retrieved through diffs.
Think of a LinkSyncAdapter as a git branch to which agents commit
their changes to and pull diffs from their current revision
to the latest one.

### Table of contents

#### Methods

- [addCallback](#addcallback)
- [addSyncStateChangeCallback](#addsyncstatechangecallback)
- [commit](#commit)
- [currentRevision](#currentrevision)
- [others](#others)
- [public](#public)
- [render](#render)
- [sync](#sync)
- [writable](#writable)

### Methods

#### addCallback

▸ **addCallback**(`callback`): `any`

Get push notification when a diff got published

##### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`PerspectiveDiffObserver`](#perspectivediffobserver) |

##### Returns

`any`

##### Defined in

[language/Language.ts:186](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L186)

___

#### addSyncStateChangeCallback

▸ **addSyncStateChangeCallback**(`callback`): `any`

Add a sync state callback method

##### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`SyncStateChangeObserver`](#syncstatechangeobserver) |

##### Returns

`any`

##### Defined in

[language/Language.ts:189](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L189)

___

#### commit

▸ **commit**(`diff`): `Promise`<`string`\>

Publish changes

##### Parameters

| Name | Type |
| :------ | :------ |
| `diff` | [`PerspectiveDiff`](#classesperspectives_perspectivediffperspectivediffmd) |

##### Returns

`Promise`<`string`\>

##### Defined in

[language/Language.ts:183](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L183)

___

#### currentRevision

▸ **currentRevision**(): `Promise`<`string`\>

What revision are we on now -> what changes are included in output of render()

##### Returns

`Promise`<`string`\>

##### Defined in

[language/Language.ts:169](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L169)

___

#### others

▸ **others**(): `Promise`<`string`[]\>

##### Returns

`Promise`<`string`[]\>

##### Defined in

[language/Language.ts:166](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L166)

___

#### public

▸ **public**(): `boolean`

##### Returns

`boolean`

##### Defined in

[language/Language.ts:165](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L165)

___

#### render

▸ **render**(): `Promise`<[`Perspective`](#classesperspectives_perspectiveperspectivemd)\>

Returns the full, rendered Perspective at currentRevision

##### Returns

`Promise`<[`Perspective`](#classesperspectives_perspectiveperspectivemd)\>

##### Defined in

[language/Language.ts:180](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L180)

___

#### sync

▸ **sync**(): `Promise`<[`PerspectiveDiff`](#classesperspectives_perspectivediffperspectivediffmd)\>

Check for and get new changes, 
notify others of local changes.
This function will be called every 
few seconds by the ad4m-executor.

##### Returns

`Promise`<[`PerspectiveDiff`](#classesperspectives_perspectivediffperspectivediffmd)\>

##### Defined in

[language/Language.ts:177](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L177)

___

#### writable

▸ **writable**(): `boolean`

##### Returns

`boolean`

##### Defined in

[language/Language.ts:164](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L164)


<a name="interfaceslanguage_languagepublicsharingmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / PublicSharing

## Interface: PublicSharing

[language/Language](#moduleslanguage_languagemd).PublicSharing

Implement this interface if your Language supports creation of sharing
of Expressions.
See ExpressionAdapter

### Table of contents

#### Methods

- [createPublic](#createpublic)

### Methods

#### createPublic

▸ **createPublic**(`content`): `Promise`<`string`\>

Create an Expression and shares it.
Return the Expression's address.

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `content` | `object` | is the object created by the constructorIcon component |

##### Returns

`Promise`<`string`\>

##### Defined in

[language/Language.ts:118](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L118)


<a name="interfaceslanguage_languagereadonlylanguagemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / ReadOnlyLanguage

## Interface: ReadOnlyLanguage

[language/Language](#moduleslanguage_languagemd).ReadOnlyLanguage

Implement this interface if your Language is defined over a static
set of pre-defined Expressions.

### Table of contents

#### Methods

- [addressOf](#addressof)

### Methods

#### addressOf

▸ **addressOf**(`content`): `Promise`<`string`\>

This just calculates the address of an object

##### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `content` | `object` | is the object created by the constructorIcon component |

##### Returns

`Promise`<`string`\>

##### Defined in

[language/Language.ts:128](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L128)


<a name="interfaceslanguage_languagesettingsuimd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / SettingsUI

## Interface: SettingsUI

[language/Language](#moduleslanguage_languagemd).SettingsUI

### Table of contents

#### Methods

- [settingsIcon](#settingsicon)

### Methods

#### settingsIcon

▸ **settingsIcon**(): `string`

##### Returns

`string`

##### Defined in

[language/Language.ts:86](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L86)


<a name="interfaceslanguage_languagetelepresenceadaptermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/Language](#moduleslanguage_languagemd) / TelepresenceAdapter

## Interface: TelepresenceAdapter

[language/Language](#moduleslanguage_languagemd).TelepresenceAdapter

### Table of contents

#### Methods

- [getOnlineAgents](#getonlineagents)
- [registerSignalCallback](#registersignalcallback)
- [sendBroadcast](#sendbroadcast)
- [sendSignal](#sendsignal)
- [setOnlineStatus](#setonlinestatus)

### Methods

#### getOnlineAgents

▸ **getOnlineAgents**(): `Promise`<[`OnlineAgent`](#classeslanguage_languageonlineagentmd)[]\>

##### Returns

`Promise`<[`OnlineAgent`](#classeslanguage_languageonlineagentmd)[]\>

##### Defined in

[language/Language.ts:261](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L261)

___

#### registerSignalCallback

▸ **registerSignalCallback**(`callback`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`TelepresenceSignalCallback`](#telepresencesignalcallback) |

##### Returns

`Promise`<`void`\>

##### Defined in

[language/Language.ts:265](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L265)

___

#### sendBroadcast

▸ **sendBroadcast**(`payload`): `Promise`<`object`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd) |

##### Returns

`Promise`<`object`\>

##### Defined in

[language/Language.ts:264](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L264)

___

#### sendSignal

▸ **sendSignal**(`remoteAgentDid`, `payload`): `Promise`<`object`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `remoteAgentDid` | `string` |
| `payload` | [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd) |

##### Returns

`Promise`<`object`\>

##### Defined in

[language/Language.ts:263](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L263)

___

#### setOnlineStatus

▸ **setOnlineStatus**(`status`): `Promise`<`void`\>

##### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd) |

##### Returns

`Promise`<`void`\>

##### Defined in

[language/Language.ts:260](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L260)


<a name="interfaceslanguage_languagecontextagentservicemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageContext](#moduleslanguage_languagecontextmd) / AgentService

## Interface: AgentService

[language/LanguageContext](#moduleslanguage_languagecontextmd).AgentService

### Table of contents

#### Properties

- [did](#did)

#### Methods

- [createSignedExpression](#createsignedexpression)

### Properties

#### did

• `Readonly` **did**: `string`

##### Defined in

[language/LanguageContext.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L6)

### Methods

#### createSignedExpression

▸ **createSignedExpression**(`data`): [`Expression`](#classesexpression_expressionexpressionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `data` | `any` |

##### Returns

[`Expression`](#classesexpression_expressionexpressionmd)

##### Defined in

[language/LanguageContext.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L7)


<a name="interfaceslanguage_languagecontextholochainlanguagedelegatemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageContext](#moduleslanguage_languagecontextmd) / HolochainLanguageDelegate

## Interface: HolochainLanguageDelegate

[language/LanguageContext](#moduleslanguage_languagecontextmd).HolochainLanguageDelegate

### Table of contents

#### Methods

- [call](#call)
- [callAsync](#callasync)
- [registerDNAs](#registerdnas)

### Methods

#### call

▸ **call**(`dnaNick`, `zomeName`, `fnName`, `params`): `Promise`<`any`\>

Makes a single call to a given holochain DNA. Underlying implementation puts these calls into a sync fifo queue

##### Parameters

| Name | Type |
| :------ | :------ |
| `dnaNick` | `string` |
| `zomeName` | `string` |
| `fnName` | `string` |
| `params` | `string` \| `object` |

##### Returns

`Promise`<`any`\>

##### Defined in

[language/LanguageContext.ts:34](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L34)

___

#### callAsync

▸ **callAsync**(`calls`, `timeoutMs?`): `Promise`<`any`[]\>

Makes all supplied calls in parallel to the provided holochain dna... Should only be called on read operations to avoid source chain async mutation errors

##### Parameters

| Name | Type |
| :------ | :------ |
| `calls` | { `dnaNick`: `string` ; `fnName`: `string` ; `params`: `string` \| `object` ; `zomeName`: `string`  }[] |
| `timeoutMs?` | `number` |

##### Returns

`Promise`<`any`[]\>

##### Defined in

[language/LanguageContext.ts:36](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L36)

___

#### registerDNAs

▸ **registerDNAs**(`dnas`, `holochainSignalCallback?`): `Promise`<`void`\>

Installs/registers a given DNA in the ad4m-executor

##### Parameters

| Name | Type |
| :------ | :------ |
| `dnas` | [`Dna`](#classeslanguage_languagecontextdnamd)[] |
| `holochainSignalCallback?` | `AppSignalCb` |

##### Returns

`Promise`<`void`\>

##### Defined in

[language/LanguageContext.ts:32](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L32)


<a name="interfaceslanguage_languagecontextlanguagecontextmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageContext](#moduleslanguage_languagecontextmd) / LanguageContext

## Interface: LanguageContext

[language/LanguageContext](#moduleslanguage_languagecontextmd).LanguageContext

### Table of contents

#### Properties

- [Holochain](#holochain)
- [IPFS](#ipfs)
- [ad4mSignal](#ad4msignal)
- [agent](#agent)
- [customSettings](#customsettings)
- [signatures](#signatures)
- [storageDirectory](#storagedirectory)

### Properties

#### Holochain

• **Holochain**: [`HolochainLanguageDelegate`](#interfaceslanguage_languagecontextholochainlanguagedelegatemd)

##### Defined in

[language/LanguageContext.ts:20](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L20)

___

#### IPFS

• **IPFS**: `IPFS`<{}\>

##### Defined in

[language/LanguageContext.ts:16](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L16)

___

#### ad4mSignal

• **ad4mSignal**: [`Ad4mSignalCB`](#ad4msignalcb)

##### Defined in

[language/LanguageContext.ts:21](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L21)

___

#### agent

• **agent**: [`AgentService`](#interfaceslanguage_languagecontextagentservicemd)

##### Defined in

[language/LanguageContext.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L15)

___

#### customSettings

• **customSettings**: `object`

##### Defined in

[language/LanguageContext.ts:19](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L19)

___

#### signatures

• **signatures**: [`SignaturesService`](#interfaceslanguage_languagecontextsignaturesservicemd)

##### Defined in

[language/LanguageContext.ts:17](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L17)

___

#### storageDirectory

• **storageDirectory**: `string`

##### Defined in

[language/LanguageContext.ts:18](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L18)


<a name="interfaceslanguage_languagecontextsignaturesservicemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / [language/LanguageContext](#moduleslanguage_languagecontextmd) / SignaturesService

## Interface: SignaturesService

[language/LanguageContext](#moduleslanguage_languagecontextmd).SignaturesService

### Table of contents

#### Methods

- [verify](#verify)

### Methods

#### verify

▸ **verify**(`expr`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `expr` | [`Expression`](#classesexpression_expressionexpressionmd) |

##### Returns

`boolean`

##### Defined in

[language/LanguageContext.ts:11](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L11)


<a name="modulesmd"></a>

[@perspect3vism/ad4m](#readmemd) / Exports

# @perspect3vism/ad4m

## Table of contents

### Modules

- [Ad4mClient](#modulesad4mclientmd)
- [Address](#modulesaddressmd)
- [DID](#modulesdidmd)
- [Exception](#modulesexceptionmd)
- [Literal](#modulesliteralmd)
- [PubSub](#modulespubsubmd)
- [SmartLiteral](#modulessmartliteralmd)
- [agent/Agent](#modulesagent_agentmd)
- [agent/AgentClient](#modulesagent_agentclientmd)
- [agent/AgentResolver](#modulesagent_agentresolvermd)
- [agent/AgentStatus](#modulesagent_agentstatusmd)
- [buildSchema](#modulesbuildschemamd)
- [expression/Expression](#modulesexpression_expressionmd)
- [expression/ExpressionClient](#modulesexpression_expressionclientmd)
- [expression/ExpressionRef](#modulesexpression_expressionrefmd)
- [expression/ExpressionResolver](#modulesexpression_expressionresolvermd)
- [jestSetup](#modulesjestsetupmd)
- [language/Icon](#moduleslanguage_iconmd)
- [language/Language](#moduleslanguage_languagemd)
- [language/LanguageClient](#moduleslanguage_languageclientmd)
- [language/LanguageContext](#moduleslanguage_languagecontextmd)
- [language/LanguageHandle](#moduleslanguage_languagehandlemd)
- [language/LanguageMeta](#moduleslanguage_languagemetamd)
- [language/LanguageRef](#moduleslanguage_languagerefmd)
- [language/LanguageResolver](#moduleslanguage_languageresolvermd)
- [links/Links](#moduleslinks_linksmd)
- [neighbourhood/Neighbourhood](#modulesneighbourhood_neighbourhoodmd)
- [neighbourhood/NeighbourhoodClient](#modulesneighbourhood_neighbourhoodclientmd)
- [neighbourhood/NeighbourhoodProxy](#modulesneighbourhood_neighbourhoodproxymd)
- [neighbourhood/NeighbourhoodResolver](#modulesneighbourhood_neighbourhoodresolvermd)
- [perspectives/LinkQuery](#modulesperspectives_linkquerymd)
- [perspectives/Perspective](#modulesperspectives_perspectivemd)
- [perspectives/PerspectiveClient](#modulesperspectives_perspectiveclientmd)
- [perspectives/PerspectiveDiff](#modulesperspectives_perspectivediffmd)
- [perspectives/PerspectiveHandle](#modulesperspectives_perspectivehandlemd)
- [perspectives/PerspectiveProxy](#modulesperspectives_perspectiveproxymd)
- [perspectives/PerspectiveResolver](#modulesperspectives_perspectiveresolvermd)
- [runtime/RuntimeClient](#modulesruntime_runtimeclientmd)
- [runtime/RuntimeResolver](#modulesruntime_runtimeresolvermd)
- [subject/SDNADecorators](#modulessubject_sdnadecoratorsmd)
- [subject/Subject](#modulessubject_subjectmd)
- [subject/SubjectEntity](#modulessubject_subjectentitymd)
- [subject/util](#modulessubject_utilmd)
- [typeDefs](#modulestypedefsmd)
- [unwrapApolloResult](#modulesunwrapapolloresultmd)
- [utils](#modulesutilsmd)

# Modules


<a name="modulesad4mclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / Ad4mClient

## Module: Ad4mClient

### Table of contents

#### Classes

- [Ad4mClient](#classesad4mclientad4mclientmd)


<a name="modulesaddressmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / Address

## Module: Address

### Table of contents

#### Type Aliases

- [Address](#address)

### Type Aliases

#### Address

Ƭ **Address**: `string`

##### Defined in

[Address.ts:1](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/Address.ts#L1)


<a name="modulesdidmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / DID

## Module: DID

### Table of contents

#### Type Aliases

- [DID](#did)

### Type Aliases

#### DID

Ƭ **DID**: `string`

##### Defined in

[DID.ts:1](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/DID.ts#L1)


<a name="modulesexceptionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / Exception

## Module: Exception

### Table of contents

#### Enumerations

- [ExceptionType](#enumsexceptionexceptiontypemd)


<a name="modulesliteralmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / Literal

## Module: Literal

### Table of contents

#### Classes

- [Literal](#classesliteralliteralmd)


<a name="modulespubsubmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / PubSub

## Module: PubSub

### Table of contents

#### Variables

- [AGENT\_STATUS\_CHANGED](#agent_status_changed)
- [AGENT\_UPDATED](#agent_updated)
- [APPS\_CHANGED](#apps_changed)
- [DIRECT\_MESSAGE\_RECEIVED](#direct_message_received)
- [EXCEPTION\_OCCURRED\_TOPIC](#exception_occurred_topic)
- [LINK\_ADDED\_TOPIC](#link_added_topic)
- [LINK\_REMOVED\_TOPIC](#link_removed_topic)
- [LINK\_UDATED\_TOPIC](#link_udated_topic)
- [NEIGHBOURHOOD\_SIGNAL\_RECEIVED\_TOPIC](#neighbourhood_signal_received_topic)
- [PERSPECTIVE\_ADDED\_TOPIC](#perspective_added_topic)
- [PERSPECTIVE\_REMOVED\_TOPIC](#perspective_removed_topic)
- [PERSPECTIVE\_SYNC\_STATE\_CHANGE](#perspective_sync_state_change)
- [PERSPECTIVE\_UPDATED\_TOPIC](#perspective_updated_topic)
- [SIGNAL](#signal)

### Variables

#### AGENT\_STATUS\_CHANGED

• `Const` **AGENT\_STATUS\_CHANGED**: ``"agent-status-changed-topic"``

##### Defined in

[PubSub.ts:2](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L2)

___

#### AGENT\_UPDATED

• `Const` **AGENT\_UPDATED**: ``"agent-updated-topic"``

##### Defined in

[PubSub.ts:1](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L1)

___

#### APPS\_CHANGED

• `Const` **APPS\_CHANGED**: ``"apps-changed"``

##### Defined in

[PubSub.ts:14](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L14)

___

#### DIRECT\_MESSAGE\_RECEIVED

• `Const` **DIRECT\_MESSAGE\_RECEIVED**: ``"direct-message-received-topic"``

##### Defined in

[PubSub.ts:3](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L3)

___

#### EXCEPTION\_OCCURRED\_TOPIC

• `Const` **EXCEPTION\_OCCURRED\_TOPIC**: ``"exception-occurred-topic"``

##### Defined in

[PubSub.ts:11](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L11)

___

#### LINK\_ADDED\_TOPIC

• `Const` **LINK\_ADDED\_TOPIC**: ``"link-added-topic"``

##### Defined in

[PubSub.ts:7](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L7)

___

#### LINK\_REMOVED\_TOPIC

• `Const` **LINK\_REMOVED\_TOPIC**: ``"link-removed-topic"``

##### Defined in

[PubSub.ts:8](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L8)

___

#### LINK\_UDATED\_TOPIC

• `Const` **LINK\_UDATED\_TOPIC**: ``"link-updated-topic"``

##### Defined in

[PubSub.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L9)

___

#### NEIGHBOURHOOD\_SIGNAL\_RECEIVED\_TOPIC

• `Const` **NEIGHBOURHOOD\_SIGNAL\_RECEIVED\_TOPIC**: ``"neighbourhood-signal-received-topic"``

##### Defined in

[PubSub.ts:12](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L12)

___

#### PERSPECTIVE\_ADDED\_TOPIC

• `Const` **PERSPECTIVE\_ADDED\_TOPIC**: ``"perspective-added-topic"``

##### Defined in

[PubSub.ts:4](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L4)

___

#### PERSPECTIVE\_REMOVED\_TOPIC

• `Const` **PERSPECTIVE\_REMOVED\_TOPIC**: ``"perspective-removed-topic"``

##### Defined in

[PubSub.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L6)

___

#### PERSPECTIVE\_SYNC\_STATE\_CHANGE

• `Const` **PERSPECTIVE\_SYNC\_STATE\_CHANGE**: ``"perspective-sync-state-change"``

##### Defined in

[PubSub.ts:13](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L13)

___

#### PERSPECTIVE\_UPDATED\_TOPIC

• `Const` **PERSPECTIVE\_UPDATED\_TOPIC**: ``"perspective-updated-topic"``

##### Defined in

[PubSub.ts:5](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L5)

___

#### SIGNAL

• `Const` **SIGNAL**: ``"signal"``

##### Defined in

[PubSub.ts:10](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/PubSub.ts#L10)


<a name="modulessmartliteralmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / SmartLiteral

## Module: SmartLiteral

### Table of contents

#### Classes

- [SmartLiteral](#classessmartliteralsmartliteralmd)

#### Variables

- [SMART\_LITERAL\_CONTENT\_PREDICATE](#smart_literal_content_predicate)

### Variables

#### SMART\_LITERAL\_CONTENT\_PREDICATE

• `Const` **SMART\_LITERAL\_CONTENT\_PREDICATE**: ``"smart_literal://content"``

##### Defined in

[SmartLiteral.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/SmartLiteral.ts#L6)


<a name="modulesagent_agentmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / agent/Agent

## Module: agent/Agent

### Table of contents

#### Classes

- [Agent](#classesagent_agentagentmd)
- [AgentExpression](#classesagent_agentagentexpressionmd)
- [AgentSignature](#classesagent_agentagentsignaturemd)
- [Apps](#classesagent_agentappsmd)
- [AuthInfo](#classesagent_agentauthinfomd)
- [AuthInfoInput](#classesagent_agentauthinfoinputmd)
- [Capability](#classesagent_agentcapabilitymd)
- [CapabilityInput](#classesagent_agentcapabilityinputmd)
- [EntanglementProof](#classesagent_agententanglementproofmd)
- [EntanglementProofInput](#classesagent_agententanglementproofinputmd)
- [Resource](#classesagent_agentresourcemd)
- [ResourceInput](#classesagent_agentresourceinputmd)


<a name="modulesagent_agentclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / agent/AgentClient

## Module: agent/AgentClient

### Table of contents

#### Classes

- [AgentClient](#classesagent_agentclientagentclientmd)

#### Interfaces

- [InitializeArgs](#interfacesagent_agentclientinitializeargsmd)

#### Type Aliases

- [AgentAppsUpdatedCallback](#agentappsupdatedcallback)
- [AgentStatusChangedCallback](#agentstatuschangedcallback)
- [AgentUpdatedCallback](#agentupdatedcallback)

### Type Aliases

#### AgentAppsUpdatedCallback

Ƭ **AgentAppsUpdatedCallback**: () => ``null``

##### Type declaration

▸ (): ``null``

###### Returns

``null``

##### Defined in

[agent/AgentClient.ts:81](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L81)

___

#### AgentStatusChangedCallback

Ƭ **AgentStatusChangedCallback**: (`agent`: [`Agent`](#classesagent_agentagentmd)) => ``null``

##### Type declaration

▸ (`agent`): ``null``

###### Parameters

| Name | Type |
| :------ | :------ |
| `agent` | [`Agent`](#classesagent_agentagentmd) |

###### Returns

``null``

##### Defined in

[agent/AgentClient.ts:80](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L80)

___

#### AgentUpdatedCallback

Ƭ **AgentUpdatedCallback**: (`agent`: [`Agent`](#classesagent_agentagentmd)) => ``null``

##### Type declaration

▸ (`agent`): ``null``

###### Parameters

| Name | Type |
| :------ | :------ |
| `agent` | [`Agent`](#classesagent_agentagentmd) |

###### Returns

``null``

##### Defined in

[agent/AgentClient.ts:79](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentClient.ts#L79)


<a name="modulesagent_agentresolvermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / agent/AgentResolver

## Module: agent/AgentResolver

### Table of contents

#### Classes

- [default](#classesagent_agentresolverdefaultmd)

#### Variables

- [TEST\_AGENT\_DID](#test_agent_did)

### Variables

#### TEST\_AGENT\_DID

• `Const` **TEST\_AGENT\_DID**: ``"did:ad4m:test"``

##### Defined in

[agent/AgentResolver.ts:21](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/agent/AgentResolver.ts#L21)


<a name="modulesagent_agentstatusmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / agent/AgentStatus

## Module: agent/AgentStatus

### Table of contents

#### Classes

- [AgentStatus](#classesagent_agentstatusagentstatusmd)


<a name="modulesbuildschemamd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / buildSchema

## Module: buildSchema


<a name="modulesexpression_expressionmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / expression/Expression

## Module: expression/Expression

### Table of contents

#### Classes

- [Expression](#classesexpression_expressionexpressionmd)
- [ExpressionProof](#classesexpression_expressionexpressionproofmd)
- [ExpressionProofInput](#classesexpression_expressionexpressionproofinputmd)
- [ExpressionRendered](#classesexpression_expressionexpressionrenderedmd)

#### Functions

- [ExpressionGeneric](#expressiongeneric)
- [ExpressionGenericInput](#expressiongenericinput)
- [isExpression](#isexpression)

### Functions

#### ExpressionGeneric

▸ **ExpressionGeneric**<`DataType`\>(`DataTypeClass`): `any`

##### Type parameters

| Name |
| :------ |
| `DataType` |

##### Parameters

| Name | Type |
| :------ | :------ |
| `DataTypeClass` | `ClassType`<`DataType`\> |

##### Returns

`any`

##### Defined in

[expression/Expression.ts:42](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L42)

___

#### ExpressionGenericInput

▸ **ExpressionGenericInput**<`DataType`\>(`DataTypeClass`): `any`

##### Type parameters

| Name |
| :------ |
| `DataType` |

##### Parameters

| Name | Type |
| :------ | :------ |
| `DataTypeClass` | `ClassType`<`DataType`\> |

##### Returns

`any`

##### Defined in

[expression/Expression.ts:67](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L67)

___

#### isExpression

▸ **isExpression**(`e`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `e` | `any` |

##### Returns

`boolean`

##### Defined in

[expression/Expression.ts:97](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/Expression.ts#L97)


<a name="modulesexpression_expressionclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / expression/ExpressionClient

## Module: expression/ExpressionClient

### Table of contents

#### Classes

- [ExpressionClient](#classesexpression_expressionclientexpressionclientmd)


<a name="modulesexpression_expressionrefmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / expression/ExpressionRef

## Module: expression/ExpressionRef

### Table of contents

#### Classes

- [ExpressionRef](#classesexpression_expressionrefexpressionrefmd)

#### Functions

- [exprRef2String](#exprref2string)
- [parseExprUrl](#parseexprurl)

### Functions

#### exprRef2String

▸ **exprRef2String**(`ref`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `ref` | [`ExpressionRef`](#classesexpression_expressionrefexpressionrefmd) |

##### Returns

`string`

##### Defined in

[expression/ExpressionRef.ts:22](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionRef.ts#L22)

___

#### parseExprUrl

▸ **parseExprUrl**(`url`): [`ExpressionRef`](#classesexpression_expressionrefexpressionrefmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

##### Returns

[`ExpressionRef`](#classesexpression_expressionrefexpressionrefmd)

##### Defined in

[expression/ExpressionRef.ts:29](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/expression/ExpressionRef.ts#L29)


<a name="modulesexpression_expressionresolvermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / expression/ExpressionResolver

## Module: expression/ExpressionResolver

### Table of contents

#### Classes

- [default](#classesexpression_expressionresolverdefaultmd)


<a name="modulesjestsetupmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / jestSetup

## Module: jestSetup


<a name="moduleslanguage_iconmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / language/Icon

## Module: language/Icon

### Table of contents

#### Classes

- [Icon](#classeslanguage_iconiconmd)


<a name="moduleslanguage_languagemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / language/Language

## Module: language/Language

### Table of contents

#### Classes

- [InteractionCall](#classeslanguage_languageinteractioncallmd)
- [InteractionMeta](#classeslanguage_languageinteractionmetamd)
- [InteractionParameter](#classeslanguage_languageinteractionparametermd)
- [OnlineAgent](#classeslanguage_languageonlineagentmd)

#### Interfaces

- [DirectMessageAdapter](#interfaceslanguage_languagedirectmessageadaptermd)
- [ExpressionAdapter](#interfaceslanguage_languageexpressionadaptermd)
- [ExpressionUI](#interfaceslanguage_languageexpressionuimd)
- [GetAllAdapter](#interfaceslanguage_languagegetalladaptermd)
- [GetByAuthorAdapter](#interfaceslanguage_languagegetbyauthoradaptermd)
- [Interaction](#interfaceslanguage_languageinteractionmd)
- [Language](#interfaceslanguage_languagelanguagemd)
- [LanguageAdapter](#interfaceslanguage_languagelanguageadaptermd)
- [LinkSyncAdapter](#interfaceslanguage_languagelinksyncadaptermd)
- [PublicSharing](#interfaceslanguage_languagepublicsharingmd)
- [ReadOnlyLanguage](#interfaceslanguage_languagereadonlylanguagemd)
- [SettingsUI](#interfaceslanguage_languagesettingsuimd)
- [TelepresenceAdapter](#interfaceslanguage_languagetelepresenceadaptermd)

#### Type Aliases

- [MessageCallback](#messagecallback)
- [PerspectiveDiffObserver](#perspectivediffobserver)
- [StatusCallback](#statuscallback)
- [SyncStateChangeObserver](#syncstatechangeobserver)
- [TelepresenceSignalCallback](#telepresencesignalcallback)

### Type Aliases

#### MessageCallback

Ƭ **MessageCallback**: (`message`: [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)) => `void`

##### Type declaration

▸ (`message`): `void`

###### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd) |

###### Returns

`void`

##### Defined in

[language/Language.ts:192](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L192)

___

#### PerspectiveDiffObserver

Ƭ **PerspectiveDiffObserver**: (`diff`: [`PerspectiveDiff`](#classesperspectives_perspectivediffperspectivediffmd)) => `void`

##### Type declaration

▸ (`diff`): `void`

###### Parameters

| Name | Type |
| :------ | :------ |
| `diff` | [`PerspectiveDiff`](#classesperspectives_perspectivediffperspectivediffmd) |

###### Returns

`void`

##### Defined in

[language/Language.ts:151](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L151)

___

#### StatusCallback

Ƭ **StatusCallback**: (`caller`: [`DID`](#did)) => [`Perspective`](#classesperspectives_perspectiveperspectivemd)

##### Type declaration

▸ (`caller`): [`Perspective`](#classesperspectives_perspectiveperspectivemd)

###### Parameters

| Name | Type |
| :------ | :------ |
| `caller` | [`DID`](#did) |

###### Returns

[`Perspective`](#classesperspectives_perspectiveperspectivemd)

##### Defined in

[language/Language.ts:193](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L193)

___

#### SyncStateChangeObserver

Ƭ **SyncStateChangeObserver**: (`state`: [`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd)) => `void`

##### Type declaration

▸ (`state`): `void`

###### Parameters

| Name | Type |
| :------ | :------ |
| `state` | [`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd) |

###### Returns

`void`

##### Defined in

[language/Language.ts:152](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L152)

___

#### TelepresenceSignalCallback

Ƭ **TelepresenceSignalCallback**: (`payload`: [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)) => `void`

##### Type declaration

▸ (`payload`): `void`

###### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd) |

###### Returns

`void`

##### Defined in

[language/Language.ts:258](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/Language.ts#L258)


<a name="moduleslanguage_languageclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / language/LanguageClient

## Module: language/LanguageClient

### Table of contents

#### Classes

- [LanguageClient](#classeslanguage_languageclientlanguageclientmd)


<a name="moduleslanguage_languagecontextmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / language/LanguageContext

## Module: language/LanguageContext

### Table of contents

#### Classes

- [Dna](#classeslanguage_languagecontextdnamd)

#### Interfaces

- [AgentService](#interfaceslanguage_languagecontextagentservicemd)
- [HolochainLanguageDelegate](#interfaceslanguage_languagecontextholochainlanguagedelegatemd)
- [LanguageContext](#interfaceslanguage_languagecontextlanguagecontextmd)
- [SignaturesService](#interfaceslanguage_languagecontextsignaturesservicemd)

#### Type Aliases

- [Ad4mSignalCB](#ad4msignalcb)

### Type Aliases

#### Ad4mSignalCB

Ƭ **Ad4mSignalCB**: (`signal`: `any`) => `void`

##### Type declaration

▸ (`signal`): `void`

###### Parameters

| Name | Type |
| :------ | :------ |
| `signal` | `any` |

###### Returns

`void`

##### Defined in

[language/LanguageContext.ts:39](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/language/LanguageContext.ts#L39)


<a name="moduleslanguage_languagehandlemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / language/LanguageHandle

## Module: language/LanguageHandle

### Table of contents

#### Classes

- [LanguageHandle](#classeslanguage_languagehandlelanguagehandlemd)


<a name="moduleslanguage_languagemetamd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / language/LanguageMeta

## Module: language/LanguageMeta

### Table of contents

#### Classes

- [LanguageExpression](#classeslanguage_languagemetalanguageexpressionmd)
- [LanguageLanguageInput](#classeslanguage_languagemetalanguagelanguageinputmd)
- [LanguageMeta](#classeslanguage_languagemetalanguagemetamd)
- [LanguageMetaInput](#classeslanguage_languagemetalanguagemetainputmd)
- [LanguageMetaInternal](#classeslanguage_languagemetalanguagemetainternalmd)


<a name="moduleslanguage_languagerefmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / language/LanguageRef

## Module: language/LanguageRef

### Table of contents

#### Classes

- [LanguageRef](#classeslanguage_languagereflanguagerefmd)


<a name="moduleslanguage_languageresolvermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / language/LanguageResolver

## Module: language/LanguageResolver

### Table of contents

#### Classes

- [default](#classeslanguage_languageresolverdefaultmd)


<a name="moduleslinks_linksmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / links/Links

## Module: links/Links

### Table of contents

#### Classes

- [Link](#classeslinks_linkslinkmd)
- [LinkExpression](#classeslinks_linkslinkexpressionmd)
- [LinkExpressionInput](#classeslinks_linkslinkexpressioninputmd)
- [LinkExpressionMutations](#classeslinks_linkslinkexpressionmutationsmd)
- [LinkExpressionUpdated](#classeslinks_linkslinkexpressionupdatedmd)
- [LinkInput](#classeslinks_linkslinkinputmd)
- [LinkMutations](#classeslinks_linkslinkmutationsmd)

#### Functions

- [isLink](#islink)
- [linkEqual](#linkequal)

### Functions

#### isLink

▸ **isLink**(`l`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `l` | `any` |

##### Returns

`boolean`

##### Defined in

[links/Links.ts:82](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L82)

___

#### linkEqual

▸ **linkEqual**(`l1`, `l2`): `boolean`

##### Parameters

| Name | Type |
| :------ | :------ |
| `l1` | [`LinkExpression`](#classeslinks_linkslinkexpressionmd) |
| `l2` | [`LinkExpression`](#classeslinks_linkslinkexpressionmd) |

##### Returns

`boolean`

##### Defined in

[links/Links.ts:74](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/links/Links.ts#L74)


<a name="modulesneighbourhood_neighbourhoodmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / neighbourhood/Neighbourhood

## Module: neighbourhood/Neighbourhood

### Table of contents

#### Classes

- [Neighbourhood](#classesneighbourhood_neighbourhoodneighbourhoodmd)
- [NeighbourhoodExpression](#classesneighbourhood_neighbourhoodneighbourhoodexpressionmd)


<a name="modulesneighbourhood_neighbourhoodclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / neighbourhood/NeighbourhoodClient

## Module: neighbourhood/NeighbourhoodClient

### Table of contents

#### Classes

- [NeighbourhoodClient](#classesneighbourhood_neighbourhoodclientneighbourhoodclientmd)


<a name="modulesneighbourhood_neighbourhoodproxymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / neighbourhood/NeighbourhoodProxy

## Module: neighbourhood/NeighbourhoodProxy

### Table of contents

#### Classes

- [NeighbourhoodProxy](#classesneighbourhood_neighbourhoodproxyneighbourhoodproxymd)


<a name="modulesneighbourhood_neighbourhoodresolvermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / neighbourhood/NeighbourhoodResolver

## Module: neighbourhood/NeighbourhoodResolver

### Table of contents

#### Classes

- [default](#classesneighbourhood_neighbourhoodresolverdefaultmd)


<a name="modulesperspectives_linkquerymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / perspectives/LinkQuery

## Module: perspectives/LinkQuery

### Table of contents

#### Classes

- [LinkQuery](#classesperspectives_linkquerylinkquerymd)


<a name="modulesperspectives_perspectivemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / perspectives/Perspective

## Module: perspectives/Perspective

### Table of contents

#### Classes

- [Perspective](#classesperspectives_perspectiveperspectivemd)
- [PerspectiveExpression](#classesperspectives_perspectiveperspectiveexpressionmd)
- [PerspectiveInput](#classesperspectives_perspectiveperspectiveinputmd)
- [PerspectiveUnsignedInput](#classesperspectives_perspectiveperspectiveunsignedinputmd)


<a name="modulesperspectives_perspectiveclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / perspectives/PerspectiveClient

## Module: perspectives/PerspectiveClient

### Table of contents

#### Classes

- [PerspectiveClient](#classesperspectives_perspectiveclientperspectiveclientmd)

#### Type Aliases

- [LinkCallback](#linkcallback)
- [PerspectiveHandleCallback](#perspectivehandlecallback)
- [SyncStateChangeCallback](#syncstatechangecallback)
- [UuidCallback](#uuidcallback)

### Type Aliases

#### LinkCallback

Ƭ **LinkCallback**: (`link`: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)) => ``null``

##### Type declaration

▸ (`link`): ``null``

###### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`LinkExpression`](#classeslinks_linkslinkexpressionmd) |

###### Returns

``null``

##### Defined in

[perspectives/PerspectiveClient.ts:41](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L41)

___

#### PerspectiveHandleCallback

Ƭ **PerspectiveHandleCallback**: (`perspective`: [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd)) => ``null``

##### Type declaration

▸ (`perspective`): ``null``

###### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveHandle`](#classesperspectives_perspectivehandleperspectivehandlemd) |

###### Returns

``null``

##### Defined in

[perspectives/PerspectiveClient.ts:39](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L39)

___

#### SyncStateChangeCallback

Ƭ **SyncStateChangeCallback**: (`state`: [`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd)) => ``null``

##### Type declaration

▸ (`state`): ``null``

###### Parameters

| Name | Type |
| :------ | :------ |
| `state` | [`PerspectiveState`](#enumsperspectives_perspectivehandleperspectivestatemd) |

###### Returns

``null``

##### Defined in

[perspectives/PerspectiveClient.ts:42](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L42)

___

#### UuidCallback

Ƭ **UuidCallback**: (`uuid`: `string`) => ``null``

##### Type declaration

▸ (`uuid`): ``null``

###### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

###### Returns

``null``

##### Defined in

[perspectives/PerspectiveClient.ts:40](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveClient.ts#L40)


<a name="modulesperspectives_perspectivediffmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / perspectives/PerspectiveDiff

## Module: perspectives/PerspectiveDiff

### Table of contents

#### Classes

- [PerspectiveDiff](#classesperspectives_perspectivediffperspectivediffmd)
- [PerspectiveDiffExpression](#classesperspectives_perspectivediffperspectivediffexpressionmd)


<a name="modulesperspectives_perspectivehandlemd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / perspectives/PerspectiveHandle

## Module: perspectives/PerspectiveHandle

### Table of contents

#### Enumerations

- [PerspectiveState](#enumsperspectives_perspectivehandleperspectivestatemd)

#### Classes

- [PerspectiveHandle](#classesperspectives_perspectivehandleperspectivehandlemd)


<a name="modulesperspectives_perspectiveproxymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / perspectives/PerspectiveProxy

## Module: perspectives/PerspectiveProxy

### Table of contents

#### Classes

- [PerspectiveProxy](#classesperspectives_perspectiveproxyperspectiveproxymd)


<a name="modulesperspectives_perspectiveresolvermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / perspectives/PerspectiveResolver

## Module: perspectives/PerspectiveResolver

### Table of contents

#### Classes

- [default](#classesperspectives_perspectiveresolverdefaultmd)

#### Variables

- [testLink](#testlink)

### Variables

#### testLink

• `Const` **testLink**: [`LinkExpression`](#classeslinks_linkslinkexpressionmd)

##### Defined in

[perspectives/PerspectiveResolver.ts:9](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/perspectives/PerspectiveResolver.ts#L9)


<a name="modulesruntime_runtimeclientmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / runtime/RuntimeClient

## Module: runtime/RuntimeClient

### Table of contents

#### Classes

- [RuntimeClient](#classesruntime_runtimeclientruntimeclientmd)

#### Type Aliases

- [ExceptionCallback](#exceptioncallback)
- [MessageCallback](#messagecallback)

### Type Aliases

#### ExceptionCallback

Ƭ **ExceptionCallback**: (`info`: [`ExceptionInfo`](#classesruntime_runtimeresolverexceptioninfomd)) => ``null``

##### Type declaration

▸ (`info`): ``null``

###### Parameters

| Name | Type |
| :------ | :------ |
| `info` | [`ExceptionInfo`](#classesruntime_runtimeresolverexceptioninfomd) |

###### Returns

``null``

##### Defined in

[runtime/RuntimeClient.ts:21](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L21)

___

#### MessageCallback

Ƭ **MessageCallback**: (`message`: [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd)) => ``null``

##### Type declaration

▸ (`message`): ``null``

###### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`PerspectiveExpression`](#classesperspectives_perspectiveperspectiveexpressionmd) |

###### Returns

``null``

##### Defined in

[runtime/RuntimeClient.ts:20](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/runtime/RuntimeClient.ts#L20)


<a name="modulesruntime_runtimeresolvermd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / runtime/RuntimeResolver

## Module: runtime/RuntimeResolver

### Table of contents

#### Classes

- [ExceptionInfo](#classesruntime_runtimeresolverexceptioninfomd)
- [RuntimeInfo](#classesruntime_runtimeresolverruntimeinfomd)
- [SentMessage](#classesruntime_runtimeresolversentmessagemd)
- [default](#classesruntime_runtimeresolverdefaultmd)


<a name="modulessubject_sdnadecoratorsmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / subject/SDNADecorators

## Module: subject/SDNADecorators

### Table of contents

#### Classes

- [PerspectiveAction](#classessubject_sdnadecoratorsperspectiveactionmd)

#### Functions

- [SDNAClass](#sdnaclass)
- [addLink](#addlink)
- [hasLink](#haslink)
- [instanceQuery](#instancequery)
- [makeRandomPrologAtom](#makerandomprologatom)
- [subjectCollection](#subjectcollection)
- [subjectFlag](#subjectflag)
- [subjectProperty](#subjectproperty)

### Functions

#### SDNAClass

▸ **SDNAClass**(`opts`): (`target`: `any`) => `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `opts` | `SDNAClassOptions` |

##### Returns

`fn`

▸ (`target`): `void`

###### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `any` |

###### Returns

`void`

##### Defined in

[subject/SDNADecorators.ts:156](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L156)

___

#### addLink

▸ **addLink**(`source`, `predicate`, `target`): [`PerspectiveAction`](#classessubject_sdnadecoratorsperspectiveactionmd)

##### Parameters

| Name | Type |
| :------ | :------ |
| `source` | `string` |
| `predicate` | `string` |
| `target` | `string` |

##### Returns

[`PerspectiveAction`](#classessubject_sdnadecoratorsperspectiveactionmd)

##### Defined in

[subject/SDNADecorators.ts:12](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L12)

___

#### hasLink

▸ **hasLink**(`predicate`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `predicate` | `string` |

##### Returns

`string`

##### Defined in

[subject/SDNADecorators.ts:21](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L21)

___

#### instanceQuery

▸ **instanceQuery**(`options?`): <T\>(`target`: `T`, `key`: keyof `T`, `descriptor`: `PropertyDescriptor`) => `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `options?` | `InstanceQueryParams` |

##### Returns

`fn`

▸ <`T`\>(`target`, `key`, `descriptor`): `void`

###### Type parameters

| Name |
| :------ |
| `T` |

###### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `T` |
| `key` | keyof `T` |
| `descriptor` | `PropertyDescriptor` |

###### Returns

`void`

##### Defined in

[subject/SDNADecorators.ts:30](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L30)

___

#### makeRandomPrologAtom

▸ **makeRandomPrologAtom**(`length`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `length` | `number` |

##### Returns

`string`

##### Defined in

[subject/SDNADecorators.ts:142](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L142)

___

#### subjectCollection

▸ **subjectCollection**(`opts`): <T\>(`target`: `T`, `key`: keyof `T`) => `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `opts` | `CollectionOptions` |

##### Returns

`fn`

▸ <`T`\>(`target`, `key`): `void`

###### Type parameters

| Name |
| :------ |
| `T` |

###### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `T` |
| `key` | keyof `T` |

###### Returns

`void`

##### Defined in

[subject/SDNADecorators.ts:129](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L129)

___

#### subjectFlag

▸ **subjectFlag**(`opts`): <T\>(`target`: `T`, `key`: keyof `T`) => `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `opts` | `FlagOptions` |

##### Returns

`fn`

▸ <`T`\>(`target`, `key`): `void`

###### Type parameters

| Name |
| :------ |
| `T` |

###### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `T` |
| `key` | keyof `T` |

###### Returns

`void`

##### Defined in

[subject/SDNADecorators.ts:101](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L101)

___

#### subjectProperty

▸ **subjectProperty**(`opts`): <T\>(`target`: `T`, `key`: keyof `T`) => `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `opts` | `PropertyOptions` |

##### Returns

`fn`

▸ <`T`\>(`target`, `key`): `void`

###### Type parameters

| Name |
| :------ |
| `T` |

###### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `T` |
| `key` | keyof `T` |

###### Returns

`void`

##### Defined in

[subject/SDNADecorators.ts:82](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SDNADecorators.ts#L82)


<a name="modulessubject_subjectmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / subject/Subject

## Module: subject/Subject

### Table of contents

#### Classes

- [Subject](#classessubject_subjectsubjectmd)


<a name="modulessubject_subjectentitymd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / subject/SubjectEntity

## Module: subject/SubjectEntity

### Table of contents

#### Classes

- [SubjectEntity](#classessubject_subjectentitysubjectentitymd)

#### Type Aliases

- [QueryPartialEntity](#querypartialentity)
- [SubjectArray](#subjectarray)

### Type Aliases

#### QueryPartialEntity

Ƭ **QueryPartialEntity**<`T`\>: { [P in keyof T]?: T[P] \| Function }

##### Type parameters

| Name |
| :------ |
| `T` |

##### Defined in

[subject/SubjectEntity.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L15)

___

#### SubjectArray

Ƭ **SubjectArray**<`T`\>: `T`[] \| { `action`: ``"setter"`` \| ``"adder"`` ; `value`: `T`[]  }

##### Type parameters

| Name |
| :------ |
| `T` |

##### Defined in

[subject/SubjectEntity.ts:210](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/SubjectEntity.ts#L210)


<a name="modulessubject_utilmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / subject/util

## Module: subject/util

### Table of contents

#### Functions

- [capitalize](#capitalize)
- [collectionAdderToName](#collectionaddertoname)
- [collectionSetterToName](#collectionsettertoname)
- [collectionToAdderName](#collectiontoaddername)
- [collectionToSetterName](#collectiontosettername)
- [pluralToSingular](#pluraltosingular)
- [propertyNameToSetterName](#propertynametosettername)
- [setterNameToPropertyName](#setternametopropertyname)
- [singularToPlural](#singulartoplural)
- [stringifyObjectLiteral](#stringifyobjectliteral)

### Functions

#### capitalize

▸ **capitalize**(`str`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `str` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:1](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L1)

___

#### collectionAdderToName

▸ **collectionAdderToName**(`adderName`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `adderName` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:39](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L39)

___

#### collectionSetterToName

▸ **collectionSetterToName**(`adderName`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `adderName` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:45](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L45)

___

#### collectionToAdderName

▸ **collectionToAdderName**(`collection`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `collection` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:34](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L34)

___

#### collectionToSetterName

▸ **collectionToSetterName**(`collection`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `collection` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:52](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L52)

___

#### pluralToSingular

▸ **pluralToSingular**(`plural`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `plural` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:23](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L23)

___

#### propertyNameToSetterName

▸ **propertyNameToSetterName**(`property`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `property` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L6)

___

#### setterNameToPropertyName

▸ **setterNameToPropertyName**(`setter`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `setter` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:11](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L11)

___

#### singularToPlural

▸ **singularToPlural**(`singular`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `singular` | `string` |

##### Returns

`string`

##### Defined in

[subject/util.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L15)

___

#### stringifyObjectLiteral

▸ **stringifyObjectLiteral**(`obj`): `any`

##### Parameters

| Name | Type |
| :------ | :------ |
| `obj` | `any` |

##### Returns

`any`

##### Defined in

[subject/util.ts:57](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/subject/util.ts#L57)


<a name="modulestypedefsmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / typeDefs

## Module: typeDefs

### Table of contents

#### Variables

- [typeDefsString](#typedefsstring)

### Variables

#### typeDefsString

• `Const` **typeDefsString**: ``""``

##### Defined in

[typeDefs.ts:6](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/typeDefs.ts#L6)


<a name="modulesunwrapapolloresultmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / unwrapApolloResult

## Module: unwrapApolloResult

### Table of contents

#### Functions

- [default](#default)

### Functions

#### default

▸ **default**(`result`): `any`

##### Parameters

| Name | Type |
| :------ | :------ |
| `result` | `ApolloQueryResult`<`any`\> \| `FetchResult`<`any`, `Record`<`string`, `any`\>, `Record`<`string`, `any`\>\> |

##### Returns

`any`

##### Defined in

[unwrapApolloResult.ts:3](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/unwrapApolloResult.ts#L3)


<a name="modulesutilsmd"></a>

[@perspect3vism/ad4m](#readmemd) / [Exports](#modulesmd) / utils

## Module: utils

### Table of contents

#### Functions

- [capSentence](#capsentence)
- [formatList](#formatlist)

### Functions

#### capSentence

▸ **capSentence**(`cap`): `string`

##### Parameters

| Name | Type |
| :------ | :------ |
| `cap` | `any` |

##### Returns

`string`

##### Defined in

[utils.ts:15](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/utils.ts#L15)

___

#### formatList

▸ **formatList**(`list`): `any`

##### Parameters

| Name | Type |
| :------ | :------ |
| `list` | `any` |

##### Returns

`any`

##### Defined in

[utils.ts:1](https://github.com/perspect3vism/ad4m/blob/9216581c/core/src/utils.ts#L1)
