[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [agent/AgentClient](../modules/agent_AgentClient.md) / AgentClient

# Class: AgentClient

[agent/AgentClient](../modules/agent_AgentClient.md).AgentClient

Provides access to all functions regarding the local agent,
such as generating, locking, unlocking, importing the DID keystore,
as well as updating the publicly shared Agent expression.

## Table of contents

### Constructors

- [constructor](agent_AgentClient.AgentClient.md#constructor)

### Properties

- [#agentStatusChangedCallbacks](agent_AgentClient.AgentClient.md##agentstatuschangedcallbacks)
- [#apolloClient](agent_AgentClient.AgentClient.md##apolloclient)
- [#updatedCallbacks](agent_AgentClient.AgentClient.md##updatedcallbacks)

### Methods

- [addAgentStatusChangedListener](agent_AgentClient.AgentClient.md#addagentstatuschangedlistener)
- [addEntanglementProofs](agent_AgentClient.AgentClient.md#addentanglementproofs)
- [addUpdatedListener](agent_AgentClient.AgentClient.md#addupdatedlistener)
- [byDID](agent_AgentClient.AgentClient.md#bydid)
- [deleteEntanglementProofs](agent_AgentClient.AgentClient.md#deleteentanglementproofs)
- [entanglementProofPreFlight](agent_AgentClient.AgentClient.md#entanglementproofpreflight)
- [generate](agent_AgentClient.AgentClient.md#generate)
- [generateJwt](agent_AgentClient.AgentClient.md#generatejwt)
- [getEntanglementProofs](agent_AgentClient.AgentClient.md#getentanglementproofs)
- [import](agent_AgentClient.AgentClient.md#import)
- [isLocked](agent_AgentClient.AgentClient.md#islocked)
- [lock](agent_AgentClient.AgentClient.md#lock)
- [me](agent_AgentClient.AgentClient.md#me)
- [mutatePublicPerspective](agent_AgentClient.AgentClient.md#mutatepublicperspective)
- [permitCapability](agent_AgentClient.AgentClient.md#permitcapability)
- [requestCapability](agent_AgentClient.AgentClient.md#requestcapability)
- [signMessage](agent_AgentClient.AgentClient.md#signmessage)
- [status](agent_AgentClient.AgentClient.md#status)
- [subscribeAgentStatusChanged](agent_AgentClient.AgentClient.md#subscribeagentstatuschanged)
- [subscribeAgentUpdated](agent_AgentClient.AgentClient.md#subscribeagentupdated)
- [unlock](agent_AgentClient.AgentClient.md#unlock)
- [updateDirectMessageLanguage](agent_AgentClient.AgentClient.md#updatedirectmessagelanguage)
- [updatePublicPerspective](agent_AgentClient.AgentClient.md#updatepublicperspective)

## Constructors

### constructor

• **new AgentClient**(`client`, `subscribe?`)

#### Parameters

| Name | Type | Default value |
| :------ | :------ | :------ |
| `client` | `ApolloClient`<`any`\> | `undefined` |
| `subscribe` | `boolean` | `true` |

#### Defined in

[agent/AgentClient.ts:66](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L66)

## Properties

### #agentStatusChangedCallbacks

• `Private` **#agentStatusChangedCallbacks**: [`AgentStatusChangedCallback`](../modules/agent_AgentClient.md#agentstatuschangedcallback)[]

#### Defined in

[agent/AgentClient.ts:64](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L64)

___

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[agent/AgentClient.ts:62](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L62)

___

### #updatedCallbacks

• `Private` **#updatedCallbacks**: [`AgentUpdatedCallback`](../modules/agent_AgentClient.md#agentupdatedcallback)[]

#### Defined in

[agent/AgentClient.ts:63](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L63)

## Methods

### addAgentStatusChangedListener

▸ **addAgentStatusChangedListener**(`listener`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `listener` | `any` |

#### Returns

`void`

#### Defined in

[agent/AgentClient.ts:306](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L306)

___

### addEntanglementProofs

▸ **addEntanglementProofs**(`proofs`): `Promise`<[`EntanglementProof`](agent_Agent.EntanglementProof.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `proofs` | [`EntanglementProofInput`](agent_Agent.EntanglementProofInput.md)[] |

#### Returns

`Promise`<[`EntanglementProof`](agent_Agent.EntanglementProof.md)[]\>

#### Defined in

[agent/AgentClient.ts:239](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L239)

___

### addUpdatedListener

▸ **addUpdatedListener**(`listener`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `listener` | `any` |

#### Returns

`void`

#### Defined in

[agent/AgentClient.ts:286](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L286)

___

### byDID

▸ **byDID**(`did`): `Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |

#### Returns

`Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Defined in

[agent/AgentClient.ts:160](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L160)

___

### deleteEntanglementProofs

▸ **deleteEntanglementProofs**(`proofs`): `Promise`<[`EntanglementProof`](agent_Agent.EntanglementProof.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `proofs` | [`EntanglementProofInput`](agent_Agent.EntanglementProofInput.md)[] |

#### Returns

`Promise`<[`EntanglementProof`](agent_Agent.EntanglementProof.md)[]\>

#### Defined in

[agent/AgentClient.ts:251](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L251)

___

### entanglementProofPreFlight

▸ **entanglementProofPreFlight**(`deviceKey`, `deviceKeyType`): `Promise`<[`EntanglementProof`](agent_Agent.EntanglementProof.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `deviceKey` | `string` |
| `deviceKeyType` | `string` |

#### Returns

`Promise`<[`EntanglementProof`](agent_Agent.EntanglementProof.md)\>

#### Defined in

[agent/AgentClient.ts:274](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L274)

___

### generate

▸ **generate**(`passphrase`): `Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |

#### Returns

`Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Defined in

[agent/AgentClient.ts:103](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L103)

___

### generateJwt

▸ **generateJwt**(`requestId`, `rand`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |
| `rand` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[agent/AgentClient.ts:346](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L346)

___

### getEntanglementProofs

▸ **getEntanglementProofs**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[agent/AgentClient.ts:263](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L263)

___

### import

▸ **import**(`args`): `Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `args` | [`InitializeArgs`](../interfaces/agent_AgentClient.InitializeArgs.md) |

#### Returns

`Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Defined in

[agent/AgentClient.ts:117](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L117)

___

### isLocked

▸ **isLocked**(): `Promise`<`boolean`\>

#### Returns

`Promise`<`boolean`\>

#### Defined in

[agent/AgentClient.ts:356](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L356)

___

### lock

▸ **lock**(`passphrase`): `Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |

#### Returns

`Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Defined in

[agent/AgentClient.ts:135](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L135)

___

### me

▸ **me**(): `Promise`<[`Agent`](agent_Agent.Agent.md)\>

Returns the Agent expression of the local agent as it is shared
publicly via the AgentLanguage.

I.e. this is the users profile.

#### Returns

`Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Defined in

[agent/AgentClient.ts:83](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L83)

___

### mutatePublicPerspective

▸ **mutatePublicPerspective**(`mutations`): `Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `mutations` | [`LinkMutations`](links_Links.LinkMutations.md) |

#### Returns

`Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Defined in

[agent/AgentClient.ts:195](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L195)

___

### permitCapability

▸ **permitCapability**(`auth`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `auth` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[agent/AgentClient.ts:336](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L336)

___

### requestCapability

▸ **requestCapability**(`appName`, `appDesc`, `appUrl`, `capabilities`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `appName` | `string` |
| `appDesc` | `string` |
| `appUrl` | `string` |
| `capabilities` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[agent/AgentClient.ts:326](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L326)

___

### signMessage

▸ **signMessage**(`message`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `message` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[agent/AgentClient.ts:365](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L365)

___

### status

▸ **status**(): `Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Returns

`Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Defined in

[agent/AgentClient.ts:92](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L92)

___

### subscribeAgentStatusChanged

▸ **subscribeAgentStatusChanged**(): `void`

#### Returns

`void`

#### Defined in

[agent/AgentClient.ts:310](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L310)

___

### subscribeAgentUpdated

▸ **subscribeAgentUpdated**(): `void`

#### Returns

`void`

#### Defined in

[agent/AgentClient.ts:290](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L290)

___

### unlock

▸ **unlock**(`passphrase`): `Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |

#### Returns

`Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Defined in

[agent/AgentClient.ts:147](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L147)

___

### updateDirectMessageLanguage

▸ **updateDirectMessageLanguage**(`directMessageLanguage`): `Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `directMessageLanguage` | `string` |

#### Returns

`Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Defined in

[agent/AgentClient.ts:224](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L224)

___

### updatePublicPerspective

▸ **updatePublicPerspective**(`perspective`): `Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveInput`](perspectives_Perspective.PerspectiveInput.md) |

#### Returns

`Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Defined in

[agent/AgentClient.ts:172](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/agent/AgentClient.ts#L172)
