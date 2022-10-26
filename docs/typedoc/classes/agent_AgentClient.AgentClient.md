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

[agent/AgentClient.ts:61](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L61)

## Properties

### #agentStatusChangedCallbacks

• `Private` **#agentStatusChangedCallbacks**: [`AgentStatusChangedCallback`](../modules/agent_AgentClient.md#agentstatuschangedcallback)[]

#### Defined in

[agent/AgentClient.ts:59](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L59)

___

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[agent/AgentClient.ts:57](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L57)

___

### #updatedCallbacks

• `Private` **#updatedCallbacks**: [`AgentUpdatedCallback`](../modules/agent_AgentClient.md#agentupdatedcallback)[]

#### Defined in

[agent/AgentClient.ts:58](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L58)

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

[agent/AgentClient.ts:301](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L301)

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

[agent/AgentClient.ts:234](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L234)

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

[agent/AgentClient.ts:281](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L281)

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

[agent/AgentClient.ts:155](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L155)

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

[agent/AgentClient.ts:246](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L246)

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

[agent/AgentClient.ts:269](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L269)

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

[agent/AgentClient.ts:98](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L98)

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

[agent/AgentClient.ts:341](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L341)

___

### getEntanglementProofs

▸ **getEntanglementProofs**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[agent/AgentClient.ts:258](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L258)

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

[agent/AgentClient.ts:112](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L112)

___

### isLocked

▸ **isLocked**(): `Promise`<`boolean`\>

#### Returns

`Promise`<`boolean`\>

#### Defined in

[agent/AgentClient.ts:351](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L351)

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

[agent/AgentClient.ts:130](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L130)

___

### me

▸ **me**(): `Promise`<[`Agent`](agent_Agent.Agent.md)\>

Returns the Agent expression of the local agent as it is shared
publicly via the AgentLanguage.

I.e. this is the users profile.

#### Returns

`Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Defined in

[agent/AgentClient.ts:78](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L78)

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

[agent/AgentClient.ts:190](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L190)

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

[agent/AgentClient.ts:331](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L331)

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

[agent/AgentClient.ts:321](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L321)

___

### status

▸ **status**(): `Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Returns

`Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Defined in

[agent/AgentClient.ts:87](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L87)

___

### subscribeAgentStatusChanged

▸ **subscribeAgentStatusChanged**(): `void`

#### Returns

`void`

#### Defined in

[agent/AgentClient.ts:305](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L305)

___

### subscribeAgentUpdated

▸ **subscribeAgentUpdated**(): `void`

#### Returns

`void`

#### Defined in

[agent/AgentClient.ts:285](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L285)

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

[agent/AgentClient.ts:142](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L142)

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

[agent/AgentClient.ts:219](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L219)

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

[agent/AgentClient.ts:167](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentClient.ts#L167)
