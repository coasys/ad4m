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
- [getApps](agent_AgentClient.AgentClient.md#getapps)
- [getEntanglementProofs](agent_AgentClient.AgentClient.md#getentanglementproofs)
- [import](agent_AgentClient.AgentClient.md#import)
- [isLocked](agent_AgentClient.AgentClient.md#islocked)
- [lock](agent_AgentClient.AgentClient.md#lock)
- [me](agent_AgentClient.AgentClient.md#me)
- [mutatePublicPerspective](agent_AgentClient.AgentClient.md#mutatepublicperspective)
- [permitCapability](agent_AgentClient.AgentClient.md#permitcapability)
- [removeApp](agent_AgentClient.AgentClient.md#removeapp)
- [requestCapability](agent_AgentClient.AgentClient.md#requestcapability)
- [revokeToken](agent_AgentClient.AgentClient.md#revoketoken)
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

[agent/AgentClient.ts:84](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L84)

## Properties

### #agentStatusChangedCallbacks

• `Private` **#agentStatusChangedCallbacks**: [`AgentStatusChangedCallback`](../modules/agent_AgentClient.md#agentstatuschangedcallback)[]

#### Defined in

[agent/AgentClient.ts:82](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L82)

___

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[agent/AgentClient.ts:80](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L80)

___

### #updatedCallbacks

• `Private` **#updatedCallbacks**: [`AgentUpdatedCallback`](../modules/agent_AgentClient.md#agentupdatedcallback)[]

#### Defined in

[agent/AgentClient.ts:81](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L81)

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

[agent/AgentClient.ts:324](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L324)

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

[agent/AgentClient.ts:257](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L257)

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

[agent/AgentClient.ts:304](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L304)

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

[agent/AgentClient.ts:178](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L178)

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

[agent/AgentClient.ts:269](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L269)

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

[agent/AgentClient.ts:292](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L292)

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

[agent/AgentClient.ts:121](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L121)

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

[agent/AgentClient.ts:364](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L364)

___

### getApps

▸ **getApps**(): `Promise`<[`Apps`](agent_Agent.Apps.md)[]\>

#### Returns

`Promise`<[`Apps`](agent_Agent.Apps.md)[]\>

#### Defined in

[agent/AgentClient.ts:374](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L374)

___

### getEntanglementProofs

▸ **getEntanglementProofs**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[agent/AgentClient.ts:281](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L281)

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

[agent/AgentClient.ts:135](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L135)

___

### isLocked

▸ **isLocked**(): `Promise`<`boolean`\>

#### Returns

`Promise`<`boolean`\>

#### Defined in

[agent/AgentClient.ts:410](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L410)

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

[agent/AgentClient.ts:153](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L153)

___

### me

▸ **me**(): `Promise`<[`Agent`](agent_Agent.Agent.md)\>

Returns the Agent expression of the local agent as it is shared
publicly via the AgentLanguage.

I.e. this is the users profile.

#### Returns

`Promise`<[`Agent`](agent_Agent.Agent.md)\>

#### Defined in

[agent/AgentClient.ts:101](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L101)

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

[agent/AgentClient.ts:213](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L213)

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

[agent/AgentClient.ts:354](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L354)

___

### removeApp

▸ **removeApp**(`requestId`): `Promise`<[`Apps`](agent_Agent.Apps.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |

#### Returns

`Promise`<[`Apps`](agent_Agent.Apps.md)[]\>

#### Defined in

[agent/AgentClient.ts:386](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L386)

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

[agent/AgentClient.ts:344](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L344)

___

### revokeToken

▸ **revokeToken**(`requestId`): `Promise`<[`Apps`](agent_Agent.Apps.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |

#### Returns

`Promise`<[`Apps`](agent_Agent.Apps.md)[]\>

#### Defined in

[agent/AgentClient.ts:398](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L398)

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

[agent/AgentClient.ts:419](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L419)

___

### status

▸ **status**(): `Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Returns

`Promise`<[`AgentStatus`](agent_AgentStatus.AgentStatus.md)\>

#### Defined in

[agent/AgentClient.ts:110](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L110)

___

### subscribeAgentStatusChanged

▸ **subscribeAgentStatusChanged**(): `void`

#### Returns

`void`

#### Defined in

[agent/AgentClient.ts:328](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L328)

___

### subscribeAgentUpdated

▸ **subscribeAgentUpdated**(): `void`

#### Returns

`void`

#### Defined in

[agent/AgentClient.ts:308](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L308)

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

[agent/AgentClient.ts:165](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L165)

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

[agent/AgentClient.ts:242](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L242)

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

[agent/AgentClient.ts:190](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/agent/AgentClient.ts#L190)
