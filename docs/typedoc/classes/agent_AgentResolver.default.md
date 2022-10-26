[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [agent/AgentResolver](../modules/agent_AgentResolver.md) / default

# Class: default

[agent/AgentResolver](../modules/agent_AgentResolver.md).default

## Table of contents

### Constructors

- [constructor](agent_AgentResolver.default.md#constructor)

### Methods

- [agent](agent_AgentResolver.default.md#agent)
- [agentAddEntanglementProofs](agent_AgentResolver.default.md#agentaddentanglementproofs)
- [agentByDID](agent_AgentResolver.default.md#agentbydid)
- [agentDeleteEntanglementProofs](agent_AgentResolver.default.md#agentdeleteentanglementproofs)
- [agentEntanglementProofPreFlight](agent_AgentResolver.default.md#agententanglementproofpreflight)
- [agentGenerate](agent_AgentResolver.default.md#agentgenerate)
- [agentGenerateJwt](agent_AgentResolver.default.md#agentgeneratejwt)
- [agentGetEntanglementProofs](agent_AgentResolver.default.md#agentgetentanglementproofs)
- [agentImport](agent_AgentResolver.default.md#agentimport)
- [agentIsLocked](agent_AgentResolver.default.md#agentislocked)
- [agentLock](agent_AgentResolver.default.md#agentlock)
- [agentPermitCapability](agent_AgentResolver.default.md#agentpermitcapability)
- [agentRequestCapability](agent_AgentResolver.default.md#agentrequestcapability)
- [agentStatus](agent_AgentResolver.default.md#agentstatus)
- [agentStatusChanged](agent_AgentResolver.default.md#agentstatuschanged)
- [agentUnlock](agent_AgentResolver.default.md#agentunlock)
- [agentUpdateDirectMessageLanguage](agent_AgentResolver.default.md#agentupdatedirectmessagelanguage)
- [agentUpdatePublicPerspective](agent_AgentResolver.default.md#agentupdatepublicperspective)
- [agentUpdated](agent_AgentResolver.default.md#agentupdated)

## Constructors

### constructor

• **new default**()

## Methods

### agent

▸ **agent**(): [`Agent`](agent_Agent.Agent.md)

#### Returns

[`Agent`](agent_Agent.Agent.md)

#### Defined in

[agent/AgentResolver.ts:11](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L11)

___

### agentAddEntanglementProofs

▸ **agentAddEntanglementProofs**(`proofs`): [`EntanglementProof`](agent_Agent.EntanglementProof.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `proofs` | [`EntanglementProofInput`](agent_Agent.EntanglementProofInput.md)[] |

#### Returns

[`EntanglementProof`](agent_Agent.EntanglementProof.md)[]

#### Defined in

[agent/AgentResolver.ts:93](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L93)

___

### agentByDID

▸ **agentByDID**(`did`): [`Agent`](agent_Agent.Agent.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |

#### Returns

[`Agent`](agent_Agent.Agent.md)

#### Defined in

[agent/AgentResolver.ts:62](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L62)

___

### agentDeleteEntanglementProofs

▸ **agentDeleteEntanglementProofs**(`proofs`): [`EntanglementProof`](agent_Agent.EntanglementProof.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `proofs` | [`EntanglementProofInput`](agent_Agent.EntanglementProofInput.md)[] |

#### Returns

[`EntanglementProof`](agent_Agent.EntanglementProof.md)[]

#### Defined in

[agent/AgentResolver.ts:98](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L98)

___

### agentEntanglementProofPreFlight

▸ **agentEntanglementProofPreFlight**(`deviceKey`, `deviceKeyType`): [`EntanglementProof`](agent_Agent.EntanglementProof.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `deviceKey` | `string` |
| `deviceKeyType` | `string` |

#### Returns

[`EntanglementProof`](agent_Agent.EntanglementProof.md)

#### Defined in

[agent/AgentResolver.ts:108](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L108)

___

### agentGenerate

▸ **agentGenerate**(`passphrase`, `pubSub`): [`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |
| `pubSub` | `any` |

#### Returns

[`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Defined in

[agent/AgentResolver.ts:21](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L21)

___

### agentGenerateJwt

▸ **agentGenerateJwt**(`requestId`, `rand`): `String`

#### Parameters

| Name | Type |
| :------ | :------ |
| `requestId` | `string` |
| `rand` | `string` |

#### Returns

`String`

#### Defined in

[agent/AgentResolver.ts:128](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L128)

___

### agentGetEntanglementProofs

▸ **agentGetEntanglementProofs**(): [`EntanglementProof`](agent_Agent.EntanglementProof.md)[]

#### Returns

[`EntanglementProof`](agent_Agent.EntanglementProof.md)[]

#### Defined in

[agent/AgentResolver.ts:103](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L103)

___

### agentImport

▸ **agentImport**(`did`, `didDocument`, `keystore`, `passphrase`): [`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `didDocument` | `string` |
| `keystore` | `string` |
| `passphrase` | `string` |

#### Returns

[`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Defined in

[agent/AgentResolver.ts:31](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L31)

___

### agentIsLocked

▸ **agentIsLocked**(): `Boolean`

#### Returns

`Boolean`

#### Defined in

[agent/AgentResolver.ts:133](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L133)

___

### agentLock

▸ **agentLock**(`passphrase`, `pubSub`): [`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |
| `pubSub` | `any` |

#### Returns

[`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Defined in

[agent/AgentResolver.ts:41](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L41)

___

### agentPermitCapability

▸ **agentPermitCapability**(`auth`): `String`

#### Parameters

| Name | Type |
| :------ | :------ |
| `auth` | `string` |

#### Returns

`String`

#### Defined in

[agent/AgentResolver.ts:123](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L123)

___

### agentRequestCapability

▸ **agentRequestCapability**(`appName`, `appDesc`, `appUrl`, `capabilities`): `String`

#### Parameters

| Name | Type |
| :------ | :------ |
| `appName` | `string` |
| `appDesc` | `string` |
| `appUrl` | `string` |
| `capabilities` | `string` |

#### Returns

`String`

#### Defined in

[agent/AgentResolver.ts:113](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L113)

___

### agentStatus

▸ **agentStatus**(): [`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Returns

[`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Defined in

[agent/AgentResolver.ts:16](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L16)

___

### agentStatusChanged

▸ **agentStatusChanged**(): [`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Returns

[`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Defined in

[agent/AgentResolver.ts:88](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L88)

___

### agentUnlock

▸ **agentUnlock**(`passphrase`, `pubSub`): [`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `passphrase` | `string` |
| `pubSub` | `any` |

#### Returns

[`AgentStatus`](agent_AgentStatus.AgentStatus.md)

#### Defined in

[agent/AgentResolver.ts:51](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L51)

___

### agentUpdateDirectMessageLanguage

▸ **agentUpdateDirectMessageLanguage**(`directMessageLanguage`, `pubSub`): [`Agent`](agent_Agent.Agent.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `directMessageLanguage` | `string` |
| `pubSub` | `any` |

#### Returns

[`Agent`](agent_Agent.Agent.md)

#### Defined in

[agent/AgentResolver.ts:75](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L75)

___

### agentUpdatePublicPerspective

▸ **agentUpdatePublicPerspective**(`perspective`, `pubSub`): [`Agent`](agent_Agent.Agent.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveInput`](perspectives_Perspective.PerspectiveInput.md) |
| `pubSub` | `any` |

#### Returns

[`Agent`](agent_Agent.Agent.md)

#### Defined in

[agent/AgentResolver.ts:67](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L67)

___

### agentUpdated

▸ **agentUpdated**(): [`Agent`](agent_Agent.Agent.md)

#### Returns

[`Agent`](agent_Agent.Agent.md)

#### Defined in

[agent/AgentResolver.ts:83](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/AgentResolver.ts#L83)
