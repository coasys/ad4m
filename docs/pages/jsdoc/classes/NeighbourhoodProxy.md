[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / NeighbourhoodProxy

# Class: NeighbourhoodProxy

## Table of contents

### Constructors

- [constructor](NeighbourhoodProxy.md#constructor)

### Properties

- [#client](NeighbourhoodProxy.md##client)
- [#pID](NeighbourhoodProxy.md##pid)

### Methods

- [addSignalHandler](NeighbourhoodProxy.md#addsignalhandler)
- [hasTelepresenceAdapter](NeighbourhoodProxy.md#hastelepresenceadapter)
- [onlineAgents](NeighbourhoodProxy.md#onlineagents)
- [otherAgents](NeighbourhoodProxy.md#otheragents)
- [removeSignalHandler](NeighbourhoodProxy.md#removesignalhandler)
- [sendBroadcast](NeighbourhoodProxy.md#sendbroadcast)
- [sendBroadcastU](NeighbourhoodProxy.md#sendbroadcastu)
- [sendSignal](NeighbourhoodProxy.md#sendsignal)
- [sendSignalU](NeighbourhoodProxy.md#sendsignalu)
- [setOnlineStatus](NeighbourhoodProxy.md#setonlinestatus)
- [setOnlineStatusU](NeighbourhoodProxy.md#setonlinestatusu)

## Constructors

### constructor

• **new NeighbourhoodProxy**(`client`, `pID`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `client` | `NeighbourhoodClient` |
| `pID` | `string` |

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:10](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L10)

## Properties

### #client

• `Private` **#client**: `NeighbourhoodClient`

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:7](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L7)

___

### #pID

• `Private` **#pID**: `string`

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:8](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L8)

## Methods

### addSignalHandler

▸ **addSignalHandler**(`handler`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `handler` | (`payload`: [`PerspectiveExpression`](PerspectiveExpression.md)) => `void` |

#### Returns

`Promise`<`void`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:51](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L51)

___

### hasTelepresenceAdapter

▸ **hasTelepresenceAdapter**(): `Promise`<`boolean`\>

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:19](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L19)

___

### onlineAgents

▸ **onlineAgents**(): `Promise`<[`OnlineAgent`](OnlineAgent.md)[]\>

#### Returns

`Promise`<[`OnlineAgent`](OnlineAgent.md)[]\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:23](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L23)

___

### otherAgents

▸ **otherAgents**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:15](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L15)

___

### removeSignalHandler

▸ **removeSignalHandler**(`handler`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `handler` | (`payload`: [`PerspectiveExpression`](PerspectiveExpression.md)) => `void` |

#### Returns

`void`

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:55](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L55)

___

### sendBroadcast

▸ **sendBroadcast**(`payload`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`Perspective`](Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:43](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L43)

___

### sendBroadcastU

▸ **sendBroadcastU**(`payload`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`PerspectiveUnsignedInput`](PerspectiveUnsignedInput.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:47](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L47)

___

### sendSignal

▸ **sendSignal**(`remoteAgentDid`, `payload`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `remoteAgentDid` | `string` |
| `payload` | [`Perspective`](Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:35](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L35)

___

### sendSignalU

▸ **sendSignalU**(`remoteAgentDid`, `payload`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `remoteAgentDid` | `string` |
| `payload` | [`PerspectiveUnsignedInput`](PerspectiveUnsignedInput.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:39](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L39)

___

### setOnlineStatus

▸ **setOnlineStatus**(`status`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`Perspective`](Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:27](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L27)

___

### setOnlineStatusU

▸ **setOnlineStatusU**(`status`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`PerspectiveUnsignedInput`](PerspectiveUnsignedInput.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:31](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/neighbourhood/NeighbourhoodProxy.ts#L31)
