[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [neighbourhood/NeighbourhoodProxy](../modules/neighbourhood_NeighbourhoodProxy.md) / NeighbourhoodProxy

# Class: NeighbourhoodProxy

[neighbourhood/NeighbourhoodProxy](../modules/neighbourhood_NeighbourhoodProxy.md).NeighbourhoodProxy

## Table of contents

### Constructors

- [constructor](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md#constructor)

### Properties

- [#client](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md##client)
- [#pID](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md##pid)

### Methods

- [addSignalHandler](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md#addsignalhandler)
- [hasTelepresenceAdapter](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md#hastelepresenceadapter)
- [onlineAgents](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md#onlineagents)
- [otherAgents](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md#otheragents)
- [sendBroadcast](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md#sendbroadcast)
- [sendSignal](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md#sendsignal)
- [setOnlineStatus](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md#setonlinestatus)

## Constructors

### constructor

• **new NeighbourhoodProxy**(`client`, `pID`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `client` | [`NeighbourhoodClient`](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md) |
| `pID` | `string` |

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:10](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L10)

## Properties

### #client

• `Private` **#client**: [`NeighbourhoodClient`](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md)

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:7](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L7)

___

### #pID

• `Private` **#pID**: `string`

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:8](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L8)

## Methods

### addSignalHandler

▸ **addSignalHandler**(`handler`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `handler` | (`payload`: [`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)) => `void` |

#### Returns

`Promise`<`void`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:39](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L39)

___

### hasTelepresenceAdapter

▸ **hasTelepresenceAdapter**(): `Promise`<`boolean`\>

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:19](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L19)

___

### onlineAgents

▸ **onlineAgents**(): `Promise`<[`OnlineAgent`](language_Language.OnlineAgent.md)[]\>

#### Returns

`Promise`<[`OnlineAgent`](language_Language.OnlineAgent.md)[]\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:23](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L23)

___

### otherAgents

▸ **otherAgents**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:15](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L15)

___

### sendBroadcast

▸ **sendBroadcast**(`payload`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:35](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L35)

___

### sendSignal

▸ **sendSignal**(`remoteAgentDid`, `payload`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `remoteAgentDid` | `string` |
| `payload` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:31](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L31)

___

### setOnlineStatus

▸ **setOnlineStatus**(`status`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodProxy.ts:27](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodProxy.ts#L27)
