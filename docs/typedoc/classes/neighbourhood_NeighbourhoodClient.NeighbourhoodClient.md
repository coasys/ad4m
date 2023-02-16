[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [neighbourhood/NeighbourhoodClient](../modules/neighbourhood_NeighbourhoodClient.md) / NeighbourhoodClient

# Class: NeighbourhoodClient

[neighbourhood/NeighbourhoodClient](../modules/neighbourhood_NeighbourhoodClient.md).NeighbourhoodClient

## Table of contents

### Constructors

- [constructor](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#constructor)

### Properties

- [#apolloClient](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md##apolloclient)

### Methods

- [addSignalHandler](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#addsignalhandler)
- [hasTelepresenceAdapter](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#hastelepresenceadapter)
- [joinFromUrl](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#joinfromurl)
- [onlineAgents](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#onlineagents)
- [otherAgents](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#otheragents)
- [publishFromPerspective](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#publishfromperspective)
- [sendBroadcast](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#sendbroadcast)
- [sendSignal](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#sendsignal)
- [setOnlineStatus](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#setonlinestatus)

## Constructors

### constructor

• **new NeighbourhoodClient**(`client`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `client` | `ApolloClient`<`any`\> |

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:13](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L13)

## Properties

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:11](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L11)

## Methods

### addSignalHandler

▸ **addSignalHandler**(`perspectiveUUID`, `handler`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `handler` | [`TelepresenceSignalCallback`](../modules/language_Language.md#telepresencesignalcallback) |

#### Returns

`Promise`<`void`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:164](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L164)

___

### hasTelepresenceAdapter

▸ **hasTelepresenceAdapter**(`perspectiveUUID`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:76](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L76)

___

### joinFromUrl

▸ **joinFromUrl**(`url`): `Promise`<[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

`Promise`<[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:39](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L39)

___

### onlineAgents

▸ **onlineAgents**(`perspectiveUUID`): `Promise`<[`OnlineAgent`](language_Language.OnlineAgent.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

#### Returns

`Promise`<[`OnlineAgent`](language_Language.OnlineAgent.md)[]\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:86](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L86)

___

### otherAgents

▸ **otherAgents**(`perspectiveUUID`): `Promise`<`string`[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:66](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L66)

___

### publishFromPerspective

▸ **publishFromPerspective**(`perspectiveUUID`, `linkLanguage`, `meta`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `linkLanguage` | `string` |
| `meta` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`string`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:17](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L17)

___

### sendBroadcast

▸ **sendBroadcast**(`perspectiveUUID`, `payload`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `payload` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:147](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L147)

___

### sendSignal

▸ **sendSignal**(`perspectiveUUID`, `remoteAgentDid`, `payload`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `remoteAgentDid` | `string` |
| `payload` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:128](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L128)

___

### setOnlineStatus

▸ **setOnlineStatus**(`perspectiveUUID`, `status`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `status` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:111](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodClient.ts#L111)
