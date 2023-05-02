[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [neighbourhood/NeighbourhoodResolver](../modules/neighbourhood_NeighbourhoodResolver.md) / default

# Class: default

[neighbourhood/NeighbourhoodResolver](../modules/neighbourhood_NeighbourhoodResolver.md).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

## Table of contents

### Constructors

- [constructor](neighbourhood_NeighbourhoodResolver.default.md#constructor)

### Methods

- [neighbourhoodHasTelepresenceAdapter](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodhastelepresenceadapter)
- [neighbourhoodJoinFromUrl](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodjoinfromurl)
- [neighbourhoodOnlineAgents](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodonlineagents)
- [neighbourhoodOtherAgents](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodotheragents)
- [neighbourhoodPublishFromPerspective](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodpublishfromperspective)
- [neighbourhoodSendBroadcast](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodsendbroadcast)
- [neighbourhoodSendSignal](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodsendsignal)
- [neighbourhoodSetOnlineStatus](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodsetonlinestatus)
- [neighbourhoodSignal](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodsignal)

## Constructors

### constructor

• **new default**()

## Methods

### neighbourhoodHasTelepresenceAdapter

▸ **neighbourhoodHasTelepresenceAdapter**(`perspectiveUUID`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

#### Returns

`boolean`

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:53](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L53)

___

### neighbourhoodJoinFromUrl

▸ **neighbourhoodJoinFromUrl**(`url`, `pubSub`): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |
| `pubSub` | `any` |

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:37](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L37)

___

### neighbourhoodOnlineAgents

▸ **neighbourhoodOnlineAgents**(`perspectiveUUID`): [`OnlineAgent`](language_Language.OnlineAgent.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

#### Returns

[`OnlineAgent`](language_Language.OnlineAgent.md)[]

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:58](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L58)

___

### neighbourhoodOtherAgents

▸ **neighbourhoodOtherAgents**(`perspectiveUUID`): `string`[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |

#### Returns

`string`[]

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:48](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L48)

___

### neighbourhoodPublishFromPerspective

▸ **neighbourhoodPublishFromPerspective**(`perspectiveUUID`, `linkLanguage`, `meta`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `linkLanguage` | `string` |
| `meta` | [`PerspectiveInput`](perspectives_Perspective.PerspectiveInput.md) |

#### Returns

`string`

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:28](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L28)

___

### neighbourhoodSendBroadcast

▸ **neighbourhoodSendBroadcast**(`perspectiveUUID`, `signal`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `signal` | [`PerspectiveInput`](perspectives_Perspective.PerspectiveInput.md) |

#### Returns

`boolean`

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:76](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L76)

___

### neighbourhoodSendSignal

▸ **neighbourhoodSendSignal**(`perspectiveUUID`, `recipient`, `signal`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `recipient` | `string` |
| `signal` | [`PerspectiveInput`](perspectives_Perspective.PerspectiveInput.md) |

#### Returns

`boolean`

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:71](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L71)

___

### neighbourhoodSetOnlineStatus

▸ **neighbourhoodSetOnlineStatus**(`perspectiveUUID`, `status`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `status` | [`PerspectiveInput`](perspectives_Perspective.PerspectiveInput.md) |

#### Returns

`boolean`

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:66](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L66)

___

### neighbourhoodSignal

▸ **neighbourhoodSignal**(`pID`): [`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `pID` | `string` |

#### Returns

[`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:81](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/neighbourhood/NeighbourhoodResolver.ts#L81)
