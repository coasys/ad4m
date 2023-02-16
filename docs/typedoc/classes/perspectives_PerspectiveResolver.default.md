[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [perspectives/PerspectiveResolver](../modules/perspectives_PerspectiveResolver.md) / default

# Class: default

[perspectives/PerspectiveResolver](../modules/perspectives_PerspectiveResolver.md).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

## Table of contents

### Constructors

- [constructor](perspectives_PerspectiveResolver.default.md#constructor)

### Methods

- [perspective](perspectives_PerspectiveResolver.default.md#perspective)
- [perspectiveAdd](perspectives_PerspectiveResolver.default.md#perspectiveadd)
- [perspectiveAddLink](perspectives_PerspectiveResolver.default.md#perspectiveaddlink)
- [perspectiveAddLinkExpression](perspectives_PerspectiveResolver.default.md#perspectiveaddlinkexpression)
- [perspectiveAddLinks](perspectives_PerspectiveResolver.default.md#perspectiveaddlinks)
- [perspectiveAdded](perspectives_PerspectiveResolver.default.md#perspectiveadded)
- [perspectiveLinkAdded](perspectives_PerspectiveResolver.default.md#perspectivelinkadded)
- [perspectiveLinkMutations](perspectives_PerspectiveResolver.default.md#perspectivelinkmutations)
- [perspectiveLinkRemoved](perspectives_PerspectiveResolver.default.md#perspectivelinkremoved)
- [perspectiveLinkUpdated](perspectives_PerspectiveResolver.default.md#perspectivelinkupdated)
- [perspectivePublishSnapshot](perspectives_PerspectiveResolver.default.md#perspectivepublishsnapshot)
- [perspectiveQueryLinks](perspectives_PerspectiveResolver.default.md#perspectivequerylinks)
- [perspectiveQueryProlog](perspectives_PerspectiveResolver.default.md#perspectivequeryprolog)
- [perspectiveRemove](perspectives_PerspectiveResolver.default.md#perspectiveremove)
- [perspectiveRemoveLink](perspectives_PerspectiveResolver.default.md#perspectiveremovelink)
- [perspectiveRemoveLinks](perspectives_PerspectiveResolver.default.md#perspectiveremovelinks)
- [perspectiveRemoved](perspectives_PerspectiveResolver.default.md#perspectiveremoved)
- [perspectiveSnapshot](perspectives_PerspectiveResolver.default.md#perspectivesnapshot)
- [perspectiveUpdate](perspectives_PerspectiveResolver.default.md#perspectiveupdate)
- [perspectiveUpdateLink](perspectives_PerspectiveResolver.default.md#perspectiveupdatelink)
- [perspectiveUpdated](perspectives_PerspectiveResolver.default.md#perspectiveupdated)
- [perspectives](perspectives_PerspectiveResolver.default.md#perspectives)

## Constructors

### constructor

• **new default**()

## Methods

### perspective

▸ **perspective**(`uuid`): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:44](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L44)

___

### perspectiveAdd

▸ **perspectiveAdd**(`name`, `pubSub`): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `name` | `string` |
| `pubSub` | `any` |

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:69](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L69)

___

### perspectiveAddLink

▸ **perspectiveAddLink**(`uuid`, `link`, `pubSub`): [`LinkExpression`](links_Links.LinkExpression.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkInput`](links_Links.LinkInput.md) |
| `pubSub` | `any` |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:90](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L90)

___

### perspectiveAddLinkExpression

▸ **perspectiveAddLinkExpression**(`uuid`, `link`, `pubSub`): [`LinkExpression`](links_Links.LinkExpression.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |
| `pubSub` | `any` |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:147](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L147)

___

### perspectiveAddLinks

▸ **perspectiveAddLinks**(`uuid`, `links`, `pubSub`): [`LinkExpression`](links_Links.LinkExpression.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `links` | [`LinkInput`](links_Links.LinkInput.md)[] |
| `pubSub` | `any` |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)[]

#### Defined in

[perspectives/PerspectiveResolver.ts:102](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L102)

___

### perspectiveAdded

▸ **perspectiveAdded**(): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:172](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L172)

___

### perspectiveLinkAdded

▸ **perspectiveLinkAdded**(`uuid`): [`LinkExpression`](links_Links.LinkExpression.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:188](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L188)

___

### perspectiveLinkMutations

▸ **perspectiveLinkMutations**(`uuid`, `mutations`, `pubSub`): [`LinkExpressionMutations`](links_Links.LinkExpressionMutations.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `mutations` | [`LinkMutations`](links_Links.LinkMutations.md) |
| `pubSub` | `any` |

#### Returns

[`LinkExpressionMutations`](links_Links.LinkExpressionMutations.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:140](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L140)

___

### perspectiveLinkRemoved

▸ **perspectiveLinkRemoved**(`uuid`): [`LinkExpression`](links_Links.LinkExpression.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:193](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L193)

___

### perspectiveLinkUpdated

▸ **perspectiveLinkUpdated**(`uuid`): [`LinkExpressionUpdated`](links_Links.LinkExpressionUpdated.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

[`LinkExpressionUpdated`](links_Links.LinkExpressionUpdated.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:198](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L198)

___

### perspectivePublishSnapshot

▸ **perspectivePublishSnapshot**(`uuid`): `String`

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

`String`

#### Defined in

[perspectives/PerspectiveResolver.ts:54](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L54)

___

### perspectiveQueryLinks

▸ **perspectiveQueryLinks**(`uuid`, `query`): [`LinkExpression`](links_Links.LinkExpression.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `query` | [`LinkQuery`](perspectives_LinkQuery.LinkQuery.md) |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)[]

#### Defined in

[perspectives/PerspectiveResolver.ts:59](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L59)

___

### perspectiveQueryProlog

▸ **perspectiveQueryProlog**(`uuid`, `query`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `query` | `String` |

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveResolver.ts:64](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L64)

___

### perspectiveRemove

▸ **perspectiveRemove**(`uuid`, `pubSub`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `pubSub` | `any` |

#### Returns

`boolean`

#### Defined in

[perspectives/PerspectiveResolver.ts:83](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L83)

___

### perspectiveRemoveLink

▸ **perspectiveRemoveLink**(`uuid`, `link`, `pubSub`): `Boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |
| `pubSub` | `any` |

#### Returns

`Boolean`

#### Defined in

[perspectives/PerspectiveResolver.ts:166](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L166)

___

### perspectiveRemoveLinks

▸ **perspectiveRemoveLinks**(`uuid`, `links`, `pubSub`): [`LinkExpression`](links_Links.LinkExpression.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `links` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md)[] |
| `pubSub` | `any` |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)[]

#### Defined in

[perspectives/PerspectiveResolver.ts:121](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L121)

___

### perspectiveRemoved

▸ **perspectiveRemoved**(): `string`

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveResolver.ts:183](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L183)

___

### perspectiveSnapshot

▸ **perspectiveSnapshot**(`uuid`): [`Perspective`](perspectives_Perspective.Perspective.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

[`Perspective`](perspectives_Perspective.Perspective.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:49](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L49)

___

### perspectiveUpdate

▸ **perspectiveUpdate**(`uuid`, `name`, `pubSub`): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `name` | `string` |
| `pubSub` | `any` |

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:76](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L76)

___

### perspectiveUpdateLink

▸ **perspectiveUpdateLink**(`uuid`, `oldlink`, `newlink`, `pubSub`): [`LinkExpression`](links_Links.LinkExpression.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `oldlink` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |
| `newlink` | [`LinkInput`](links_Links.LinkInput.md) |
| `pubSub` | `any` |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:153](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L153)

___

### perspectiveUpdated

▸ **perspectiveUpdated**(): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:178](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L178)

___

### perspectives

▸ **perspectives**(): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)[]

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)[]

#### Defined in

[perspectives/PerspectiveResolver.ts:30](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveResolver.ts#L30)
