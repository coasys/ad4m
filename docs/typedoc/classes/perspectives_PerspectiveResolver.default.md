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
- [perspectiveAdded](perspectives_PerspectiveResolver.default.md#perspectiveadded)
- [perspectiveLinkAdded](perspectives_PerspectiveResolver.default.md#perspectivelinkadded)
- [perspectiveLinkRemoved](perspectives_PerspectiveResolver.default.md#perspectivelinkremoved)
- [perspectivePublishSnapshot](perspectives_PerspectiveResolver.default.md#perspectivepublishsnapshot)
- [perspectiveQueryLinks](perspectives_PerspectiveResolver.default.md#perspectivequerylinks)
- [perspectiveQueryProlog](perspectives_PerspectiveResolver.default.md#perspectivequeryprolog)
- [perspectiveRemove](perspectives_PerspectiveResolver.default.md#perspectiveremove)
- [perspectiveRemoveLink](perspectives_PerspectiveResolver.default.md#perspectiveremovelink)
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

[perspectives/PerspectiveResolver.ts:43](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L43)

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

[perspectives/PerspectiveResolver.ts:68](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L68)

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

[perspectives/PerspectiveResolver.ts:89](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L89)

___

### perspectiveAdded

▸ **perspectiveAdded**(): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:120](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L120)

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

[perspectives/PerspectiveResolver.ts:136](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L136)

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

[perspectives/PerspectiveResolver.ts:141](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L141)

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

[perspectives/PerspectiveResolver.ts:53](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L53)

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

[perspectives/PerspectiveResolver.ts:58](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L58)

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

[perspectives/PerspectiveResolver.ts:63](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L63)

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

[perspectives/PerspectiveResolver.ts:82](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L82)

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

[perspectives/PerspectiveResolver.ts:114](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L114)

___

### perspectiveRemoved

▸ **perspectiveRemoved**(): `string`

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveResolver.ts:131](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L131)

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

[perspectives/PerspectiveResolver.ts:48](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L48)

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

[perspectives/PerspectiveResolver.ts:75](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L75)

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

[perspectives/PerspectiveResolver.ts:101](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L101)

___

### perspectiveUpdated

▸ **perspectiveUpdated**(): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveResolver.ts:126](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L126)

___

### perspectives

▸ **perspectives**(): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)[]

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)[]

#### Defined in

[perspectives/PerspectiveResolver.ts:30](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveResolver.ts#L30)
