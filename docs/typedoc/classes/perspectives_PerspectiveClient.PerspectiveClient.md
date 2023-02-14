[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [perspectives/PerspectiveClient](../modules/perspectives_PerspectiveClient.md) / PerspectiveClient

# Class: PerspectiveClient

[perspectives/PerspectiveClient](../modules/perspectives_PerspectiveClient.md).PerspectiveClient

## Table of contents

### Constructors

- [constructor](perspectives_PerspectiveClient.PerspectiveClient.md#constructor)

### Properties

- [#apolloClient](perspectives_PerspectiveClient.PerspectiveClient.md##apolloclient)
- [#expressionClient](perspectives_PerspectiveClient.PerspectiveClient.md##expressionclient)
- [#neighbourhoodClient](perspectives_PerspectiveClient.PerspectiveClient.md##neighbourhoodclient)
- [#perspectiveAddedCallbacks](perspectives_PerspectiveClient.PerspectiveClient.md##perspectiveaddedcallbacks)
- [#perspectiveRemovedCallbacks](perspectives_PerspectiveClient.PerspectiveClient.md##perspectiveremovedcallbacks)
- [#perspectiveUpdatedCallbacks](perspectives_PerspectiveClient.PerspectiveClient.md##perspectiveupdatedcallbacks)

### Methods

- [add](perspectives_PerspectiveClient.PerspectiveClient.md#add)
- [addLink](perspectives_PerspectiveClient.PerspectiveClient.md#addlink)
- [addLinkExpression](perspectives_PerspectiveClient.PerspectiveClient.md#addlinkexpression)
- [addLinks](perspectives_PerspectiveClient.PerspectiveClient.md#addlinks)
- [addPerspectiveAddedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectiveaddedlistener)
- [addPerspectiveLinkAddedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectivelinkaddedlistener)
- [addPerspectiveLinkRemovedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectivelinkremovedlistener)
- [addPerspectiveLinkUpdatedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectivelinkupdatedlistener)
- [addPerspectiveRemovedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectiveremovedlistener)
- [addPerspectiveUpdatedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectiveupdatedlistener)
- [all](perspectives_PerspectiveClient.PerspectiveClient.md#all)
- [byUUID](perspectives_PerspectiveClient.PerspectiveClient.md#byuuid)
- [createExpression](perspectives_PerspectiveClient.PerspectiveClient.md#createexpression)
- [getExpression](perspectives_PerspectiveClient.PerspectiveClient.md#getexpression)
- [getNeighbourhoodProxy](perspectives_PerspectiveClient.PerspectiveClient.md#getneighbourhoodproxy)
- [linkMutations](perspectives_PerspectiveClient.PerspectiveClient.md#linkmutations)
- [publishSnapshotByUUID](perspectives_PerspectiveClient.PerspectiveClient.md#publishsnapshotbyuuid)
- [queryLinks](perspectives_PerspectiveClient.PerspectiveClient.md#querylinks)
- [queryProlog](perspectives_PerspectiveClient.PerspectiveClient.md#queryprolog)
- [remove](perspectives_PerspectiveClient.PerspectiveClient.md#remove)
- [removeLink](perspectives_PerspectiveClient.PerspectiveClient.md#removelink)
- [removeLinks](perspectives_PerspectiveClient.PerspectiveClient.md#removelinks)
- [setExpressionClient](perspectives_PerspectiveClient.PerspectiveClient.md#setexpressionclient)
- [setNeighbourhoodClient](perspectives_PerspectiveClient.PerspectiveClient.md#setneighbourhoodclient)
- [snapshotByUUID](perspectives_PerspectiveClient.PerspectiveClient.md#snapshotbyuuid)
- [subscribePerspectiveAdded](perspectives_PerspectiveClient.PerspectiveClient.md#subscribeperspectiveadded)
- [subscribePerspectiveRemoved](perspectives_PerspectiveClient.PerspectiveClient.md#subscribeperspectiveremoved)
- [subscribePerspectiveUpdated](perspectives_PerspectiveClient.PerspectiveClient.md#subscribeperspectiveupdated)
- [update](perspectives_PerspectiveClient.PerspectiveClient.md#update)
- [updateLink](perspectives_PerspectiveClient.PerspectiveClient.md#updatelink)

## Constructors

### constructor

• **new PerspectiveClient**(`client`, `subscribe?`)

#### Parameters

| Name | Type | Default value |
| :------ | :------ | :------ |
| `client` | `ApolloClient`<`any`\> | `undefined` |
| `subscribe` | `boolean` | `true` |

#### Defined in

[perspectives/PerspectiveClient.ts:50](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L50)

## Properties

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[perspectives/PerspectiveClient.ts:43](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L43)

___

### #expressionClient

• `Private` `Optional` **#expressionClient**: [`ExpressionClient`](expression_ExpressionClient.ExpressionClient.md)

#### Defined in

[perspectives/PerspectiveClient.ts:47](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L47)

___

### #neighbourhoodClient

• `Private` `Optional` **#neighbourhoodClient**: [`NeighbourhoodClient`](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md)

#### Defined in

[perspectives/PerspectiveClient.ts:48](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L48)

___

### #perspectiveAddedCallbacks

• `Private` **#perspectiveAddedCallbacks**: [`PerspectiveHandleCallback`](../modules/perspectives_PerspectiveClient.md#perspectivehandlecallback)[]

#### Defined in

[perspectives/PerspectiveClient.ts:44](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L44)

___

### #perspectiveRemovedCallbacks

• `Private` **#perspectiveRemovedCallbacks**: [`UuidCallback`](../modules/perspectives_PerspectiveClient.md#uuidcallback)[]

#### Defined in

[perspectives/PerspectiveClient.ts:46](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L46)

___

### #perspectiveUpdatedCallbacks

• `Private` **#perspectiveUpdatedCallbacks**: [`PerspectiveHandleCallback`](../modules/perspectives_PerspectiveClient.md#perspectivehandlecallback)[]

#### Defined in

[perspectives/PerspectiveClient.ts:45](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L45)

## Methods

### add

▸ **add**(`name`): `Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `name` | `string` |

#### Returns

`Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:140](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L140)

___

### addLink

▸ **addLink**(`uuid`, `link`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`Link`](links_Links.Link.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:173](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L173)

___

### addLinkExpression

▸ **addLinkExpression**(`uuid`, `link`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:226](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L226)

___

### addLinks

▸ **addLinks**(`uuid`, `links`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `links` | [`Link`](links_Links.Link.md)[] |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveClient.ts:185](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L185)

___

### addPerspectiveAddedListener

▸ **addPerspectiveAddedListener**(`cb`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`PerspectiveHandleCallback`](../modules/perspectives_PerspectiveClient.md#perspectivehandlecallback) |

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:283](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L283)

___

### addPerspectiveLinkAddedListener

▸ **addPerspectiveLinkAddedListener**(`uuid`, `cb`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `String` |
| `cb` | [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback)[] |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveClient.ts:340](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L340)

___

### addPerspectiveLinkRemovedListener

▸ **addPerspectiveLinkRemovedListener**(`uuid`, `cb`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `String` |
| `cb` | [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback)[] |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveClient.ts:357](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L357)

___

### addPerspectiveLinkUpdatedListener

▸ **addPerspectiveLinkUpdatedListener**(`uuid`, `cb`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `String` |
| `cb` | [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback)[] |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveClient.ts:374](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L374)

___

### addPerspectiveRemovedListener

▸ **addPerspectiveRemovedListener**(`cb`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`UuidCallback`](../modules/perspectives_PerspectiveClient.md#uuidcallback) |

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:321](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L321)

___

### addPerspectiveUpdatedListener

▸ **addPerspectiveUpdatedListener**(`cb`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`PerspectiveHandleCallback`](../modules/perspectives_PerspectiveClient.md#perspectivehandlecallback) |

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:302](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L302)

___

### all

▸ **all**(): `Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)[]\>

#### Returns

`Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)[]\>

#### Defined in

[perspectives/PerspectiveClient.ts:71](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L71)

___

### byUUID

▸ **byUUID**(`uuid`): `Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

`Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:83](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L83)

___

### createExpression

▸ **createExpression**(`content`, `languageAddress`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `any` |
| `languageAddress` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[perspectives/PerspectiveClient.ts:278](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L278)

___

### getExpression

▸ **getExpression**(`expressionURI`): `Promise`<[`ExpressionRendered`](expression_Expression.ExpressionRendered.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `expressionURI` | `string` |

#### Returns

`Promise`<[`ExpressionRendered`](expression_Expression.ExpressionRendered.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:274](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L274)

___

### getNeighbourhoodProxy

▸ **getNeighbourhoodProxy**(`uuid`): [`NeighbourhoodProxy`](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

[`NeighbourhoodProxy`](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md)

#### Defined in

[perspectives/PerspectiveClient.ts:398](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L398)

___

### linkMutations

▸ **linkMutations**(`uuid`, `mutations`): `Promise`<[`LinkExpressionMutations`](links_Links.LinkExpressionMutations.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `mutations` | [`LinkMutations`](links_Links.LinkMutations.md) |

#### Returns

`Promise`<[`LinkExpressionMutations`](links_Links.LinkExpressionMutations.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:209](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L209)

___

### publishSnapshotByUUID

▸ **publishSnapshotByUUID**(`uuid`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[perspectives/PerspectiveClient.ts:107](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L107)

___

### queryLinks

▸ **queryLinks**(`uuid`, `query`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `query` | [`LinkQuery`](perspectives_LinkQuery.LinkQuery.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveClient.ts:117](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L117)

___

### queryProlog

▸ **queryProlog**(`uuid`, `query`): `Promise`<`any`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `query` | `string` |

#### Returns

`Promise`<`any`\>

#### Defined in

[perspectives/PerspectiveClient.ts:129](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L129)

___

### remove

▸ **remove**(`uuid`): `Promise`<{ `perspectiveRemove`: `boolean`  }\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

`Promise`<{ `perspectiveRemove`: `boolean`  }\>

#### Defined in

[perspectives/PerspectiveClient.ts:164](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L164)

___

### removeLink

▸ **removeLink**(`uuid`, `link`): `Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `link` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |

#### Returns

`Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

#### Defined in

[perspectives/PerspectiveClient.ts:261](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L261)

___

### removeLinks

▸ **removeLinks**(`uuid`, `links`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `links` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md)[] |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveClient.ts:197](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L197)

___

### setExpressionClient

▸ **setExpressionClient**(`client`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `client` | [`ExpressionClient`](expression_ExpressionClient.ExpressionClient.md) |

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:63](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L63)

___

### setNeighbourhoodClient

▸ **setNeighbourhoodClient**(`client`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `client` | [`NeighbourhoodClient`](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md) |

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:67](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L67)

___

### snapshotByUUID

▸ **snapshotByUUID**(`uuid`): `Promise`<[`Perspective`](perspectives_Perspective.Perspective.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |

#### Returns

`Promise`<[`Perspective`](perspectives_Perspective.Perspective.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:96](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L96)

___

### subscribePerspectiveAdded

▸ **subscribePerspectiveAdded**(): `void`

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:287](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L287)

___

### subscribePerspectiveRemoved

▸ **subscribePerspectiveRemoved**(): `void`

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:325](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L325)

___

### subscribePerspectiveUpdated

▸ **subscribePerspectiveUpdated**(): `void`

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:306](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L306)

___

### update

▸ **update**(`uuid`, `name`): `Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `name` | `string` |

#### Returns

`Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:152](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L152)

___

### updateLink

▸ **updateLink**(`uuid`, `oldLink`, `newLink`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `uuid` | `string` |
| `oldLink` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |
| `newLink` | [`LinkInput`](links_Links.LinkInput.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveClient.ts:238](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveClient.ts#L238)
