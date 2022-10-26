[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [perspectives/PerspectiveClient](../modules/perspectives_PerspectiveClient.md) / PerspectiveClient

# Class: PerspectiveClient

[perspectives/PerspectiveClient](../modules/perspectives_PerspectiveClient.md).PerspectiveClient

## Table of contents

### Constructors

- [constructor](perspectives_PerspectiveClient.PerspectiveClient.md#constructor)

### Properties

- [#apolloClient](perspectives_PerspectiveClient.PerspectiveClient.md##apolloclient)
- [#perspectiveAddedCallbacks](perspectives_PerspectiveClient.PerspectiveClient.md##perspectiveaddedcallbacks)
- [#perspectiveRemovedCallbacks](perspectives_PerspectiveClient.PerspectiveClient.md##perspectiveremovedcallbacks)
- [#perspectiveUpdatedCallbacks](perspectives_PerspectiveClient.PerspectiveClient.md##perspectiveupdatedcallbacks)

### Methods

- [add](perspectives_PerspectiveClient.PerspectiveClient.md#add)
- [addLink](perspectives_PerspectiveClient.PerspectiveClient.md#addlink)
- [addPerspectiveAddedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectiveaddedlistener)
- [addPerspectiveLinkAddedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectivelinkaddedlistener)
- [addPerspectiveLinkRemovedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectivelinkremovedlistener)
- [addPerspectiveRemovedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectiveremovedlistener)
- [addPerspectiveUpdatedListener](perspectives_PerspectiveClient.PerspectiveClient.md#addperspectiveupdatedlistener)
- [all](perspectives_PerspectiveClient.PerspectiveClient.md#all)
- [byUUID](perspectives_PerspectiveClient.PerspectiveClient.md#byuuid)
- [publishSnapshotByUUID](perspectives_PerspectiveClient.PerspectiveClient.md#publishsnapshotbyuuid)
- [queryLinks](perspectives_PerspectiveClient.PerspectiveClient.md#querylinks)
- [queryProlog](perspectives_PerspectiveClient.PerspectiveClient.md#queryprolog)
- [remove](perspectives_PerspectiveClient.PerspectiveClient.md#remove)
- [removeLink](perspectives_PerspectiveClient.PerspectiveClient.md#removelink)
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

[perspectives/PerspectiveClient.ts:43](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L43)

## Properties

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[perspectives/PerspectiveClient.ts:38](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L38)

___

### #perspectiveAddedCallbacks

• `Private` **#perspectiveAddedCallbacks**: [`PerspectiveHandleCallback`](../modules/perspectives_PerspectiveClient.md#perspectivehandlecallback)[]

#### Defined in

[perspectives/PerspectiveClient.ts:39](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L39)

___

### #perspectiveRemovedCallbacks

• `Private` **#perspectiveRemovedCallbacks**: [`UuidCallback`](../modules/perspectives_PerspectiveClient.md#uuidcallback)[]

#### Defined in

[perspectives/PerspectiveClient.ts:41](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L41)

___

### #perspectiveUpdatedCallbacks

• `Private` **#perspectiveUpdatedCallbacks**: [`PerspectiveHandleCallback`](../modules/perspectives_PerspectiveClient.md#perspectivehandlecallback)[]

#### Defined in

[perspectives/PerspectiveClient.ts:40](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L40)

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

[perspectives/PerspectiveClient.ts:125](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L125)

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

[perspectives/PerspectiveClient.ts:158](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L158)

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

[perspectives/PerspectiveClient.ts:206](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L206)

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

[perspectives/PerspectiveClient.ts:263](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L263)

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

[perspectives/PerspectiveClient.ts:280](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L280)

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

[perspectives/PerspectiveClient.ts:244](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L244)

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

[perspectives/PerspectiveClient.ts:225](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L225)

___

### all

▸ **all**(): `Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)[]\>

#### Returns

`Promise`<[`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)[]\>

#### Defined in

[perspectives/PerspectiveClient.ts:56](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L56)

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

[perspectives/PerspectiveClient.ts:68](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L68)

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

[perspectives/PerspectiveClient.ts:92](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L92)

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

[perspectives/PerspectiveClient.ts:102](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L102)

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

[perspectives/PerspectiveClient.ts:114](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L114)

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

[perspectives/PerspectiveClient.ts:149](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L149)

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

[perspectives/PerspectiveClient.ts:193](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L193)

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

[perspectives/PerspectiveClient.ts:81](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L81)

___

### subscribePerspectiveAdded

▸ **subscribePerspectiveAdded**(): `void`

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:210](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L210)

___

### subscribePerspectiveRemoved

▸ **subscribePerspectiveRemoved**(): `void`

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:248](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L248)

___

### subscribePerspectiveUpdated

▸ **subscribePerspectiveUpdated**(): `void`

#### Returns

`void`

#### Defined in

[perspectives/PerspectiveClient.ts:229](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L229)

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

[perspectives/PerspectiveClient.ts:137](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L137)

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

[perspectives/PerspectiveClient.ts:170](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveClient.ts#L170)
