[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [perspectives/PerspectiveProxy](../modules/perspectives_PerspectiveProxy.md) / PerspectiveProxy

# Class: PerspectiveProxy

[perspectives/PerspectiveProxy](../modules/perspectives_PerspectiveProxy.md).PerspectiveProxy

Perspective UI proxy object

Convenience object for UIs to interact with a perspective.
It is created by some of the methods in the PerspectiveClient class and includes
a reference to the PerspectiveClient object that created it.

## Table of contents

### Constructors

- [constructor](perspectives_PerspectiveProxy.PerspectiveProxy.md#constructor)

### Properties

- [#client](perspectives_PerspectiveProxy.PerspectiveProxy.md##client)
- [#executeAction](perspectives_PerspectiveProxy.PerspectiveProxy.md##executeaction)
- [#handle](perspectives_PerspectiveProxy.PerspectiveProxy.md##handle)
- [#perspectiveLinkAddedCallbacks](perspectives_PerspectiveProxy.PerspectiveProxy.md##perspectivelinkaddedcallbacks)
- [#perspectiveLinkRemovedCallbacks](perspectives_PerspectiveProxy.PerspectiveProxy.md##perspectivelinkremovedcallbacks)

### Accessors

- [name](perspectives_PerspectiveProxy.PerspectiveProxy.md#name)
- [neighbourhood](perspectives_PerspectiveProxy.PerspectiveProxy.md#neighbourhood)
- [sharedUrl](perspectives_PerspectiveProxy.PerspectiveProxy.md#sharedurl)
- [uuid](perspectives_PerspectiveProxy.PerspectiveProxy.md#uuid)

### Methods

- [add](perspectives_PerspectiveProxy.PerspectiveProxy.md#add)
- [addListener](perspectives_PerspectiveProxy.PerspectiveProxy.md#addlistener)
- [availableFlows](perspectives_PerspectiveProxy.PerspectiveProxy.md#availableflows)
- [expressionsInFlowState](perspectives_PerspectiveProxy.PerspectiveProxy.md#expressionsinflowstate)
- [flowActions](perspectives_PerspectiveProxy.PerspectiveProxy.md#flowactions)
- [flowState](perspectives_PerspectiveProxy.PerspectiveProxy.md#flowstate)
- [get](perspectives_PerspectiveProxy.PerspectiveProxy.md#get)
- [getSingleTarget](perspectives_PerspectiveProxy.PerspectiveProxy.md#getsingletarget)
- [infer](perspectives_PerspectiveProxy.PerspectiveProxy.md#infer)
- [loadSnapshot](perspectives_PerspectiveProxy.PerspectiveProxy.md#loadsnapshot)
- [remove](perspectives_PerspectiveProxy.PerspectiveProxy.md#remove)
- [removeListener](perspectives_PerspectiveProxy.PerspectiveProxy.md#removelistener)
- [runFlowAction](perspectives_PerspectiveProxy.PerspectiveProxy.md#runflowaction)
- [sdnaFlows](perspectives_PerspectiveProxy.PerspectiveProxy.md#sdnaflows)
- [setSingleTarget](perspectives_PerspectiveProxy.PerspectiveProxy.md#setsingletarget)
- [snapshot](perspectives_PerspectiveProxy.PerspectiveProxy.md#snapshot)
- [startFlow](perspectives_PerspectiveProxy.PerspectiveProxy.md#startflow)
- [update](perspectives_PerspectiveProxy.PerspectiveProxy.md#update)

## Constructors

### constructor

• **new PerspectiveProxy**(`handle`, `ad4m`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `handle` | [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md) |
| `ad4m` | [`PerspectiveClient`](perspectives_PerspectiveClient.PerspectiveClient.md) |

#### Defined in

[perspectives/PerspectiveProxy.ts:23](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L23)

## Properties

### #client

• `Private` **#client**: [`PerspectiveClient`](perspectives_PerspectiveClient.PerspectiveClient.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:18](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L18)

___

### #executeAction

• `Private` **#executeAction**: `any`

#### Defined in

[perspectives/PerspectiveProxy.ts:21](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L21)

___

### #handle

• `Private` **#handle**: [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:17](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L17)

___

### #perspectiveLinkAddedCallbacks

• `Private` **#perspectiveLinkAddedCallbacks**: [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback)[]

#### Defined in

[perspectives/PerspectiveProxy.ts:19](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L19)

___

### #perspectiveLinkRemovedCallbacks

• `Private` **#perspectiveLinkRemovedCallbacks**: [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback)[]

#### Defined in

[perspectives/PerspectiveProxy.ts:20](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L20)

## Accessors

### name

• `get` **name**(): `string`

Given name of the perspective

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveProxy.ts:70](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L70)

___

### neighbourhood

• `get` **neighbourhood**(): `void` \| [`Neighbourhood`](neighbourhood_Neighbourhood.Neighbourhood.md)

If the perspective is shared as a Neighbourhood, this is the Neighbourhood Expression

#### Returns

`void` \| [`Neighbourhood`](neighbourhood_Neighbourhood.Neighbourhood.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:80](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L80)

___

### sharedUrl

• `get` **sharedUrl**(): `string` \| `void`

If the perspective is shared as a Neighbourhood, this is the Neighbourhood URL

#### Returns

`string` \| `void`

#### Defined in

[perspectives/PerspectiveProxy.ts:75](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L75)

___

### uuid

• `get` **uuid**(): `string`

Unique ID of the perspective

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveProxy.ts:65](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L65)

## Methods

### add

▸ **add**(`link`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

Adds a link to this perspective

#### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`Link`](links_Links.Link.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:95](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L95)

___

### addListener

▸ **addListener**(`type`, `cb`): `Promise`<`void`\>

Adds a link listener

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `type` | `PerspectiveListenerTypes` | Can be 'link-added' or 'link-removed' |
| `cb` | [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback) | Callback function that is called when a link is added to the perspective |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:113](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L113)

___

### availableFlows

▸ **availableFlows**(`exprAddr`): `Promise`<`string`[]\>

Returns all Social DNA flows that can be started from the given expression

#### Parameters

| Name | Type |
| :------ | :------ |
| `exprAddr` | `string` |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:201](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L201)

___

### expressionsInFlowState

▸ **expressionsInFlowState**(`flowName`, `flowState`): `Promise`<`string`[]\>

Returns all expressions in the given state of given Social DNA flow

#### Parameters

| Name | Type |
| :------ | :------ |
| `flowName` | `string` |
| `flowState` | `number` |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:215](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L215)

___

### flowActions

▸ **flowActions**(`flowName`, `exprAddr`): `Promise`<`string`[]\>

Returns available action names, with regard to Social DNA flow and expression's flow state

#### Parameters

| Name | Type |
| :------ | :------ |
| `flowName` | `string` |
| `exprAddr` | `string` |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:227](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L227)

___

### flowState

▸ **flowState**(`flowName`, `exprAddr`): `Promise`<`number`\>

Returns the given expression's flow state with regard to given Social DNA flow

#### Parameters

| Name | Type |
| :------ | :------ |
| `flowName` | `string` |
| `exprAddr` | `string` |

#### Returns

`Promise`<`number`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:221](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L221)

___

### get

▸ **get**(`query`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

Returns all the links of this perspective that matches the LinkQuery

#### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](perspectives_LinkQuery.LinkQuery.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:85](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L85)

___

### getSingleTarget

▸ **getSingleTarget**(`query`): `Promise`<`string` \| `void`\>

Convenience function to get the target of the first link that matches the given query
This makes sense when the query is expected to return only one link
and the target of that link is what you are looking for.

Works best together with

**`Member`**

setSingelTarget()

#### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](perspectives_LinkQuery.LinkQuery.md) |

#### Returns

`Promise`<`string` \| `void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:165](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L165)

___

### infer

▸ **infer**(`query`): `Promise`<`any`\>

Runs a Prolog query on the perspective's Prolog engine

#### Parameters

| Name | Type |
| :------ | :------ |
| `query` | `string` |

#### Returns

`Promise`<`any`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:90](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L90)

___

### loadSnapshot

▸ **loadSnapshot**(`snapshot`): `Promise`<`void`\>

Take and load all the links from the given snapshot

#### Parameters

| Name | Type |
| :------ | :------ |
| `snapshot` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:146](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L146)

___

### remove

▸ **remove**(`link`): `Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

Removes a link from this perspective

#### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`LinkExpression`](links_Links.LinkExpression.md) |

#### Returns

`Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

#### Defined in

[perspectives/PerspectiveProxy.ts:105](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L105)

___

### removeListener

▸ **removeListener**(`type`, `cb`): `Promise`<`void`\>

Removes a previously added link listener

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `type` | `PerspectiveListenerTypes` | Can be 'link-added' or 'link-removed' |
| `cb` | [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback) | Callback function that is called when a link is added to the perspective |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:125](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L125)

___

### runFlowAction

▸ **runFlowAction**(`flowName`, `exprAddr`, `actionName`): `Promise`<`void`\>

Runs given Social DNA flow action

#### Parameters

| Name | Type |
| :------ | :------ |
| `flowName` | `string` |
| `exprAddr` | `string` |
| `actionName` | `string` |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:233](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L233)

___

### sdnaFlows

▸ **sdnaFlows**(): `Promise`<`string`[]\>

Returns all the Social DNA flows defined in this perspective

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:195](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L195)

___

### setSingleTarget

▸ **setSingleTarget**(`link`): `Promise`<`void`\>

Convenience function to ensure there is only one link with given source and predicate
This function will remove all links with the same source and predicate as the given link,
and then add the given link.
This ensures there is only one target for the given source and predicate.

Works best together with

**`Member`**

getSingleTarget()

#### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`Link`](links_Links.Link.md) |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:181](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L181)

___

### snapshot

▸ **snapshot**(): `Promise`<[`Perspective`](perspectives_Perspective.Perspective.md)\>

Create and return a snapshot of this perspective
A snapshot is a rendered Perspectie object that contains all the links of the perspective.

#### Returns

`Promise`<[`Perspective`](perspectives_Perspective.Perspective.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:141](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L141)

___

### startFlow

▸ **startFlow**(`flowName`, `exprAddr`): `Promise`<`void`\>

Starts the Social DNA flow

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `flowName` | `string` | on the expression |
| `exprAddr` | `string` |  |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:207](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L207)

___

### update

▸ **update**(`oldLink`, `newLink`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

Changes a link in this perspective

#### Parameters

| Name | Type |
| :------ | :------ |
| `oldLink` | [`LinkExpression`](links_Links.LinkExpression.md) |
| `newLink` | [`Link`](links_Links.Link.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:100](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/PerspectiveProxy.ts#L100)
