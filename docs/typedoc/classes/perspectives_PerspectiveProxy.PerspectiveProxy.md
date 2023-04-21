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
- [#handle](perspectives_PerspectiveProxy.PerspectiveProxy.md##handle)
- [#perspectiveLinkAddedCallbacks](perspectives_PerspectiveProxy.PerspectiveProxy.md##perspectivelinkaddedcallbacks)
- [#perspectiveLinkRemovedCallbacks](perspectives_PerspectiveProxy.PerspectiveProxy.md##perspectivelinkremovedcallbacks)
- [#perspectiveLinkUpdatedCallbacks](perspectives_PerspectiveProxy.PerspectiveProxy.md##perspectivelinkupdatedcallbacks)

### Accessors

- [name](perspectives_PerspectiveProxy.PerspectiveProxy.md#name)
- [neighbourhood](perspectives_PerspectiveProxy.PerspectiveProxy.md#neighbourhood)
- [sharedUrl](perspectives_PerspectiveProxy.PerspectiveProxy.md#sharedurl)
- [state](perspectives_PerspectiveProxy.PerspectiveProxy.md#state)
- [uuid](perspectives_PerspectiveProxy.PerspectiveProxy.md#uuid)

### Methods

- [add](perspectives_PerspectiveProxy.PerspectiveProxy.md#add)
- [addLinkExpression](perspectives_PerspectiveProxy.PerspectiveProxy.md#addlinkexpression)
- [addLinks](perspectives_PerspectiveProxy.PerspectiveProxy.md#addlinks)
- [addListener](perspectives_PerspectiveProxy.PerspectiveProxy.md#addlistener)
- [addSdna](perspectives_PerspectiveProxy.PerspectiveProxy.md#addsdna)
- [availableFlows](perspectives_PerspectiveProxy.PerspectiveProxy.md#availableflows)
- [createExpression](perspectives_PerspectiveProxy.PerspectiveProxy.md#createexpression)
- [createSubject](perspectives_PerspectiveProxy.PerspectiveProxy.md#createsubject)
- [ensureSDNASubjectClass](perspectives_PerspectiveProxy.PerspectiveProxy.md#ensuresdnasubjectclass)
- [executeAction](perspectives_PerspectiveProxy.PerspectiveProxy.md#executeaction)
- [expressionsInFlowState](perspectives_PerspectiveProxy.PerspectiveProxy.md#expressionsinflowstate)
- [flowActions](perspectives_PerspectiveProxy.PerspectiveProxy.md#flowactions)
- [flowState](perspectives_PerspectiveProxy.PerspectiveProxy.md#flowstate)
- [get](perspectives_PerspectiveProxy.PerspectiveProxy.md#get)
- [getAllSubjectInstances](perspectives_PerspectiveProxy.PerspectiveProxy.md#getallsubjectinstances)
- [getExpression](perspectives_PerspectiveProxy.PerspectiveProxy.md#getexpression)
- [getNeighbourhoodProxy](perspectives_PerspectiveProxy.PerspectiveProxy.md#getneighbourhoodproxy)
- [getSdna](perspectives_PerspectiveProxy.PerspectiveProxy.md#getsdna)
- [getSingleTarget](perspectives_PerspectiveProxy.PerspectiveProxy.md#getsingletarget)
- [getSubjectProxy](perspectives_PerspectiveProxy.PerspectiveProxy.md#getsubjectproxy)
- [infer](perspectives_PerspectiveProxy.PerspectiveProxy.md#infer)
- [isSubjectInstance](perspectives_PerspectiveProxy.PerspectiveProxy.md#issubjectinstance)
- [linkMutations](perspectives_PerspectiveProxy.PerspectiveProxy.md#linkmutations)
- [loadSnapshot](perspectives_PerspectiveProxy.PerspectiveProxy.md#loadsnapshot)
- [remove](perspectives_PerspectiveProxy.PerspectiveProxy.md#remove)
- [removeLinks](perspectives_PerspectiveProxy.PerspectiveProxy.md#removelinks)
- [removeListener](perspectives_PerspectiveProxy.PerspectiveProxy.md#removelistener)
- [runFlowAction](perspectives_PerspectiveProxy.PerspectiveProxy.md#runflowaction)
- [sdnaFlows](perspectives_PerspectiveProxy.PerspectiveProxy.md#sdnaflows)
- [setSdna](perspectives_PerspectiveProxy.PerspectiveProxy.md#setsdna)
- [setSingleTarget](perspectives_PerspectiveProxy.PerspectiveProxy.md#setsingletarget)
- [snapshot](perspectives_PerspectiveProxy.PerspectiveProxy.md#snapshot)
- [startFlow](perspectives_PerspectiveProxy.PerspectiveProxy.md#startflow)
- [stringOrTemplateObjectToSubjectClass](perspectives_PerspectiveProxy.PerspectiveProxy.md#stringortemplateobjecttosubjectclass)
- [subjectClasses](perspectives_PerspectiveProxy.PerspectiveProxy.md#subjectclasses)
- [subjectClassesByTemplate](perspectives_PerspectiveProxy.PerspectiveProxy.md#subjectclassesbytemplate)
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

[perspectives/PerspectiveProxy.ts:33](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L33)

## Properties

### #client

• `Private` **#client**: [`PerspectiveClient`](perspectives_PerspectiveClient.PerspectiveClient.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:28](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L28)

___

### #handle

• `Private` **#handle**: [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:27](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L27)

___

### #perspectiveLinkAddedCallbacks

• `Private` **#perspectiveLinkAddedCallbacks**: [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback)[]

#### Defined in

[perspectives/PerspectiveProxy.ts:29](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L29)

___

### #perspectiveLinkRemovedCallbacks

• `Private` **#perspectiveLinkRemovedCallbacks**: [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback)[]

#### Defined in

[perspectives/PerspectiveProxy.ts:30](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L30)

___

### #perspectiveLinkUpdatedCallbacks

• `Private` **#perspectiveLinkUpdatedCallbacks**: [`LinkCallback`](../modules/perspectives_PerspectiveClient.md#linkcallback)[]

#### Defined in

[perspectives/PerspectiveProxy.ts:31](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L31)

## Accessors

### name

• `get` **name**(): `string`

Given name of the perspective

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveProxy.ts:95](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L95)

___

### neighbourhood

• `get` **neighbourhood**(): `void` \| [`Neighbourhood`](neighbourhood_Neighbourhood.Neighbourhood.md)

If the perspective is shared as a Neighbourhood, this is the Neighbourhood Expression

#### Returns

`void` \| [`Neighbourhood`](neighbourhood_Neighbourhood.Neighbourhood.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:105](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L105)

___

### sharedUrl

• `get` **sharedUrl**(): `string` \| `void`

If the perspective is shared as a Neighbourhood, this is the Neighbourhood URL

#### Returns

`string` \| `void`

#### Defined in

[perspectives/PerspectiveProxy.ts:100](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L100)

___

### state

• `get` **state**(): [`PerspectiveState`](../enums/perspectives_PerspectiveHandle.PerspectiveState.md)

Returns the state of the perspective *

#### Returns

[`PerspectiveState`](../enums/perspectives_PerspectiveHandle.PerspectiveState.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:110](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L110)

___

### uuid

• `get` **uuid**(): `string`

Unique ID of the perspective

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveProxy.ts:90](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L90)

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

[perspectives/PerspectiveProxy.ts:125](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L125)

___

### addLinkExpression

▸ **addLinkExpression**(`link`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

Adds a linkExpression to this perspective

#### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:145](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L145)

___

### addLinks

▸ **addLinks**(`links`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

Adds multiple links to this perspective *

#### Parameters

| Name | Type |
| :------ | :------ |
| `links` | [`Link`](links_Links.Link.md)[] |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:130](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L130)

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

[perspectives/PerspectiveProxy.ts:169](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L169)

___

### addSdna

▸ **addSdna**(`sdnaCode`): `Promise`<`void`\>

Adds the given Social DNA code to the perspective's SDNA code

#### Parameters

| Name | Type |
| :------ | :------ |
| `sdnaCode` | `string` |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:328](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L328)

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

[perspectives/PerspectiveProxy.ts:265](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L265)

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

[perspectives/PerspectiveProxy.ts:161](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L161)

___

### createSubject

▸ **createSubject**<`T`\>(`subjectClass`, `exprAddr`): `Promise`<`T`\>

Creates a new subject instance by running its (SDNA defined) constructor,
which means adding links around the given expression address so that it
conforms to the given subject class.

#### Type parameters

| Name |
| :------ |
| `T` |

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `subjectClass` | `T` | Either a string with the name of the subject class, or an object with the properties of the subject class. In the latter case, the first subject class that matches the given properties will be used. |
| `exprAddr` | `string` | The address of the expression to be turned into a subject instance |

#### Returns

`Promise`<`T`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:368](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L368)

___

### ensureSDNASubjectClass

▸ **ensureSDNASubjectClass**(`jsClass`): `Promise`<`void`\>

Takes a JS class (its constructor) and assumes that it was decorated by
the

**`Subject Class`**

etc. decorators. It then tests if there is a subject class
already present in the perspective's SDNA that matches the given class.
If there is no such class, it gets the JS class's SDNA by calling its
static generateSDNA() function and adds it to the perspective's SDNA.

#### Parameters

| Name | Type |
| :------ | :------ |
| `jsClass` | `any` |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:506](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L506)

___

### executeAction

▸ **executeAction**(`actions`, `expression`, `parameters`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `actions` | `any` |
| `expression` | `any` |
| `parameters` | `Parameter`[] |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:44](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L44)

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

[perspectives/PerspectiveProxy.ts:279](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L279)

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

[perspectives/PerspectiveProxy.ts:291](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L291)

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

[perspectives/PerspectiveProxy.ts:285](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L285)

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

[perspectives/PerspectiveProxy.ts:115](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L115)

___

### getAllSubjectInstances

▸ **getAllSubjectInstances**<`T`\>(`subjectClass`): `Promise`<`T`[]\>

Returns all subject instances of the given subject class as proxy objects.

#### Type parameters

| Name |
| :------ |
| `T` |

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `subjectClass` | `T` | Either a string with the name of the subject class, or an object with the properties of the subject class. In the latter case, all subject classes that match the given properties will be used. |

#### Returns

`Promise`<`T`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:416](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L416)

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

[perspectives/PerspectiveProxy.ts:157](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L157)

___

### getNeighbourhoodProxy

▸ **getNeighbourhoodProxy**(): [`NeighbourhoodProxy`](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md)

#### Returns

[`NeighbourhoodProxy`](neighbourhood_NeighbourhoodProxy.NeighbourhoodProxy.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:514](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L514)

___

### getSdna

▸ **getSdna**(): `Promise`<`string`[]\>

Returns the perspective's Social DNA code 
This will return all SDNA code elements in an array.

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:318](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L318)

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

[perspectives/PerspectiveProxy.ts:227](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L227)

___

### getSubjectProxy

▸ **getSubjectProxy**<`T`\>(`base`, `subjectClass`): `Promise`<`T`\>

For an existing subject instance (existing in the perspective's links)
this function returns a proxy object that can be used to access the subject's
properties and methods.

#### Type parameters

| Name |
| :------ |
| `T` |

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `base` | `string` | URI of the subject's root expression |
| `subjectClass` | `T` | Either a string with the name of the subject class, or an object with the properties of the subject class. In the latter case, the first subject class that matches the given properties will be used. |

#### Returns

`Promise`<`T`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:401](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L401)

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

[perspectives/PerspectiveProxy.ts:120](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L120)

___

### isSubjectInstance

▸ **isSubjectInstance**<`T`\>(`expression`, `subjectClass`): `Promise`<`boolean`\>

Checks if the given expression is a subject instance of the given subject class

#### Type parameters

| Name |
| :------ |
| `T` |

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `expression` | `string` | The expression to be checked |
| `subjectClass` | `T` | Either a string with the name of the subject class, or an object with the properties of the subject class. In the latter case, the first subject class that matches the given properties will be used. |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:386](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L386)

___

### linkMutations

▸ **linkMutations**(`mutations`): `Promise`<[`LinkExpressionMutations`](links_Links.LinkExpressionMutations.md)\>

Adds and removes multiple links from this perspective *

#### Parameters

| Name | Type |
| :------ | :------ |
| `mutations` | [`LinkMutations`](links_Links.LinkMutations.md) |

#### Returns

`Promise`<[`LinkExpressionMutations`](links_Links.LinkExpressionMutations.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:140](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L140)

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

[perspectives/PerspectiveProxy.ts:208](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L208)

___

### remove

▸ **remove**(`link`): `Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |

#### Returns

`Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

#### Defined in

[perspectives/PerspectiveProxy.ts:153](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L153)

___

### removeLinks

▸ **removeLinks**(`links`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

Removes multiple links from this perspective *

#### Parameters

| Name | Type |
| :------ | :------ |
| `links` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md)[] |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:135](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L135)

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

[perspectives/PerspectiveProxy.ts:183](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L183)

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

[perspectives/PerspectiveProxy.ts:297](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L297)

___

### sdnaFlows

▸ **sdnaFlows**(): `Promise`<`string`[]\>

Returns all the Social DNA flows defined in this perspective

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:259](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L259)

___

### setSdna

▸ **setSdna**(`sdnaCode`): `Promise`<`void`\>

Set the perspective's Social DNA code to the given string. 
This will replace all previous SDNA code elements with the new one.

#### Parameters

| Name | Type |
| :------ | :------ |
| `sdnaCode` | `string` |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:307](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L307)

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

[perspectives/PerspectiveProxy.ts:243](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L243)

___

### snapshot

▸ **snapshot**(): `Promise`<[`Perspective`](perspectives_Perspective.Perspective.md)\>

Create and return a snapshot of this perspective
A snapshot is a rendered Perspectie object that contains all the links of the perspective.

#### Returns

`Promise`<[`Perspective`](perspectives_Perspective.Perspective.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:203](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L203)

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

[perspectives/PerspectiveProxy.ts:271](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L271)

___

### stringOrTemplateObjectToSubjectClass

▸ **stringOrTemplateObjectToSubjectClass**<`T`\>(`subjectClass`): `Promise`<`string`\>

#### Type parameters

| Name |
| :------ |
| `T` |

#### Parameters

| Name | Type |
| :------ | :------ |
| `subjectClass` | `T` |

#### Returns

`Promise`<`string`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:345](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L345)

___

### subjectClasses

▸ **subjectClasses**(): `Promise`<`string`[]\>

Returns all the Subject classes defined in this perspectives SDNA

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:337](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L337)

___

### subjectClassesByTemplate

▸ **subjectClassesByTemplate**(`obj`): `Promise`<`string`[]\>

Returns all subject classes that match the given template object.
This function looks at the properties of the template object and
its setters and collections to create a Prolog query that finds
all subject classes that would be converted to a proxy object
with exactly the same properties and collections.

Since there could be multiple subject classes that match the given
criteria, this function returns a list of class names.

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `obj` | `object` | The template object |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:444](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L444)

___

### update

▸ **update**(`oldLink`, `newLink`): `Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `oldLink` | [`LinkExpressionInput`](links_Links.LinkExpressionInput.md) |
| `newLink` | [`Link`](links_Links.Link.md) |

#### Returns

`Promise`<[`LinkExpression`](links_Links.LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:149](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/perspectives/PerspectiveProxy.ts#L149)