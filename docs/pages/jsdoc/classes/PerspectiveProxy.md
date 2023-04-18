[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / PerspectiveProxy

# Class: PerspectiveProxy

Perspective UI proxy object

Convenience object for UIs to interact with a perspective.
It is created by some of the methods in the PerspectiveClient class and includes
a reference to the PerspectiveClient object that created it.

## Table of contents

### Constructors

- [constructor](PerspectiveProxy.md#constructor)

### Properties

- [#client](PerspectiveProxy.md##client)
- [#handle](PerspectiveProxy.md##handle)
- [#perspectiveLinkAddedCallbacks](PerspectiveProxy.md##perspectivelinkaddedcallbacks)
- [#perspectiveLinkRemovedCallbacks](PerspectiveProxy.md##perspectivelinkremovedcallbacks)
- [#perspectiveLinkUpdatedCallbacks](PerspectiveProxy.md##perspectivelinkupdatedcallbacks)
- [#perspectiveSyncStateChangeCallbacks](PerspectiveProxy.md##perspectivesyncstatechangecallbacks)

### Accessors

- [name](PerspectiveProxy.md#name)
- [neighbourhood](PerspectiveProxy.md#neighbourhood)
- [sharedUrl](PerspectiveProxy.md#sharedurl)
- [state](PerspectiveProxy.md#state)
- [uuid](PerspectiveProxy.md#uuid)

### Methods

- [add](PerspectiveProxy.md#add)
- [addLinkExpression](PerspectiveProxy.md#addlinkexpression)
- [addLinks](PerspectiveProxy.md#addlinks)
- [addListener](PerspectiveProxy.md#addlistener)
- [addSdna](PerspectiveProxy.md#addsdna)
- [addSyncStateChangeListener](PerspectiveProxy.md#addsyncstatechangelistener)
- [availableFlows](PerspectiveProxy.md#availableflows)
- [createExpression](PerspectiveProxy.md#createexpression)
- [createSubject](PerspectiveProxy.md#createsubject)
- [ensureSDNASubjectClass](PerspectiveProxy.md#ensuresdnasubjectclass)
- [executeAction](PerspectiveProxy.md#executeaction)
- [expressionsInFlowState](PerspectiveProxy.md#expressionsinflowstate)
- [flowActions](PerspectiveProxy.md#flowactions)
- [flowState](PerspectiveProxy.md#flowstate)
- [get](PerspectiveProxy.md#get)
- [getAllSubjectInstances](PerspectiveProxy.md#getallsubjectinstances)
- [getAllSubjectProxies](PerspectiveProxy.md#getallsubjectproxies)
- [getExpression](PerspectiveProxy.md#getexpression)
- [getNeighbourhoodProxy](PerspectiveProxy.md#getneighbourhoodproxy)
- [getSdna](PerspectiveProxy.md#getsdna)
- [getSingleTarget](PerspectiveProxy.md#getsingletarget)
- [getSubjectProxy](PerspectiveProxy.md#getsubjectproxy)
- [infer](PerspectiveProxy.md#infer)
- [isSubjectInstance](PerspectiveProxy.md#issubjectinstance)
- [linkMutations](PerspectiveProxy.md#linkmutations)
- [loadSnapshot](PerspectiveProxy.md#loadsnapshot)
- [remove](PerspectiveProxy.md#remove)
- [removeLinks](PerspectiveProxy.md#removelinks)
- [removeListener](PerspectiveProxy.md#removelistener)
- [runFlowAction](PerspectiveProxy.md#runflowaction)
- [sdnaFlows](PerspectiveProxy.md#sdnaflows)
- [setSdna](PerspectiveProxy.md#setsdna)
- [setSingleTarget](PerspectiveProxy.md#setsingletarget)
- [snapshot](PerspectiveProxy.md#snapshot)
- [startFlow](PerspectiveProxy.md#startflow)
- [stringOrTemplateObjectToSubjectClass](PerspectiveProxy.md#stringortemplateobjecttosubjectclass)
- [subjectClasses](PerspectiveProxy.md#subjectclasses)
- [subjectClassesByTemplate](PerspectiveProxy.md#subjectclassesbytemplate)
- [update](PerspectiveProxy.md#update)

## Constructors

### constructor

• **new PerspectiveProxy**(`handle`, `ad4m`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `handle` | [`PerspectiveHandle`](PerspectiveHandle.md) |
| `ad4m` | `PerspectiveClient` |

#### Defined in

[perspectives/PerspectiveProxy.ts:34](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L34)

## Properties

### #client

• `Private` **#client**: `PerspectiveClient`

#### Defined in

[perspectives/PerspectiveProxy.ts:28](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L28)

___

### #handle

• `Private` **#handle**: [`PerspectiveHandle`](PerspectiveHandle.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:27](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L27)

___

### #perspectiveLinkAddedCallbacks

• `Private` **#perspectiveLinkAddedCallbacks**: `LinkCallback`[]

#### Defined in

[perspectives/PerspectiveProxy.ts:29](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L29)

___

### #perspectiveLinkRemovedCallbacks

• `Private` **#perspectiveLinkRemovedCallbacks**: `LinkCallback`[]

#### Defined in

[perspectives/PerspectiveProxy.ts:30](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L30)

___

### #perspectiveLinkUpdatedCallbacks

• `Private` **#perspectiveLinkUpdatedCallbacks**: `LinkCallback`[]

#### Defined in

[perspectives/PerspectiveProxy.ts:31](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L31)

___

### #perspectiveSyncStateChangeCallbacks

• `Private` **#perspectiveSyncStateChangeCallbacks**: `SyncStateChangeCallback`[]

#### Defined in

[perspectives/PerspectiveProxy.ts:32](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L32)

## Accessors

### name

• `get` **name**(): `string`

Given name of the perspective

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveProxy.ts:98](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L98)

___

### neighbourhood

• `get` **neighbourhood**(): `void` \| [`Neighbourhood`](Neighbourhood.md)

If the perspective is shared as a Neighbourhood, this is the Neighbourhood Expression

#### Returns

`void` \| [`Neighbourhood`](Neighbourhood.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:108](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L108)

___

### sharedUrl

• `get` **sharedUrl**(): `string` \| `void`

If the perspective is shared as a Neighbourhood, this is the Neighbourhood URL

#### Returns

`string` \| `void`

#### Defined in

[perspectives/PerspectiveProxy.ts:103](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L103)

___

### state

• `get` **state**(): [`PerspectiveState`](../enums/PerspectiveState.md)

Returns the state of the perspective *

#### Returns

[`PerspectiveState`](../enums/PerspectiveState.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:113](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L113)

___

### uuid

• `get` **uuid**(): `string`

Unique ID of the perspective

#### Returns

`string`

#### Defined in

[perspectives/PerspectiveProxy.ts:93](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L93)

## Methods

### add

▸ **add**(`link`): `Promise`<[`LinkExpression`](LinkExpression.md)\>

Adds a link to this perspective

#### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`Link`](Link.md) |

#### Returns

`Promise`<[`LinkExpression`](LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:128](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L128)

___

### addLinkExpression

▸ **addLinkExpression**(`link`): `Promise`<[`LinkExpression`](LinkExpression.md)\>

Adds a linkExpression to this perspective

#### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`LinkExpressionInput`](LinkExpressionInput.md) |

#### Returns

`Promise`<[`LinkExpression`](LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:148](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L148)

___

### addLinks

▸ **addLinks**(`links`): `Promise`<[`LinkExpression`](LinkExpression.md)[]\>

Adds multiple links to this perspective *

#### Parameters

| Name | Type |
| :------ | :------ |
| `links` | [`Link`](Link.md)[] |

#### Returns

`Promise`<[`LinkExpression`](LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:133](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L133)

___

### addListener

▸ **addListener**(`type`, `cb`): `Promise`<`void`\>

Adds a link listener

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `type` | `PerspectiveListenerTypes` | Can be 'link-added' or 'link-removed' |
| `cb` | `LinkCallback` | Callback function that is called when a link is added to the perspective |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:172](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L172)

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

[perspectives/PerspectiveProxy.ts:339](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L339)

___

### addSyncStateChangeListener

▸ **addSyncStateChangeListener**(`cb`): `Promise`<`void`\>

Adds a sync state listener

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `cb` | `SyncStateChangeCallback` | Callback function that is called when the sync state of the perspective changes |

#### Returns

`Promise`<`void`\>

A function that can be called to remove the listener

#### Defined in

[perspectives/PerspectiveProxy.ts:186](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L186)

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

[perspectives/PerspectiveProxy.ts:276](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L276)

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

[perspectives/PerspectiveProxy.ts:164](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L164)

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

[perspectives/PerspectiveProxy.ts:379](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L379)

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

[perspectives/PerspectiveProxy.ts:533](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L533)

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

[perspectives/PerspectiveProxy.ts:47](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L47)

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

[perspectives/PerspectiveProxy.ts:290](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L290)

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

[perspectives/PerspectiveProxy.ts:302](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L302)

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

[perspectives/PerspectiveProxy.ts:296](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L296)

___

### get

▸ **get**(`query`): `Promise`<[`LinkExpression`](LinkExpression.md)[]\>

Returns all the links of this perspective that matches the LinkQuery

#### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](LinkQuery.md) |

#### Returns

`Promise`<[`LinkExpression`](LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:118](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L118)

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

[perspectives/PerspectiveProxy.ts:427](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L427)

___

### getAllSubjectProxies

▸ **getAllSubjectProxies**<`T`\>(`subjectClass`): `Promise`<`T`[]\>

#### Type parameters

| Name |
| :------ |
| `T` |

#### Parameters

| Name | Type |
| :------ | :------ |
| `subjectClass` | `T` |

#### Returns

`Promise`<`T`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:444](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L444)

___

### getExpression

▸ **getExpression**(`expressionURI`): `Promise`<[`ExpressionRendered`](ExpressionRendered.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `expressionURI` | `string` |

#### Returns

`Promise`<[`ExpressionRendered`](ExpressionRendered.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:160](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L160)

___

### getNeighbourhoodProxy

▸ **getNeighbourhoodProxy**(): [`NeighbourhoodProxy`](NeighbourhoodProxy.md)

#### Returns

[`NeighbourhoodProxy`](NeighbourhoodProxy.md)

#### Defined in

[perspectives/PerspectiveProxy.ts:541](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L541)

___

### getSdna

▸ **getSdna**(): `Promise`<`string`[]\>

Returns the perspective's Social DNA code 
This will return all SDNA code elements in an array.

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:329](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L329)

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
| `query` | [`LinkQuery`](LinkQuery.md) |

#### Returns

`Promise`<`string` \| `void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:238](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L238)

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

[perspectives/PerspectiveProxy.ts:412](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L412)

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

[perspectives/PerspectiveProxy.ts:123](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L123)

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

[perspectives/PerspectiveProxy.ts:397](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L397)

___

### linkMutations

▸ **linkMutations**(`mutations`): `Promise`<[`LinkExpressionMutations`](LinkExpressionMutations.md)\>

Adds and removes multiple links from this perspective *

#### Parameters

| Name | Type |
| :------ | :------ |
| `mutations` | [`LinkMutations`](LinkMutations.md) |

#### Returns

`Promise`<[`LinkExpressionMutations`](LinkExpressionMutations.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:143](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L143)

___

### loadSnapshot

▸ **loadSnapshot**(`snapshot`): `Promise`<`void`\>

Take and load all the links from the given snapshot

#### Parameters

| Name | Type |
| :------ | :------ |
| `snapshot` | [`Perspective`](Perspective.md) |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:219](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L219)

___

### remove

▸ **remove**(`link`): `Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `link` | [`LinkExpressionInput`](LinkExpressionInput.md) |

#### Returns

`Promise`<{ `perspectiveRemoveLink`: `boolean`  }\>

#### Defined in

[perspectives/PerspectiveProxy.ts:156](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L156)

___

### removeLinks

▸ **removeLinks**(`links`): `Promise`<[`LinkExpression`](LinkExpression.md)[]\>

Removes multiple links from this perspective *

#### Parameters

| Name | Type |
| :------ | :------ |
| `links` | [`LinkExpressionInput`](LinkExpressionInput.md)[] |

#### Returns

`Promise`<[`LinkExpression`](LinkExpression.md)[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:138](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L138)

___

### removeListener

▸ **removeListener**(`type`, `cb`): `Promise`<`void`\>

Removes a previously added link listener

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `type` | `PerspectiveListenerTypes` | Can be 'link-added' or 'link-removed' |
| `cb` | `LinkCallback` | Callback function that is called when a link is added to the perspective |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:194](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L194)

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

[perspectives/PerspectiveProxy.ts:308](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L308)

___

### sdnaFlows

▸ **sdnaFlows**(): `Promise`<`string`[]\>

Returns all the Social DNA flows defined in this perspective

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:270](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L270)

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

[perspectives/PerspectiveProxy.ts:318](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L318)

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
| `link` | [`Link`](Link.md) |

#### Returns

`Promise`<`void`\>

#### Defined in

[perspectives/PerspectiveProxy.ts:254](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L254)

___

### snapshot

▸ **snapshot**(): `Promise`<[`Perspective`](Perspective.md)\>

Create and return a snapshot of this perspective
A snapshot is a rendered Perspectie object that contains all the links of the perspective.

#### Returns

`Promise`<[`Perspective`](Perspective.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:214](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L214)

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

[perspectives/PerspectiveProxy.ts:282](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L282)

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

[perspectives/PerspectiveProxy.ts:356](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L356)

___

### subjectClasses

▸ **subjectClasses**(): `Promise`<`string`[]\>

Returns all the Subject classes defined in this perspectives SDNA

#### Returns

`Promise`<`string`[]\>

#### Defined in

[perspectives/PerspectiveProxy.ts:348](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L348)

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

[perspectives/PerspectiveProxy.ts:471](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L471)

___

### update

▸ **update**(`oldLink`, `newLink`): `Promise`<[`LinkExpression`](LinkExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `oldLink` | [`LinkExpressionInput`](LinkExpressionInput.md) |
| `newLink` | [`Link`](Link.md) |

#### Returns

`Promise`<[`LinkExpression`](LinkExpression.md)\>

#### Defined in

[perspectives/PerspectiveProxy.ts:152](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/perspectives/PerspectiveProxy.ts#L152)
