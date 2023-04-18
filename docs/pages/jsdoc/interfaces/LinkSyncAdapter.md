[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / LinkSyncAdapter

# Interface: LinkSyncAdapter

Interface for "Link Languages" that facilitate the synchronization
between agents' local Perspectives inside a Neighbourhood.
The assumption is that every version of the shared Perspective
is labeled with a unique revision string.
Changes are committed and retrieved through diffs.
Think of a LinkSyncAdapter as a git branch to which agents commit
their changes to and pull diffs from their current revision
to the latest one.

## Table of contents

### Methods

- [addCallback](LinkSyncAdapter.md#addcallback)
- [addSyncStateChangeCallback](LinkSyncAdapter.md#addsyncstatechangecallback)
- [commit](LinkSyncAdapter.md#commit)
- [currentRevision](LinkSyncAdapter.md#currentrevision)
- [others](LinkSyncAdapter.md#others)
- [public](LinkSyncAdapter.md#public)
- [render](LinkSyncAdapter.md#render)
- [sync](LinkSyncAdapter.md#sync)
- [writable](LinkSyncAdapter.md#writable)

## Methods

### addCallback

▸ **addCallback**(`callback`): `any`

Get push notification when a diff got published

#### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`PerspectiveDiffObserver`](../modules.md#perspectivediffobserver) |

#### Returns

`any`

#### Defined in

[language/Language.ts:186](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L186)

___

### addSyncStateChangeCallback

▸ **addSyncStateChangeCallback**(`callback`): `any`

Add a sync state callback method

#### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`SyncStateChangeObserver`](../modules.md#syncstatechangeobserver) |

#### Returns

`any`

#### Defined in

[language/Language.ts:189](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L189)

___

### commit

▸ **commit**(`diff`): `Promise`<`string`\>

Publish changes

#### Parameters

| Name | Type |
| :------ | :------ |
| `diff` | [`PerspectiveDiff`](../classes/PerspectiveDiff.md) |

#### Returns

`Promise`<`string`\>

#### Defined in

[language/Language.ts:183](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L183)

___

### currentRevision

▸ **currentRevision**(): `Promise`<`string`\>

What revision are we on now -> what changes are included in output of render()

#### Returns

`Promise`<`string`\>

#### Defined in

[language/Language.ts:169](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L169)

___

### others

▸ **others**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[language/Language.ts:166](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L166)

___

### public

▸ **public**(): `boolean`

#### Returns

`boolean`

#### Defined in

[language/Language.ts:165](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L165)

___

### render

▸ **render**(): `Promise`<[`Perspective`](../classes/Perspective.md)\>

Returns the full, rendered Perspective at currentRevision

#### Returns

`Promise`<[`Perspective`](../classes/Perspective.md)\>

#### Defined in

[language/Language.ts:180](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L180)

___

### sync

▸ **sync**(): `Promise`<[`PerspectiveDiff`](../classes/PerspectiveDiff.md)\>

Check for and get new changes, 
notify others of local changes.
This function will be called every 
few seconds by the ad4m-executor.

#### Returns

`Promise`<[`PerspectiveDiff`](../classes/PerspectiveDiff.md)\>

#### Defined in

[language/Language.ts:177](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L177)

___

### writable

▸ **writable**(): `boolean`

#### Returns

`boolean`

#### Defined in

[language/Language.ts:164](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L164)
