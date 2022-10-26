[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/Language](../modules/language_Language.md) / LinkSyncAdapter

# Interface: LinkSyncAdapter

[language/Language](../modules/language_Language.md).LinkSyncAdapter

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

- [addCallback](language_Language.LinkSyncAdapter.md#addcallback)
- [commit](language_Language.LinkSyncAdapter.md#commit)
- [currentRevision](language_Language.LinkSyncAdapter.md#currentrevision)
- [latestRevision](language_Language.LinkSyncAdapter.md#latestrevision)
- [others](language_Language.LinkSyncAdapter.md#others)
- [public](language_Language.LinkSyncAdapter.md#public)
- [pull](language_Language.LinkSyncAdapter.md#pull)
- [render](language_Language.LinkSyncAdapter.md#render)
- [writable](language_Language.LinkSyncAdapter.md#writable)

## Methods

### addCallback

▸ **addCallback**(`callback`): `any`

Get push notification when a diff got published

#### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`PerspectiveDiffObserver`](../modules/language_Language.md#perspectivediffobserver) |

#### Returns

`any`

#### Defined in

[language/Language.ts:172](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L172)

___

### commit

▸ **commit**(`diff`): `Promise`<`string`\>

Publish changes

#### Parameters

| Name | Type |
| :------ | :------ |
| `diff` | [`PerspectiveDiff`](../classes/perspectives_PerspectiveDiff.PerspectiveDiff.md) |

#### Returns

`Promise`<`string`\>

#### Defined in

[language/Language.ts:169](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L169)

___

### currentRevision

▸ **currentRevision**(): `Promise`<`string`\>

What revision are we on now -> what changes are included in output of render()

#### Returns

`Promise`<`string`\>

#### Defined in

[language/Language.ts:160](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L160)

___

### latestRevision

▸ **latestRevision**(): `Promise`<`string`\>

Call this to check if there are new changes
(compare returned revision with last one that was pulled)

#### Returns

`Promise`<`string`\>

#### Defined in

[language/Language.ts:157](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L157)

___

### others

▸ **others**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[language/Language.ts:152](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L152)

___

### public

▸ **public**(): `boolean`

#### Returns

`boolean`

#### Defined in

[language/Language.ts:151](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L151)

___

### pull

▸ **pull**(): `Promise`<[`PerspectiveDiff`](../classes/perspectives_PerspectiveDiff.PerspectiveDiff.md)\>

Check for and get new changes

#### Returns

`Promise`<[`PerspectiveDiff`](../classes/perspectives_PerspectiveDiff.PerspectiveDiff.md)\>

#### Defined in

[language/Language.ts:163](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L163)

___

### render

▸ **render**(): `Promise`<[`Perspective`](../classes/perspectives_Perspective.Perspective.md)\>

Returns the full, rendered Perspective at currentRevision

#### Returns

`Promise`<[`Perspective`](../classes/perspectives_Perspective.Perspective.md)\>

#### Defined in

[language/Language.ts:166](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L166)

___

### writable

▸ **writable**(): `boolean`

#### Returns

`boolean`

#### Defined in

[language/Language.ts:150](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L150)
