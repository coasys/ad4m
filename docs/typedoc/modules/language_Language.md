[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / language/Language

# Module: language/Language

## Table of contents

### Classes

- [InteractionCall](../classes/language_Language.InteractionCall.md)
- [InteractionMeta](../classes/language_Language.InteractionMeta.md)
- [InteractionParameter](../classes/language_Language.InteractionParameter.md)
- [OnlineAgent](../classes/language_Language.OnlineAgent.md)
- [TelepresenceRpcCall](../classes/language_Language.TelepresenceRpcCall.md)

### Interfaces

- [DirectMessageAdapter](../interfaces/language_Language.DirectMessageAdapter.md)
- [ExpressionAdapter](../interfaces/language_Language.ExpressionAdapter.md)
- [ExpressionUI](../interfaces/language_Language.ExpressionUI.md)
- [GetAllAdapter](../interfaces/language_Language.GetAllAdapter.md)
- [GetByAuthorAdapter](../interfaces/language_Language.GetByAuthorAdapter.md)
- [Interaction](../interfaces/language_Language.Interaction.md)
- [Language](../interfaces/language_Language.Language.md)
- [LanguageAdapter](../interfaces/language_Language.LanguageAdapter.md)
- [LinkSyncAdapter](../interfaces/language_Language.LinkSyncAdapter.md)
- [PublicSharing](../interfaces/language_Language.PublicSharing.md)
- [ReadOnlyLanguage](../interfaces/language_Language.ReadOnlyLanguage.md)
- [SettingsUI](../interfaces/language_Language.SettingsUI.md)
- [TelepresenceAdapter](../interfaces/language_Language.TelepresenceAdapter.md)

### Type Aliases

- [MessageCallback](language_Language.md#messagecallback)
- [PerspectiveDiffObserver](language_Language.md#perspectivediffobserver)
- [StatusCallback](language_Language.md#statuscallback)
- [TelepresenceRpcCallback](language_Language.md#telepresencerpccallback)

## Type Aliases

### MessageCallback

Ƭ **MessageCallback**: (`message`: [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)) => `void`

#### Type declaration

▸ (`message`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md) |

##### Returns

`void`

#### Defined in

[language/Language.ts:175](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L175)

___

### PerspectiveDiffObserver

Ƭ **PerspectiveDiffObserver**: (`diff`: [`PerspectiveDiff`](../classes/perspectives_PerspectiveDiff.PerspectiveDiff.md)) => `void`

#### Type declaration

▸ (`diff`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `diff` | [`PerspectiveDiff`](../classes/perspectives_PerspectiveDiff.PerspectiveDiff.md) |

##### Returns

`void`

#### Defined in

[language/Language.ts:138](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L138)

___

### StatusCallback

Ƭ **StatusCallback**: (`caller`: [`DID`](DID.md#did)) => [`Perspective`](../classes/perspectives_Perspective.Perspective.md)

#### Type declaration

▸ (`caller`): [`Perspective`](../classes/perspectives_Perspective.Perspective.md)

##### Parameters

| Name | Type |
| :------ | :------ |
| `caller` | [`DID`](DID.md#did) |

##### Returns

[`Perspective`](../classes/perspectives_Perspective.Perspective.md)

#### Defined in

[language/Language.ts:176](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L176)

___

### TelepresenceRpcCallback

Ƭ **TelepresenceRpcCallback**: (`call`: [`TelepresenceRpcCall`](../classes/language_Language.TelepresenceRpcCall.md)) => `object`

#### Type declaration

▸ (`call`): `object`

##### Parameters

| Name | Type |
| :------ | :------ |
| `call` | [`TelepresenceRpcCall`](../classes/language_Language.TelepresenceRpcCall.md) |

##### Returns

`object`

#### Defined in

[language/Language.ts:243](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L243)
