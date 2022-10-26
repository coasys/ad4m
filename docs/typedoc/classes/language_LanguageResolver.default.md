[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/LanguageResolver](../modules/language_LanguageResolver.md) / default

# Class: default

[language/LanguageResolver](../modules/language_LanguageResolver.md).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

## Table of contents

### Constructors

- [constructor](language_LanguageResolver.default.md#constructor)

### Methods

- [language](language_LanguageResolver.default.md#language)
- [languageApplyTemplateAndPublish](language_LanguageResolver.default.md#languageapplytemplateandpublish)
- [languageMeta](language_LanguageResolver.default.md#languagemeta)
- [languagePublish](language_LanguageResolver.default.md#languagepublish)
- [languageRemove](language_LanguageResolver.default.md#languageremove)
- [languageSource](language_LanguageResolver.default.md#languagesource)
- [languageWriteSettings](language_LanguageResolver.default.md#languagewritesettings)
- [languages](language_LanguageResolver.default.md#languages)

## Constructors

### constructor

• **new default**()

## Methods

### language

▸ **language**(`address`): [`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

[`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)

#### Defined in

[language/LanguageResolver.ts:16](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageResolver.ts#L16)

___

### languageApplyTemplateAndPublish

▸ **languageApplyTemplateAndPublish**(`sourceLanguageHash`, `templateData`): [`LanguageRef`](language_LanguageRef.LanguageRef.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `sourceLanguageHash` | `string` |
| `templateData` | `string` |

#### Returns

[`LanguageRef`](language_LanguageRef.LanguageRef.md)

#### Defined in

[language/LanguageResolver.ts:49](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageResolver.ts#L49)

___

### languageMeta

▸ **languageMeta**(`address`): [`LanguageMeta`](language_LanguageMeta.LanguageMeta.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

[`LanguageMeta`](language_LanguageMeta.LanguageMeta.md)

#### Defined in

[language/LanguageResolver.ts:75](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageResolver.ts#L75)

___

### languagePublish

▸ **languagePublish**(`languagePath`, `languageMeta`): [`LanguageMeta`](language_LanguageMeta.LanguageMeta.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `languagePath` | `string` |
| `languageMeta` | [`LanguageMetaInput`](language_LanguageMeta.LanguageMetaInput.md) |

#### Returns

[`LanguageMeta`](language_LanguageMeta.LanguageMeta.md)

#### Defined in

[language/LanguageResolver.ts:57](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageResolver.ts#L57)

___

### languageRemove

▸ **languageRemove**(`address`): `Boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

`Boolean`

#### Defined in

[language/LanguageResolver.ts:95](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageResolver.ts#L95)

___

### languageSource

▸ **languageSource**(`address`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

`string`

#### Defined in

[language/LanguageResolver.ts:90](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageResolver.ts#L90)

___

### languageWriteSettings

▸ **languageWriteSettings**(`languageAddress`, `settings`): `Boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `languageAddress` | `string` |
| `settings` | `string` |

#### Returns

`Boolean`

#### Defined in

[language/LanguageResolver.ts:41](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageResolver.ts#L41)

___

### languages

▸ **languages**(`filter`): [`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `filter` | `string` |

#### Returns

[`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)[]

#### Defined in

[language/LanguageResolver.ts:29](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageResolver.ts#L29)
