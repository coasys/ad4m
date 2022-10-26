[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/LanguageClient](../modules/language_LanguageClient.md) / LanguageClient

# Class: LanguageClient

[language/LanguageClient](../modules/language_LanguageClient.md).LanguageClient

## Table of contents

### Constructors

- [constructor](language_LanguageClient.LanguageClient.md#constructor)

### Properties

- [#apolloClient](language_LanguageClient.LanguageClient.md##apolloclient)

### Methods

- [all](language_LanguageClient.LanguageClient.md#all)
- [applyTemplateAndPublish](language_LanguageClient.LanguageClient.md#applytemplateandpublish)
- [byAddress](language_LanguageClient.LanguageClient.md#byaddress)
- [byFilter](language_LanguageClient.LanguageClient.md#byfilter)
- [meta](language_LanguageClient.LanguageClient.md#meta)
- [publish](language_LanguageClient.LanguageClient.md#publish)
- [remove](language_LanguageClient.LanguageClient.md#remove)
- [source](language_LanguageClient.LanguageClient.md#source)
- [writeSettings](language_LanguageClient.LanguageClient.md#writesettings)

## Constructors

### constructor

• **new LanguageClient**(`apolloClient`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `apolloClient` | `ApolloClient`<`any`\> |

#### Defined in

[language/LanguageClient.ts:31](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L31)

## Properties

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[language/LanguageClient.ts:29](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L29)

## Methods

### all

▸ **all**(): `Promise`<[`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)[]\>

#### Returns

`Promise`<[`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)[]\>

#### Defined in

[language/LanguageClient.ts:59](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L59)

___

### applyTemplateAndPublish

▸ **applyTemplateAndPublish**(`sourceLanguageHash`, `templateData`): `Promise`<[`LanguageRef`](language_LanguageRef.LanguageRef.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `sourceLanguageHash` | `string` |
| `templateData` | `string` |

#### Returns

`Promise`<[`LanguageRef`](language_LanguageRef.LanguageRef.md)\>

#### Defined in

[language/LanguageClient.ts:76](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L76)

___

### byAddress

▸ **byAddress**(`address`): `Promise`<[`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

`Promise`<[`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)\>

#### Defined in

[language/LanguageClient.ts:35](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L35)

___

### byFilter

▸ **byFilter**(`filter`): `Promise`<[`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `filter` | `string` |

#### Returns

`Promise`<[`LanguageHandle`](language_LanguageHandle.LanguageHandle.md)[]\>

#### Defined in

[language/LanguageClient.ts:47](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L47)

___

### meta

▸ **meta**(`address`): `Promise`<[`LanguageMeta`](language_LanguageMeta.LanguageMeta.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

`Promise`<[`LanguageMeta`](language_LanguageMeta.LanguageMeta.md)\>

#### Defined in

[language/LanguageClient.ts:114](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L114)

___

### publish

▸ **publish**(`languagePath`, `languageMeta`): `Promise`<[`LanguageMeta`](language_LanguageMeta.LanguageMeta.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `languagePath` | `string` |
| `languageMeta` | [`LanguageMetaInput`](language_LanguageMeta.LanguageMetaInput.md) |

#### Returns

`Promise`<[`LanguageMeta`](language_LanguageMeta.LanguageMeta.md)\>

#### Defined in

[language/LanguageClient.ts:95](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L95)

___

### remove

▸ **remove**(`address`): `Promise`<`Boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

`Promise`<`Boolean`\>

#### Defined in

[language/LanguageClient.ts:146](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L146)

___

### source

▸ **source**(`address`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[language/LanguageClient.ts:131](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L131)

___

### writeSettings

▸ **writeSettings**(`languageAddress`, `settings`): `Promise`<`Boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `languageAddress` | `string` |
| `settings` | `string` |

#### Returns

`Promise`<`Boolean`\>

#### Defined in

[language/LanguageClient.ts:63](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/LanguageClient.ts#L63)
