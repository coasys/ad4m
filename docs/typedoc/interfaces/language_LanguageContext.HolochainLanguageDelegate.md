[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/LanguageContext](../modules/language_LanguageContext.md) / HolochainLanguageDelegate

# Interface: HolochainLanguageDelegate

[language/LanguageContext](../modules/language_LanguageContext.md).HolochainLanguageDelegate

## Table of contents

### Methods

- [call](language_LanguageContext.HolochainLanguageDelegate.md#call)
- [registerDNAs](language_LanguageContext.HolochainLanguageDelegate.md#registerdnas)

## Methods

### call

▸ **call**(`dnaNick`, `zomeName`, `fnName`, `params`): `Promise`<`any`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `dnaNick` | `string` |
| `zomeName` | `string` |
| `fnName` | `string` |
| `params` | `string` \| `object` |

#### Returns

`Promise`<`any`\>

#### Defined in

[language/LanguageContext.ts:31](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/language/LanguageContext.ts#L31)

___

### registerDNAs

▸ **registerDNAs**(`dnas`, `holochainSignalCallback?`): `any`

#### Parameters

| Name | Type |
| :------ | :------ |
| `dnas` | [`Dna`](../classes/language_LanguageContext.Dna.md)[] |
| `holochainSignalCallback?` | `AppSignalCb` |

#### Returns

`any`

#### Defined in

[language/LanguageContext.ts:30](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/language/LanguageContext.ts#L30)
