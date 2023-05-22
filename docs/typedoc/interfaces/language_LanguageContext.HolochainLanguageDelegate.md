[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/LanguageContext](../modules/language_LanguageContext.md) / HolochainLanguageDelegate

# Interface: HolochainLanguageDelegate

[language/LanguageContext](../modules/language_LanguageContext.md).HolochainLanguageDelegate

## Table of contents

### Methods

- [call](language_LanguageContext.HolochainLanguageDelegate.md#call)
- [callAsync](language_LanguageContext.HolochainLanguageDelegate.md#callasync)
- [registerDNAs](language_LanguageContext.HolochainLanguageDelegate.md#registerdnas)

## Methods

### call

▸ **call**(`dnaNick`, `zomeName`, `fnName`, `params`): `Promise`<`any`\>

Makes a single call to a given holochain DNA. Underlying implementation puts these calls into a sync fifo queue

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

[language/LanguageContext.ts:34](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/LanguageContext.ts#L34)

___

### callAsync

▸ **callAsync**(`calls`, `timeoutMs?`): `Promise`<`any`[]\>

Makes all supplied calls in parallel to the provided holochain dna... Should only be called on read operations to avoid source chain async mutation errors

#### Parameters

| Name | Type |
| :------ | :------ |
| `calls` | { `dnaNick`: `string` ; `fnName`: `string` ; `params`: `string` \| `object` ; `zomeName`: `string`  }[] |
| `timeoutMs?` | `number` |

#### Returns

`Promise`<`any`[]\>

#### Defined in

[language/LanguageContext.ts:36](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/LanguageContext.ts#L36)

___

### registerDNAs

▸ **registerDNAs**(`dnas`, `holochainSignalCallback?`): `Promise`<`void`\>

Installs/registers a given DNA in the ad4m-executor

#### Parameters

| Name | Type |
| :------ | :------ |
| `dnas` | [`Dna`](../classes/language_LanguageContext.Dna.md)[] |
| `holochainSignalCallback?` | `AppSignalCb` |

#### Returns

`Promise`<`void`\>

#### Defined in

[language/LanguageContext.ts:32](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/LanguageContext.ts#L32)