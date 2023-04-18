[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / HolochainLanguageDelegate

# Interface: HolochainLanguageDelegate

## Table of contents

### Methods

- [call](HolochainLanguageDelegate.md#call)
- [callAsync](HolochainLanguageDelegate.md#callasync)
- [registerDNAs](HolochainLanguageDelegate.md#registerdnas)

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

[language/LanguageContext.ts:34](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/LanguageContext.ts#L34)

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

[language/LanguageContext.ts:36](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/LanguageContext.ts#L36)

___

### registerDNAs

▸ **registerDNAs**(`dnas`, `holochainSignalCallback?`): `Promise`<`void`\>

Installs/registers a given DNA in the ad4m-executor

#### Parameters

| Name | Type |
| :------ | :------ |
| `dnas` | [`Dna`](../classes/Dna.md)[] |
| `holochainSignalCallback?` | `AppSignalCb` |

#### Returns

`Promise`<`void`\>

#### Defined in

[language/LanguageContext.ts:32](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/LanguageContext.ts#L32)
