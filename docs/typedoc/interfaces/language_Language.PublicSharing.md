[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/Language](../modules/language_Language.md) / PublicSharing

# Interface: PublicSharing

[language/Language](../modules/language_Language.md).PublicSharing

Implement this interface if your Language supports creation of sharing
of Expressions.
See ExpressionAdapter

## Table of contents

### Methods

- [createPublic](language_Language.PublicSharing.md#createpublic)

## Methods

### createPublic

▸ **createPublic**(`content`): `Promise`<`string`\>

Create an Expression and shares it.
Return the Expression's address.

#### Parameters

| Name | Type | Description |
| :------ | :------ | :------ |
| `content` | `object` | is the object created by the constructorIcon component |

#### Returns

`Promise`<`string`\>

#### Defined in

[language/Language.ts:105](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L105)
