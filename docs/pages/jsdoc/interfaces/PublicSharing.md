[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / PublicSharing

# Interface: PublicSharing

Implement this interface if your Language supports creation of sharing
of Expressions.
See ExpressionAdapter

## Table of contents

### Methods

- [createPublic](PublicSharing.md#createpublic)

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

[language/Language.ts:118](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L118)