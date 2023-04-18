[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / ExpressionAdapter

# Interface: ExpressionAdapter

Interface for the most common Expression Languages

## Table of contents

### Properties

- [putAdapter](ExpressionAdapter.md#putadapter)

### Methods

- [get](ExpressionAdapter.md#get)

## Properties

### putAdapter

• **putAdapter**: [`PublicSharing`](PublicSharing.md) \| [`ReadOnlyLanguage`](ReadOnlyLanguage.md)

Strategy for putting an expression with needs to be different
for those two cases:
1. PublicSharing means that this language supports the creation
   and sharing of Expressions, which is the common use-case
2. ReadOnlyLanguage means that the Language implements a pre-defined
   set of expressions (which can be infinite or finite).
   For example the url-iframe Language which directly maps URLs to
   addresses - meaning every well formed URL is an address in this
   Language. Or a potential Language implementing the verbs/predicates
   of a spec like FOAF.

#### Defined in

[language/Language.ts:106](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L106)

## Methods

### get

▸ **get**(`address`): `Promise`<[`Expression`](../classes/Expression.md)\>

Returns an Expression by address, or null if there is no Expression
with that given address

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

`Promise`<[`Expression`](../classes/Expression.md)\>

#### Defined in

[language/Language.ts:93](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L93)
