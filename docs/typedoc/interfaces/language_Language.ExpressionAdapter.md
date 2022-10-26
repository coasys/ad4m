[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/Language](../modules/language_Language.md) / ExpressionAdapter

# Interface: ExpressionAdapter

[language/Language](../modules/language_Language.md).ExpressionAdapter

Interface for the most common Expression Languages

## Table of contents

### Properties

- [putAdapter](language_Language.ExpressionAdapter.md#putadapter)

### Methods

- [get](language_Language.ExpressionAdapter.md#get)

## Properties

### putAdapter

• **putAdapter**: [`PublicSharing`](language_Language.PublicSharing.md) \| [`ReadOnlyLanguage`](language_Language.ReadOnlyLanguage.md)

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

[language/Language.ts:93](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L93)

## Methods

### get

▸ **get**(`address`): `Promise`<[`Expression`](../classes/expression_Expression.Expression.md)\>

Returns an Expression by address, or null if there is no Expression
with that given address

#### Parameters

| Name | Type |
| :------ | :------ |
| `address` | `string` |

#### Returns

`Promise`<[`Expression`](../classes/expression_Expression.Expression.md)\>

#### Defined in

[language/Language.ts:80](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/language/Language.ts#L80)
