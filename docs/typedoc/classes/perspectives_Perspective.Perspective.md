[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [perspectives/Perspective](../modules/perspectives_Perspective.md) / Perspective

# Class: Perspective

[perspectives/Perspective](../modules/perspectives_Perspective.md).Perspective

A Perspective represents subjective meaning, encoded through
associations between expressions, a.k.a. Links, that is a graph
over the objective Expressions of any subset of Languages.

This type represents the clean onotological concept of a Perspective.
An instance of this class can be regarded as an immutable snapshot of 
a mutable perspective.

The types PerspectiveProxy and PerspectiveHandle are used when dealing 
with an instantiated mutable perspective as is done through most of 
the GraphQL mutations.

## Table of contents

### Constructors

- [constructor](perspectives_Perspective.Perspective.md#constructor)

### Properties

- [links](perspectives_Perspective.Perspective.md#links)

### Methods

- [get](perspectives_Perspective.Perspective.md#get)
- [getSingleTarget](perspectives_Perspective.Perspective.md#getsingletarget)

## Constructors

### constructor

• **new Perspective**(`links?`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `links?` | [`LinkExpression`](links_Links.LinkExpression.md)[] |

#### Defined in

[perspectives/Perspective.ts:24](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/Perspective.ts#L24)

## Properties

### links

• **links**: [`LinkExpression`](links_Links.LinkExpression.md)[]

The content of the perspective, a list/graph of links

#### Defined in

[perspectives/Perspective.ts:22](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/Perspective.ts#L22)

## Methods

### get

▸ **get**(`query`): [`LinkExpression`](links_Links.LinkExpression.md)[]

Convenience function for filtering links just like with PerspectiveProxy

#### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](perspectives_LinkQuery.LinkQuery.md) |

#### Returns

[`LinkExpression`](links_Links.LinkExpression.md)[]

#### Defined in

[perspectives/Perspective.ts:33](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/Perspective.ts#L33)

___

### getSingleTarget

▸ **getSingleTarget**(`query`): `string` \| `void`

Convenience function to get the target of the first link that matches the given query
This makes sense when the query is expected to return only one link
and the target of that link is what you are looking for.

#### Parameters

| Name | Type |
| :------ | :------ |
| `query` | [`LinkQuery`](perspectives_LinkQuery.LinkQuery.md) |

#### Returns

`string` \| `void`

#### Defined in

[perspectives/Perspective.ts:81](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/perspectives/Perspective.ts#L81)
