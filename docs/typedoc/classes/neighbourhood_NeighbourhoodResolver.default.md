[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [neighbourhood/NeighbourhoodResolver](../modules/neighbourhood_NeighbourhoodResolver.md) / default

# Class: default

[neighbourhood/NeighbourhoodResolver](../modules/neighbourhood_NeighbourhoodResolver.md).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

## Table of contents

### Constructors

- [constructor](neighbourhood_NeighbourhoodResolver.default.md#constructor)

### Methods

- [neighbourhoodJoinFromUrl](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodjoinfromurl)
- [neighbourhoodPublishFromPerspective](neighbourhood_NeighbourhoodResolver.default.md#neighbourhoodpublishfromperspective)

## Constructors

### constructor

• **new default**()

## Methods

### neighbourhoodJoinFromUrl

▸ **neighbourhoodJoinFromUrl**(`url`): [`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:24](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/neighbourhood/NeighbourhoodResolver.ts#L24)

___

### neighbourhoodPublishFromPerspective

▸ **neighbourhoodPublishFromPerspective**(`perspectiveUUID`, `linkLanguage`, `meta`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `linkLanguage` | `string` |
| `meta` | [`PerspectiveInput`](perspectives_Perspective.PerspectiveInput.md) |

#### Returns

`string`

#### Defined in

[neighbourhood/NeighbourhoodResolver.ts:15](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/neighbourhood/NeighbourhoodResolver.ts#L15)
