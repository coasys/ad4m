[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [neighbourhood/NeighbourhoodClient](../modules/neighbourhood_NeighbourhoodClient.md) / NeighbourhoodClient

# Class: NeighbourhoodClient

[neighbourhood/NeighbourhoodClient](../modules/neighbourhood_NeighbourhoodClient.md).NeighbourhoodClient

## Table of contents

### Constructors

- [constructor](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#constructor)

### Properties

- [#apolloClient](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md##apolloclient)

### Methods

- [joinFromUrl](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#joinfromurl)
- [publishFromPerspective](neighbourhood_NeighbourhoodClient.NeighbourhoodClient.md#publishfromperspective)

## Constructors

### constructor

• **new NeighbourhoodClient**(`client`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `client` | `ApolloClient`<`any`\> |

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:10](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/neighbourhood/NeighbourhoodClient.ts#L10)

## Properties

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:8](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/neighbourhood/NeighbourhoodClient.ts#L8)

## Methods

### joinFromUrl

▸ **joinFromUrl**(`url`): `Promise`<[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

`Promise`<[`PerspectiveHandle`](perspectives_PerspectiveHandle.PerspectiveHandle.md)\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:36](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/neighbourhood/NeighbourhoodClient.ts#L36)

___

### publishFromPerspective

▸ **publishFromPerspective**(`perspectiveUUID`, `linkLanguage`, `meta`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspectiveUUID` | `string` |
| `linkLanguage` | `string` |
| `meta` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`string`\>

#### Defined in

[neighbourhood/NeighbourhoodClient.ts:14](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/neighbourhood/NeighbourhoodClient.ts#L14)
