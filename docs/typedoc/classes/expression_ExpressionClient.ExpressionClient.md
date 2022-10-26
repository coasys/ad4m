[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [expression/ExpressionClient](../modules/expression_ExpressionClient.md) / ExpressionClient

# Class: ExpressionClient

[expression/ExpressionClient](../modules/expression_ExpressionClient.md).ExpressionClient

## Table of contents

### Constructors

- [constructor](expression_ExpressionClient.ExpressionClient.md#constructor)

### Properties

- [#apolloClient](expression_ExpressionClient.ExpressionClient.md##apolloclient)

### Methods

- [create](expression_ExpressionClient.ExpressionClient.md#create)
- [get](expression_ExpressionClient.ExpressionClient.md#get)
- [getMany](expression_ExpressionClient.ExpressionClient.md#getmany)
- [getRaw](expression_ExpressionClient.ExpressionClient.md#getraw)
- [interact](expression_ExpressionClient.ExpressionClient.md#interact)
- [interactions](expression_ExpressionClient.ExpressionClient.md#interactions)

## Constructors

### constructor

• **new ExpressionClient**(`client`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `client` | `ApolloClient`<`any`\> |

#### Defined in

[expression/ExpressionClient.ts:9](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionClient.ts#L9)

## Properties

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[expression/ExpressionClient.ts:7](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionClient.ts#L7)

## Methods

### create

▸ **create**(`content`, `languageAddress`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `any` |
| `languageAddress` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[expression/ExpressionClient.ts:65](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionClient.ts#L65)

___

### get

▸ **get**(`url`): `Promise`<[`ExpressionRendered`](expression_Expression.ExpressionRendered.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

`Promise`<[`ExpressionRendered`](expression_Expression.ExpressionRendered.md)\>

#### Defined in

[expression/ExpressionClient.ts:13](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionClient.ts#L13)

___

### getMany

▸ **getMany**(`urls`): `Promise`<[`ExpressionRendered`](expression_Expression.ExpressionRendered.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `urls` | `string`[] |

#### Returns

`Promise`<[`ExpressionRendered`](expression_Expression.ExpressionRendered.md)[]\>

#### Defined in

[expression/ExpressionClient.ts:34](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionClient.ts#L34)

___

### getRaw

▸ **getRaw**(`url`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

`Promise`<`string`\>

#### Defined in

[expression/ExpressionClient.ts:55](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionClient.ts#L55)

___

### interact

▸ **interact**(`url`, `interactionCall`): `Promise`<`string`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |
| `interactionCall` | [`InteractionCall`](language_Language.InteractionCall.md) |

#### Returns

`Promise`<`string`\>

#### Defined in

[expression/ExpressionClient.ts:90](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionClient.ts#L90)

___

### interactions

▸ **interactions**(`url`): `Promise`<[`InteractionMeta`](language_Language.InteractionMeta.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

`Promise`<[`InteractionMeta`](language_Language.InteractionMeta.md)[]\>

#### Defined in

[expression/ExpressionClient.ts:76](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionClient.ts#L76)
