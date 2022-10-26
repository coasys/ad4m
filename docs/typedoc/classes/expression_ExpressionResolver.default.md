[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [expression/ExpressionResolver](../modules/expression_ExpressionResolver.md) / default

# Class: default

[expression/ExpressionResolver](../modules/expression_ExpressionResolver.md).default

## Table of contents

### Constructors

- [constructor](expression_ExpressionResolver.default.md#constructor)

### Methods

- [expression](expression_ExpressionResolver.default.md#expression)
- [expressionCreate](expression_ExpressionResolver.default.md#expressioncreate)
- [expressionInteract](expression_ExpressionResolver.default.md#expressioninteract)
- [expressionInteractions](expression_ExpressionResolver.default.md#expressioninteractions)
- [expressionMany](expression_ExpressionResolver.default.md#expressionmany)
- [expressionRaw](expression_ExpressionResolver.default.md#expressionraw)

## Constructors

### constructor

• **new default**()

## Methods

### expression

▸ **expression**(`url`): [`ExpressionRendered`](expression_Expression.ExpressionRendered.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

[`ExpressionRendered`](expression_Expression.ExpressionRendered.md)

#### Defined in

[expression/ExpressionResolver.ts:16](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionResolver.ts#L16)

___

### expressionCreate

▸ **expressionCreate**(`content`, `languageAddress`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `string` |
| `languageAddress` | `string` |

#### Returns

`string`

#### Defined in

[expression/ExpressionResolver.ts:39](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionResolver.ts#L39)

___

### expressionInteract

▸ **expressionInteract**(`url`, `interactionCall`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |
| `interactionCall` | [`InteractionCall`](language_Language.InteractionCall.md) |

#### Returns

`string`

#### Defined in

[expression/ExpressionResolver.ts:57](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionResolver.ts#L57)

___

### expressionInteractions

▸ **expressionInteractions**(`url`): [`InteractionMeta`](language_Language.InteractionMeta.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

[`InteractionMeta`](language_Language.InteractionMeta.md)[]

#### Defined in

[expression/ExpressionResolver.ts:47](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionResolver.ts#L47)

___

### expressionMany

▸ **expressionMany**(`urls`): [`ExpressionRendered`](expression_Expression.ExpressionRendered.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `urls` | `string`[] |

#### Returns

[`ExpressionRendered`](expression_Expression.ExpressionRendered.md)[]

#### Defined in

[expression/ExpressionResolver.ts:25](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionResolver.ts#L25)

___

### expressionRaw

▸ **expressionRaw**(`url`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

`string`

#### Defined in

[expression/ExpressionResolver.ts:30](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/expression/ExpressionResolver.ts#L30)
