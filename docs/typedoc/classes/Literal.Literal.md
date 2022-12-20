[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [Literal](../modules/Literal.md) / Literal

# Class: Literal

[Literal](../modules/Literal.md).Literal

## Table of contents

### Constructors

- [constructor](Literal.Literal.md#constructor)

### Properties

- [#literal](Literal.Literal.md##literal)
- [#url](Literal.Literal.md##url)

### Methods

- [decodeSingleQuote](Literal.Literal.md#decodesinglequote)
- [encodeSingleQuote](Literal.Literal.md#encodesinglequote)
- [get](Literal.Literal.md#get)
- [toUrl](Literal.Literal.md#tourl)
- [from](Literal.Literal.md#from)
- [fromUrl](Literal.Literal.md#fromurl)

## Constructors

### constructor

• **new Literal**()

## Properties

### #literal

• `Private` `Optional` **#literal**: `any`

#### Defined in

[Literal.ts:2](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/Literal.ts#L2)

___

### #url

• `Private` `Optional` **#url**: `string`

#### Defined in

[Literal.ts:3](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/Literal.ts#L3)

## Methods

### decodeSingleQuote

▸ **decodeSingleQuote**(`input`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `input` | `string` |

#### Returns

`string`

#### Defined in

[Literal.ts:25](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/Literal.ts#L25)

___

### encodeSingleQuote

▸ **encodeSingleQuote**(`input`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `input` | `string` |

#### Returns

`string`

#### Defined in

[Literal.ts:19](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/Literal.ts#L19)

___

### get

▸ **get**(): `any`

#### Returns

`any`

#### Defined in

[Literal.ts:53](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/Literal.ts#L53)

___

### toUrl

▸ **toUrl**(): `string`

#### Returns

`string`

#### Defined in

[Literal.ts:31](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/Literal.ts#L31)

___

### from

▸ `Static` **from**(`literal`): [`Literal`](Literal.Literal.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `literal` | `any` |

#### Returns

[`Literal`](Literal.Literal.md)

#### Defined in

[Literal.ts:13](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/Literal.ts#L13)

___

### fromUrl

▸ `Static` **fromUrl**(`url`): [`Literal`](Literal.Literal.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

[`Literal`](Literal.Literal.md)

#### Defined in

[Literal.ts:5](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/Literal.ts#L5)
