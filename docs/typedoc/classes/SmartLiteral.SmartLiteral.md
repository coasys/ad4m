[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [SmartLiteral](../modules/SmartLiteral.md) / SmartLiteral

# Class: SmartLiteral

[SmartLiteral](../modules/SmartLiteral.md).SmartLiteral

## Table of contents

### Constructors

- [constructor](SmartLiteral.SmartLiteral.md#constructor)

### Properties

- [#base](SmartLiteral.SmartLiteral.md##base)
- [#perspective](SmartLiteral.SmartLiteral.md##perspective)

### Accessors

- [base](SmartLiteral.SmartLiteral.md#base)

### Methods

- [get](SmartLiteral.SmartLiteral.md#get)
- [set](SmartLiteral.SmartLiteral.md#set)
- [create](SmartLiteral.SmartLiteral.md#create)
- [getAllSmartLiterals](SmartLiteral.SmartLiteral.md#getallsmartliterals)
- [isSmartLiteralBase](SmartLiteral.SmartLiteral.md#issmartliteralbase)

## Constructors

### constructor

• **new SmartLiteral**(`perspective`, `base`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md) |
| `base` | `string` |

#### Defined in

[SmartLiteral.ts:23](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L23)

## Properties

### #base

• `Private` **#base**: `string`

#### Defined in

[SmartLiteral.ts:21](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L21)

___

### #perspective

• `Private` **#perspective**: [`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md)

#### Defined in

[SmartLiteral.ts:20](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L20)

## Accessors

### base

• `get` **base**(): `string`

#### Returns

`string`

#### Defined in

[SmartLiteral.ts:28](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L28)

## Methods

### get

▸ **get**(): `Promise`<`any`\>

#### Returns

`Promise`<`any`\>

#### Defined in

[SmartLiteral.ts:54](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L54)

___

### set

▸ **set**(`content`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `content` | `any` |

#### Returns

`Promise`<`void`\>

#### Defined in

[SmartLiteral.ts:67](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L67)

___

### create

▸ `Static` **create**(`perspective`, `literal`): `Promise`<[`SmartLiteral`](SmartLiteral.SmartLiteral.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md) |
| `literal` | `any` |

#### Returns

`Promise`<[`SmartLiteral`](SmartLiteral.SmartLiteral.md)\>

#### Defined in

[SmartLiteral.ts:32](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L32)

___

### getAllSmartLiterals

▸ `Static` **getAllSmartLiterals**(`perspective`): `Promise`<[`SmartLiteral`](SmartLiteral.SmartLiteral.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md) |

#### Returns

`Promise`<[`SmartLiteral`](SmartLiteral.SmartLiteral.md)[]\>

#### Defined in

[SmartLiteral.ts:47](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L47)

___

### isSmartLiteralBase

▸ `Static` **isSmartLiteralBase**(`perspective`, `base`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](perspectives_PerspectiveProxy.PerspectiveProxy.md) |
| `base` | `string` |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[SmartLiteral.ts:39](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/SmartLiteral.ts#L39)
