[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / DirectMessageAdapter

# Interface: DirectMessageAdapter

## Table of contents

### Methods

- [addMessageCallback](DirectMessageAdapter.md#addmessagecallback)
- [inbox](DirectMessageAdapter.md#inbox)
- [recipient](DirectMessageAdapter.md#recipient)
- [sendInbox](DirectMessageAdapter.md#sendinbox)
- [sendP2P](DirectMessageAdapter.md#sendp2p)
- [setStatus](DirectMessageAdapter.md#setstatus)
- [status](DirectMessageAdapter.md#status)

## Methods

### addMessageCallback

▸ **addMessageCallback**(`callback`): `any`

#### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`MessageCallback`](../modules.md#messagecallback) |

#### Returns

`any`

#### Defined in

[language/Language.ts:203](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L203)

___

### inbox

▸ **inbox**(`filter?`): `Promise`<[`PerspectiveExpression`](../classes/PerspectiveExpression.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

#### Returns

`Promise`<[`PerspectiveExpression`](../classes/PerspectiveExpression.md)[]\>

#### Defined in

[language/Language.ts:202](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L202)

___

### recipient

▸ **recipient**(): `string`

#### Returns

`string`

#### Defined in

[language/Language.ts:195](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L195)

___

### sendInbox

▸ **sendInbox**(`message`): `Promise`<`void` \| [`PerspectiveExpression`](../classes/PerspectiveExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`Perspective`](../classes/Perspective.md) |

#### Returns

`Promise`<`void` \| [`PerspectiveExpression`](../classes/PerspectiveExpression.md)\>

#### Defined in

[language/Language.ts:199](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L199)

___

### sendP2P

▸ **sendP2P**(`message`): `Promise`<`void` \| [`PerspectiveExpression`](../classes/PerspectiveExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`Perspective`](../classes/Perspective.md) |

#### Returns

`Promise`<`void` \| [`PerspectiveExpression`](../classes/PerspectiveExpression.md)\>

#### Defined in

[language/Language.ts:198](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L198)

___

### setStatus

▸ **setStatus**(`status`): `any`

#### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`PerspectiveExpression`](../classes/PerspectiveExpression.md) |

#### Returns

`any`

#### Defined in

[language/Language.ts:201](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L201)

___

### status

▸ **status**(): `Promise`<`void` \| [`PerspectiveExpression`](../classes/PerspectiveExpression.md)\>

#### Returns

`Promise`<`void` \| [`PerspectiveExpression`](../classes/PerspectiveExpression.md)\>

#### Defined in

[language/Language.ts:197](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L197)
