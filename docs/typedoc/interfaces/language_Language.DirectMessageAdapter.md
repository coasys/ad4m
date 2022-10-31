[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/Language](../modules/language_Language.md) / DirectMessageAdapter

# Interface: DirectMessageAdapter

[language/Language](../modules/language_Language.md).DirectMessageAdapter

## Table of contents

### Methods

- [addMessageCallback](language_Language.DirectMessageAdapter.md#addmessagecallback)
- [inbox](language_Language.DirectMessageAdapter.md#inbox)
- [recipient](language_Language.DirectMessageAdapter.md#recipient)
- [sendInbox](language_Language.DirectMessageAdapter.md#sendinbox)
- [sendP2P](language_Language.DirectMessageAdapter.md#sendp2p)
- [setStatus](language_Language.DirectMessageAdapter.md#setstatus)
- [status](language_Language.DirectMessageAdapter.md#status)

## Methods

### addMessageCallback

▸ **addMessageCallback**(`callback`): `any`

#### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`MessageCallback`](../modules/language_Language.md#messagecallback) |

#### Returns

`any`

#### Defined in

[language/Language.ts:189](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/language/Language.ts#L189)

___

### inbox

▸ **inbox**(`filter?`): `Promise`<[`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

#### Returns

`Promise`<[`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)[]\>

#### Defined in

[language/Language.ts:188](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/language/Language.ts#L188)

___

### recipient

▸ **recipient**(): `string`

#### Returns

`string`

#### Defined in

[language/Language.ts:181](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/language/Language.ts#L181)

___

### sendInbox

▸ **sendInbox**(`message`): `Promise`<`void` \| [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`Perspective`](../classes/perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`void` \| [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)\>

#### Defined in

[language/Language.ts:185](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/language/Language.ts#L185)

___

### sendP2P

▸ **sendP2P**(`message`): `Promise`<`void` \| [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`Perspective`](../classes/perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`void` \| [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)\>

#### Defined in

[language/Language.ts:184](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/language/Language.ts#L184)

___

### setStatus

▸ **setStatus**(`status`): `any`

#### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md) |

#### Returns

`any`

#### Defined in

[language/Language.ts:187](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/language/Language.ts#L187)

___

### status

▸ **status**(): `Promise`<`void` \| [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)\>

#### Returns

`Promise`<`void` \| [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md)\>

#### Defined in

[language/Language.ts:183](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/language/Language.ts#L183)
