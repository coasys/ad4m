[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/Language](../modules/language_Language.md) / TelepresenceAdapter

# Interface: TelepresenceAdapter

[language/Language](../modules/language_Language.md).TelepresenceAdapter

## Table of contents

### Methods

- [getOnlineAgents](language_Language.TelepresenceAdapter.md#getonlineagents)
- [registerSignalCallback](language_Language.TelepresenceAdapter.md#registersignalcallback)
- [sendBroadcast](language_Language.TelepresenceAdapter.md#sendbroadcast)
- [sendSignal](language_Language.TelepresenceAdapter.md#sendsignal)
- [setOnlineStatus](language_Language.TelepresenceAdapter.md#setonlinestatus)

## Methods

### getOnlineAgents

▸ **getOnlineAgents**(): `Promise`<[`OnlineAgent`](../classes/language_Language.OnlineAgent.md)[]\>

#### Returns

`Promise`<[`OnlineAgent`](../classes/language_Language.OnlineAgent.md)[]\>

#### Defined in

[language/Language.ts:256](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L256)

___

### registerSignalCallback

▸ **registerSignalCallback**(`callback`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`TelepresenceSignalCallback`](../modules/language_Language.md#telepresencesignalcallback) |

#### Returns

`Promise`<`void`\>

#### Defined in

[language/Language.ts:260](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L260)

___

### sendBroadcast

▸ **sendBroadcast**(`payload`): `Promise`<`object`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md) |

#### Returns

`Promise`<`object`\>

#### Defined in

[language/Language.ts:259](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L259)

___

### sendSignal

▸ **sendSignal**(`remoteAgentDid`, `payload`): `Promise`<`object`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `remoteAgentDid` | `string` |
| `payload` | [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md) |

#### Returns

`Promise`<`object`\>

#### Defined in

[language/Language.ts:258](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L258)

___

### setOnlineStatus

▸ **setOnlineStatus**(`status`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`PerspectiveExpression`](../classes/perspectives_Perspective.PerspectiveExpression.md) |

#### Returns

`Promise`<`void`\>

#### Defined in

[language/Language.ts:255](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L255)
