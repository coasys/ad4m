[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / TelepresenceAdapter

# Interface: TelepresenceAdapter

## Table of contents

### Methods

- [getOnlineAgents](TelepresenceAdapter.md#getonlineagents)
- [registerSignalCallback](TelepresenceAdapter.md#registersignalcallback)
- [sendBroadcast](TelepresenceAdapter.md#sendbroadcast)
- [sendSignal](TelepresenceAdapter.md#sendsignal)
- [setOnlineStatus](TelepresenceAdapter.md#setonlinestatus)

## Methods

### getOnlineAgents

▸ **getOnlineAgents**(): `Promise`<[`OnlineAgent`](../classes/OnlineAgent.md)[]\>

#### Returns

`Promise`<[`OnlineAgent`](../classes/OnlineAgent.md)[]\>

#### Defined in

[language/Language.ts:261](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L261)

___

### registerSignalCallback

▸ **registerSignalCallback**(`callback`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `callback` | [`TelepresenceSignalCallback`](../modules.md#telepresencesignalcallback) |

#### Returns

`Promise`<`void`\>

#### Defined in

[language/Language.ts:265](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L265)

___

### sendBroadcast

▸ **sendBroadcast**(`payload`): `Promise`<`object`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`PerspectiveExpression`](../classes/PerspectiveExpression.md) |

#### Returns

`Promise`<`object`\>

#### Defined in

[language/Language.ts:264](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L264)

___

### sendSignal

▸ **sendSignal**(`remoteAgentDid`, `payload`): `Promise`<`object`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `remoteAgentDid` | `string` |
| `payload` | [`PerspectiveExpression`](../classes/PerspectiveExpression.md) |

#### Returns

`Promise`<`object`\>

#### Defined in

[language/Language.ts:263](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L263)

___

### setOnlineStatus

▸ **setOnlineStatus**(`status`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`PerspectiveExpression`](../classes/PerspectiveExpression.md) |

#### Returns

`Promise`<`void`\>

#### Defined in

[language/Language.ts:260](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L260)
