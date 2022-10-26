[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [runtime/RuntimeResolver](../modules/runtime_RuntimeResolver.md) / default

# Class: default

[runtime/RuntimeResolver](../modules/runtime_RuntimeResolver.md).default

Resolver classes are used here to define the GraphQL schema 
(through the type-graphql annotations)
and are spawned in the client tests in Ad4mClient.test.ts.
For the latter, they return test fixtures.

## Table of contents

### Constructors

- [constructor](runtime_RuntimeResolver.default.md#constructor)

### Methods

- [addTrustedAgents](runtime_RuntimeResolver.default.md#addtrustedagents)
- [deleteTrustedAgents](runtime_RuntimeResolver.default.md#deletetrustedagents)
- [exceptionOccurred](runtime_RuntimeResolver.default.md#exceptionoccurred)
- [getTrustedAgents](runtime_RuntimeResolver.default.md#gettrustedagents)
- [runtimeAddFriends](runtime_RuntimeResolver.default.md#runtimeaddfriends)
- [runtimeAddKnownLinkLanguageTemplates](runtime_RuntimeResolver.default.md#runtimeaddknownlinklanguagetemplates)
- [runtimeFriendSendMessage](runtime_RuntimeResolver.default.md#runtimefriendsendmessage)
- [runtimeFriendStatus](runtime_RuntimeResolver.default.md#runtimefriendstatus)
- [runtimeFriends](runtime_RuntimeResolver.default.md#runtimefriends)
- [runtimeHcAddAgentInfos](runtime_RuntimeResolver.default.md#runtimehcaddagentinfos)
- [runtimeHcAgentInfos](runtime_RuntimeResolver.default.md#runtimehcagentinfos)
- [runtimeInfo](runtime_RuntimeResolver.default.md#runtimeinfo)
- [runtimeKnownLinkLanguageTemplates](runtime_RuntimeResolver.default.md#runtimeknownlinklanguagetemplates)
- [runtimeMessageInbox](runtime_RuntimeResolver.default.md#runtimemessageinbox)
- [runtimeMessageOutbox](runtime_RuntimeResolver.default.md#runtimemessageoutbox)
- [runtimeMessageReceived](runtime_RuntimeResolver.default.md#runtimemessagereceived)
- [runtimeOpenLink](runtime_RuntimeResolver.default.md#runtimeopenlink)
- [runtimeQuit](runtime_RuntimeResolver.default.md#runtimequit)
- [runtimeRemoveFriends](runtime_RuntimeResolver.default.md#runtimeremovefriends)
- [runtimeRemoveKnownLinkLanguageTemplates](runtime_RuntimeResolver.default.md#runtimeremoveknownlinklanguagetemplates)
- [runtimeSetStatus](runtime_RuntimeResolver.default.md#runtimesetstatus)
- [runtimeVerifyStringSignedByDid](runtime_RuntimeResolver.default.md#runtimeverifystringsignedbydid)

## Constructors

### constructor

• **new default**()

## Methods

### addTrustedAgents

▸ **addTrustedAgents**(`agents`): `string`[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `agents` | `string`[] |

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:79](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L79)

___

### deleteTrustedAgents

▸ **deleteTrustedAgents**(`agents`): `string`[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `agents` | `string`[] |

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:84](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L84)

___

### exceptionOccurred

▸ **exceptionOccurred**(): [`ExceptionInfo`](runtime_RuntimeResolver.ExceptionInfo.md)

#### Returns

[`ExceptionInfo`](runtime_RuntimeResolver.ExceptionInfo.md)

#### Defined in

[runtime/RuntimeResolver.ts:180](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L180)

___

### getTrustedAgents

▸ **getTrustedAgents**(): `string`[]

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:89](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L89)

___

### runtimeAddFriends

▸ **runtimeAddFriends**(`dids`): `string`[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `dids` | `string`[] |

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:114](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L114)

___

### runtimeAddKnownLinkLanguageTemplates

▸ **runtimeAddKnownLinkLanguageTemplates**(`addresses`): `string`[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `addresses` | `string`[] |

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:99](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L99)

___

### runtimeFriendSendMessage

▸ **runtimeFriendSendMessage**(`did`, `message`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `message` | [`PerspectiveInput`](perspectives_Perspective.PerspectiveInput.md) |

#### Returns

`boolean`

#### Defined in

[runtime/RuntimeResolver.ts:153](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L153)

___

### runtimeFriendStatus

▸ **runtimeFriendStatus**(`did`): [`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |

#### Returns

[`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)

#### Defined in

[runtime/RuntimeResolver.ts:148](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L148)

___

### runtimeFriends

▸ **runtimeFriends**(): `string`[]

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:109](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L109)

___

### runtimeHcAddAgentInfos

▸ **runtimeHcAddAgentInfos**(`agentInfos`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `agentInfos` | `any` |

#### Returns

`boolean`

#### Defined in

[runtime/RuntimeResolver.ts:129](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L129)

___

### runtimeHcAgentInfos

▸ **runtimeHcAgentInfos**(): `String`

#### Returns

`String`

#### Defined in

[runtime/RuntimeResolver.ts:124](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L124)

___

### runtimeInfo

▸ **runtimeInfo**(): [`RuntimeInfo`](runtime_RuntimeResolver.RuntimeInfo.md)

#### Returns

[`RuntimeInfo`](runtime_RuntimeResolver.RuntimeInfo.md)

#### Defined in

[runtime/RuntimeResolver.ts:72](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L72)

___

### runtimeKnownLinkLanguageTemplates

▸ **runtimeKnownLinkLanguageTemplates**(): `string`[]

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:94](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L94)

___

### runtimeMessageInbox

▸ **runtimeMessageInbox**(`filter?`): [`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

#### Returns

[`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)[]

#### Defined in

[runtime/RuntimeResolver.ts:161](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L161)

___

### runtimeMessageOutbox

▸ **runtimeMessageOutbox**(`filter?`): [`SentMessage`](runtime_RuntimeResolver.SentMessage.md)[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

#### Returns

[`SentMessage`](runtime_RuntimeResolver.SentMessage.md)[]

#### Defined in

[runtime/RuntimeResolver.ts:166](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L166)

___

### runtimeMessageReceived

▸ **runtimeMessageReceived**(): [`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)

#### Returns

[`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)

#### Defined in

[runtime/RuntimeResolver.ts:175](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L175)

___

### runtimeOpenLink

▸ **runtimeOpenLink**(`url`): `Boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

`Boolean`

#### Defined in

[runtime/RuntimeResolver.ts:67](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L67)

___

### runtimeQuit

▸ **runtimeQuit**(): `Boolean`

#### Returns

`Boolean`

#### Defined in

[runtime/RuntimeResolver.ts:62](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L62)

___

### runtimeRemoveFriends

▸ **runtimeRemoveFriends**(`dids`): `string`[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `dids` | `string`[] |

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:119](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L119)

___

### runtimeRemoveKnownLinkLanguageTemplates

▸ **runtimeRemoveKnownLinkLanguageTemplates**(`addresses`): `string`[]

#### Parameters

| Name | Type |
| :------ | :------ |
| `addresses` | `string`[] |

#### Returns

`string`[]

#### Defined in

[runtime/RuntimeResolver.ts:104](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L104)

___

### runtimeSetStatus

▸ **runtimeSetStatus**(`status`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `status` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`boolean`

#### Defined in

[runtime/RuntimeResolver.ts:143](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L143)

___

### runtimeVerifyStringSignedByDid

▸ **runtimeVerifyStringSignedByDid**(`did`, `didSigningKeyId`, `data`, `signedData`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `didSigningKeyId` | `string` |
| `data` | `string` |
| `signedData` | `string` |

#### Returns

`boolean`

#### Defined in

[runtime/RuntimeResolver.ts:134](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeResolver.ts#L134)
