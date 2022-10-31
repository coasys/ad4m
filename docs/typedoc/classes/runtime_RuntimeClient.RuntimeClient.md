[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [runtime/RuntimeClient](../modules/runtime_RuntimeClient.md) / RuntimeClient

# Class: RuntimeClient

[runtime/RuntimeClient](../modules/runtime_RuntimeClient.md).RuntimeClient

## Table of contents

### Constructors

- [constructor](runtime_RuntimeClient.RuntimeClient.md#constructor)

### Properties

- [#apolloClient](runtime_RuntimeClient.RuntimeClient.md##apolloclient)
- [#exceptionOccurredCallbacks](runtime_RuntimeClient.RuntimeClient.md##exceptionoccurredcallbacks)
- [#messageReceivedCallbacks](runtime_RuntimeClient.RuntimeClient.md##messagereceivedcallbacks)

### Methods

- [addExceptionCallback](runtime_RuntimeClient.RuntimeClient.md#addexceptioncallback)
- [addFriends](runtime_RuntimeClient.RuntimeClient.md#addfriends)
- [addKnownLinkLanguageTemplates](runtime_RuntimeClient.RuntimeClient.md#addknownlinklanguagetemplates)
- [addMessageCallback](runtime_RuntimeClient.RuntimeClient.md#addmessagecallback)
- [addTrustedAgents](runtime_RuntimeClient.RuntimeClient.md#addtrustedagents)
- [deleteTrustedAgents](runtime_RuntimeClient.RuntimeClient.md#deletetrustedagents)
- [friendSendMessage](runtime_RuntimeClient.RuntimeClient.md#friendsendmessage)
- [friendStatus](runtime_RuntimeClient.RuntimeClient.md#friendstatus)
- [friends](runtime_RuntimeClient.RuntimeClient.md#friends)
- [getTrustedAgents](runtime_RuntimeClient.RuntimeClient.md#gettrustedagents)
- [hcAddAgentInfos](runtime_RuntimeClient.RuntimeClient.md#hcaddagentinfos)
- [hcAgentInfos](runtime_RuntimeClient.RuntimeClient.md#hcagentinfos)
- [info](runtime_RuntimeClient.RuntimeClient.md#info)
- [knownLinkLanguageTemplates](runtime_RuntimeClient.RuntimeClient.md#knownlinklanguagetemplates)
- [messageInbox](runtime_RuntimeClient.RuntimeClient.md#messageinbox)
- [messageOutbox](runtime_RuntimeClient.RuntimeClient.md#messageoutbox)
- [openLink](runtime_RuntimeClient.RuntimeClient.md#openlink)
- [quit](runtime_RuntimeClient.RuntimeClient.md#quit)
- [removeFriends](runtime_RuntimeClient.RuntimeClient.md#removefriends)
- [removeKnownLinkLanguageTemplates](runtime_RuntimeClient.RuntimeClient.md#removeknownlinklanguagetemplates)
- [setStatus](runtime_RuntimeClient.RuntimeClient.md#setstatus)
- [subscribeExceptionOccurred](runtime_RuntimeClient.RuntimeClient.md#subscribeexceptionoccurred)
- [subscribeMessageReceived](runtime_RuntimeClient.RuntimeClient.md#subscribemessagereceived)
- [verifyStringSignedByDid](runtime_RuntimeClient.RuntimeClient.md#verifystringsignedbydid)

## Constructors

### constructor

• **new RuntimeClient**(`client`, `subscribe?`)

#### Parameters

| Name | Type | Default value |
| :------ | :------ | :------ |
| `client` | `ApolloClient`<`any`\> | `undefined` |
| `subscribe` | `boolean` | `true` |

#### Defined in

[runtime/RuntimeClient.ts:28](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L28)

## Properties

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[runtime/RuntimeClient.ts:24](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L24)

___

### #exceptionOccurredCallbacks

• `Private` **#exceptionOccurredCallbacks**: [`ExceptionCallback`](../modules/runtime_RuntimeClient.md#exceptioncallback)[]

#### Defined in

[runtime/RuntimeClient.ts:26](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L26)

___

### #messageReceivedCallbacks

• `Private` **#messageReceivedCallbacks**: [`MessageCallback`](../modules/runtime_RuntimeClient.md#messagecallback)[]

#### Defined in

[runtime/RuntimeClient.ts:25](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L25)

## Methods

### addExceptionCallback

▸ **addExceptionCallback**(`cb`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`ExceptionCallback`](../modules/runtime_RuntimeClient.md#exceptioncallback) |

#### Returns

`void`

#### Defined in

[runtime/RuntimeClient.ts:260](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L260)

___

### addFriends

▸ **addFriends**(`dids`): `Promise`<`string`[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `dids` | `string`[] |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:128](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L128)

___

### addKnownLinkLanguageTemplates

▸ **addKnownLinkLanguageTemplates**(`addresses`): `Promise`<`string`[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `addresses` | `string`[] |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:99](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L99)

___

### addMessageCallback

▸ **addMessageCallback**(`cb`): `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `cb` | [`MessageCallback`](../modules/runtime_RuntimeClient.md#messagecallback) |

#### Returns

`void`

#### Defined in

[runtime/RuntimeClient.ts:241](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L241)

___

### addTrustedAgents

▸ **addTrustedAgents**(`agents`): `Promise`<`string`[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `agents` | `string`[] |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:70](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L70)

___

### deleteTrustedAgents

▸ **deleteTrustedAgents**(`agents`): `Promise`<`string`[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `agents` | `string`[] |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:80](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L80)

___

### friendSendMessage

▸ **friendSendMessage**(`did`, `message`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `message` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[runtime/RuntimeClient.ts:206](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L206)

___

### friendStatus

▸ **friendStatus**(`did`): `Promise`<[`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |

#### Returns

`Promise`<[`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)\>

#### Defined in

[runtime/RuntimeClient.ts:196](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L196)

___

### friends

▸ **friends**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:148](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L148)

___

### getTrustedAgents

▸ **getTrustedAgents**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:90](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L90)

___

### hcAddAgentInfos

▸ **hcAddAgentInfos**(`agentInfos`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `agentInfos` | `String` |

#### Returns

`Promise`<`void`\>

#### Defined in

[runtime/RuntimeClient.ts:166](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L166)

___

### hcAgentInfos

▸ **hcAgentInfos**(): `Promise`<`String`\>

#### Returns

`Promise`<`String`\>

#### Defined in

[runtime/RuntimeClient.ts:157](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L157)

___

### info

▸ **info**(): `Promise`<[`RuntimeInfo`](runtime_RuntimeResolver.RuntimeInfo.md)\>

#### Returns

`Promise`<[`RuntimeInfo`](runtime_RuntimeResolver.RuntimeInfo.md)\>

#### Defined in

[runtime/RuntimeClient.ts:39](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L39)

___

### knownLinkLanguageTemplates

▸ **knownLinkLanguageTemplates**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:119](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L119)

___

### messageInbox

▸ **messageInbox**(`filter?`): `Promise`<[`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

#### Returns

`Promise`<[`PerspectiveExpression`](perspectives_Perspective.PerspectiveExpression.md)[]\>

#### Defined in

[runtime/RuntimeClient.ts:216](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L216)

___

### messageOutbox

▸ **messageOutbox**(`filter?`): `Promise`<[`SentMessage`](runtime_RuntimeResolver.SentMessage.md)[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `filter?` | `string` |

#### Returns

`Promise`<[`SentMessage`](runtime_RuntimeResolver.SentMessage.md)[]\>

#### Defined in

[runtime/RuntimeClient.ts:226](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L226)

___

### openLink

▸ **openLink**(`url`): `Promise`<`Boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

`Promise`<`Boolean`\>

#### Defined in

[runtime/RuntimeClient.ts:60](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L60)

___

### quit

▸ **quit**(): `Promise`<`Boolean`\>

#### Returns

`Promise`<`Boolean`\>

#### Defined in

[runtime/RuntimeClient.ts:52](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L52)

___

### removeFriends

▸ **removeFriends**(`dids`): `Promise`<`string`[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `dids` | `string`[] |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:138](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L138)

___

### removeKnownLinkLanguageTemplates

▸ **removeKnownLinkLanguageTemplates**(`addresses`): `Promise`<`string`[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `addresses` | `string`[] |

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:109](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L109)

___

### setStatus

▸ **setStatus**(`perspective`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[runtime/RuntimeClient.ts:186](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L186)

___

### subscribeExceptionOccurred

▸ **subscribeExceptionOccurred**(): `void`

#### Returns

`void`

#### Defined in

[runtime/RuntimeClient.ts:264](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L264)

___

### subscribeMessageReceived

▸ **subscribeMessageReceived**(): `void`

#### Returns

`void`

#### Defined in

[runtime/RuntimeClient.ts:245](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L245)

___

### verifyStringSignedByDid

▸ **verifyStringSignedByDid**(`did`, `didSigningKeyId`, `data`, `signedData`): `Promise`<`boolean`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `didSigningKeyId` | `string` |
| `data` | `string` |
| `signedData` | `string` |

#### Returns

`Promise`<`boolean`\>

#### Defined in

[runtime/RuntimeClient.ts:176](https://github.com/perspect3vism/ad4m-executor/blob/5a19b63d/core/src/runtime/RuntimeClient.ts#L176)
