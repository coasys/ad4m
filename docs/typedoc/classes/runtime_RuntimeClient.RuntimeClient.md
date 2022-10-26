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

[runtime/RuntimeClient.ts:28](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L28)

## Properties

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[runtime/RuntimeClient.ts:24](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L24)

___

### #exceptionOccurredCallbacks

• `Private` **#exceptionOccurredCallbacks**: [`ExceptionCallback`](../modules/runtime_RuntimeClient.md#exceptioncallback)[]

#### Defined in

[runtime/RuntimeClient.ts:26](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L26)

___

### #messageReceivedCallbacks

• `Private` **#messageReceivedCallbacks**: [`MessageCallback`](../modules/runtime_RuntimeClient.md#messagecallback)[]

#### Defined in

[runtime/RuntimeClient.ts:25](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L25)

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

[runtime/RuntimeClient.ts:258](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L258)

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

[runtime/RuntimeClient.ts:126](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L126)

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

[runtime/RuntimeClient.ts:97](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L97)

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

[runtime/RuntimeClient.ts:239](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L239)

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

[runtime/RuntimeClient.ts:68](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L68)

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

[runtime/RuntimeClient.ts:78](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L78)

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

[runtime/RuntimeClient.ts:204](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L204)

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

[runtime/RuntimeClient.ts:194](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L194)

___

### friends

▸ **friends**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:146](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L146)

___

### getTrustedAgents

▸ **getTrustedAgents**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:88](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L88)

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

[runtime/RuntimeClient.ts:164](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L164)

___

### hcAgentInfos

▸ **hcAgentInfos**(): `Promise`<`String`\>

#### Returns

`Promise`<`String`\>

#### Defined in

[runtime/RuntimeClient.ts:155](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L155)

___

### info

▸ **info**(): `Promise`<[`RuntimeInfo`](runtime_RuntimeResolver.RuntimeInfo.md)\>

#### Returns

`Promise`<[`RuntimeInfo`](runtime_RuntimeResolver.RuntimeInfo.md)\>

#### Defined in

[runtime/RuntimeClient.ts:39](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L39)

___

### knownLinkLanguageTemplates

▸ **knownLinkLanguageTemplates**(): `Promise`<`string`[]\>

#### Returns

`Promise`<`string`[]\>

#### Defined in

[runtime/RuntimeClient.ts:117](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L117)

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

[runtime/RuntimeClient.ts:214](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L214)

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

[runtime/RuntimeClient.ts:224](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L224)

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

[runtime/RuntimeClient.ts:58](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L58)

___

### quit

▸ **quit**(): `Promise`<`Boolean`\>

#### Returns

`Promise`<`Boolean`\>

#### Defined in

[runtime/RuntimeClient.ts:50](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L50)

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

[runtime/RuntimeClient.ts:136](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L136)

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

[runtime/RuntimeClient.ts:107](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L107)

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

[runtime/RuntimeClient.ts:184](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L184)

___

### subscribeExceptionOccurred

▸ **subscribeExceptionOccurred**(): `void`

#### Returns

`void`

#### Defined in

[runtime/RuntimeClient.ts:262](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L262)

___

### subscribeMessageReceived

▸ **subscribeMessageReceived**(): `void`

#### Returns

`void`

#### Defined in

[runtime/RuntimeClient.ts:243](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L243)

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

[runtime/RuntimeClient.ts:174](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/runtime/RuntimeClient.ts#L174)
