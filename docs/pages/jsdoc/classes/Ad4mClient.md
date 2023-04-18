[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / Ad4mClient

# Class: Ad4mClient

Client for the Ad4m interface wrapping GraphQL queryies
for convenient use in user facing code.

Aggregates the six sub-clients:
AgentClient, ExpressionClient, LanguageClient,
NeighbourhoodClient, PerspectiveClient and RuntimeClient
for the respective functionality.

## Table of contents

### Constructors

- [constructor](Ad4mClient.md#constructor)

### Properties

- [#agentClient](Ad4mClient.md##agentclient)
- [#apolloClient](Ad4mClient.md##apolloclient)
- [#expressionClient](Ad4mClient.md##expressionclient)
- [#languageClient](Ad4mClient.md##languageclient)
- [#neighbourhoodClient](Ad4mClient.md##neighbourhoodclient)
- [#perspectiveClient](Ad4mClient.md##perspectiveclient)
- [#runtimeClient](Ad4mClient.md##runtimeclient)

### Accessors

- [agent](Ad4mClient.md#agent)
- [expression](Ad4mClient.md#expression)
- [languages](Ad4mClient.md#languages)
- [neighbourhood](Ad4mClient.md#neighbourhood)
- [perspective](Ad4mClient.md#perspective)
- [runtime](Ad4mClient.md#runtime)

## Constructors

### constructor

• **new Ad4mClient**(`client`, `subscribe?`)

#### Parameters

| Name | Type | Default value |
| :------ | :------ | :------ |
| `client` | `ApolloClient`<`any`\> | `undefined` |
| `subscribe` | `boolean` | `true` |

#### Defined in

[Ad4mClient.ts:28](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L28)

## Properties

### #agentClient

• `Private` **#agentClient**: `AgentClient`

#### Defined in

[Ad4mClient.ts:20](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L20)

___

### #apolloClient

• `Private` **#apolloClient**: `ApolloClient`<`any`\>

#### Defined in

[Ad4mClient.ts:19](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L19)

___

### #expressionClient

• `Private` **#expressionClient**: `ExpressionClient`

#### Defined in

[Ad4mClient.ts:21](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L21)

___

### #languageClient

• `Private` **#languageClient**: `LanguageClient`

#### Defined in

[Ad4mClient.ts:22](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L22)

___

### #neighbourhoodClient

• `Private` **#neighbourhoodClient**: `NeighbourhoodClient`

#### Defined in

[Ad4mClient.ts:23](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L23)

___

### #perspectiveClient

• `Private` **#perspectiveClient**: `PerspectiveClient`

#### Defined in

[Ad4mClient.ts:24](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L24)

___

### #runtimeClient

• `Private` **#runtimeClient**: `RuntimeClient`

#### Defined in

[Ad4mClient.ts:25](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L25)

## Accessors

### agent

• `get` **agent**(): `AgentClient`

#### Returns

`AgentClient`

#### Defined in

[Ad4mClient.ts:40](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L40)

___

### expression

• `get` **expression**(): `ExpressionClient`

#### Returns

`ExpressionClient`

#### Defined in

[Ad4mClient.ts:44](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L44)

___

### languages

• `get` **languages**(): `LanguageClient`

#### Returns

`LanguageClient`

#### Defined in

[Ad4mClient.ts:48](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L48)

___

### neighbourhood

• `get` **neighbourhood**(): `NeighbourhoodClient`

#### Returns

`NeighbourhoodClient`

#### Defined in

[Ad4mClient.ts:52](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L52)

___

### perspective

• `get` **perspective**(): `PerspectiveClient`

#### Returns

`PerspectiveClient`

#### Defined in

[Ad4mClient.ts:56](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L56)

___

### runtime

• `get` **runtime**(): `RuntimeClient`

#### Returns

`RuntimeClient`

#### Defined in

[Ad4mClient.ts:60](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Ad4mClient.ts#L60)
