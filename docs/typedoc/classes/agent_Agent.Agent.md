[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [agent/Agent](../modules/agent_Agent.md) / Agent

# Class: Agent

[agent/Agent](../modules/agent_Agent.md).Agent

AD4M's representation of an Agent

AD4M Agents are build around DIDs, which are used to identify and authenticate the Agent.
Conceptually, an Agent is regarded as something that can speak and that can listen.

Agents speak by creating Expressions in AD4M Languages which are signed by the Agent's DID key,
And they also speak (broadcast) by putting semantic statements into their public "Agent Perspective".
They listen (can receive messages) through their "direct message Language".

These three aspects are represented by the three fields of this class.

This class is used as format for the Expressions in the Agent language.
Since AD4M treats DID URIs as addresses for the Agent Language,
DIDs are resolved to Expressions that are objects of this class.
Thus, this is how agents see (other) agents.

## Table of contents

### Constructors

- [constructor](agent_Agent.Agent.md#constructor)

### Properties

- [did](agent_Agent.Agent.md#did)
- [directMessageLanguage](agent_Agent.Agent.md#directmessagelanguage)
- [perspective](agent_Agent.Agent.md#perspective)

## Constructors

### constructor

• **new Agent**(`did`, `perspective?`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `did` | `string` |
| `perspective?` | [`Perspective`](perspectives_Perspective.Perspective.md) |

#### Defined in

[agent/Agent.ts:42](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/Agent.ts#L42)

## Properties

### did

• **did**: `string`

The DID of the Agent
All epxressions authored by them are signed with the keys mentioned
in the DID document behind this DID URI.

#### Defined in

[agent/Agent.ts:28](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/Agent.ts#L28)

___

### directMessageLanguage

• `Optional` **directMessageLanguage**: `string`

Address of the Language by which the Agent will receive DMs

#### Defined in

[agent/Agent.ts:40](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/Agent.ts#L40)

___

### perspective

• `Optional` **perspective**: [`Perspective`](perspectives_Perspective.Perspective.md)

The Perspective that holds the public-facing semantics/statements of the Agent 
Holds and shares a Perspective that links all information
this agent wants to offer as public-facing semantics.
This should be used for any kind of user profile information.

#### Defined in

[agent/Agent.ts:36](https://github.com/perspect3vism/ad4m/blob/6c5aaad/src/agent/Agent.ts#L36)
