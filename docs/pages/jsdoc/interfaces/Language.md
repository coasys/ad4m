[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / Language

# Interface: Language

Interface of AD4M Languages

Any JavaScript module that implements a create() function that returns an object that implements this interface
is a valid AD4M language.
So the AD4M-internal representation of a language is an object that implements this interface.

Since there are a few different kinds of languages, this interface is split into optional sub-interfaces.
The only required property is the name of the language.

The most usual kind of language is the "Expression Language", which is a language that can be used to create
and share Expressions.
For that, implement the expressionsAdapter and expressionUI interface.

The second most common kind of language is the "Link Language", which is a language that builds the core
of AD4M Neighbourhoods.
For that, implement the linksAdapter interface.

## Table of contents

### Properties

- [directMessageAdapter](Language.md#directmessageadapter)
- [expressionAdapter](Language.md#expressionadapter)
- [expressionUI](Language.md#expressionui)
- [getAllAdapter](Language.md#getalladapter)
- [getByAuthorAdapter](Language.md#getbyauthoradapter)
- [languageAdapter](Language.md#languageadapter)
- [linksAdapter](Language.md#linksadapter)
- [name](Language.md#name)
- [settingsUI](Language.md#settingsui)
- [teardown](Language.md#teardown)
- [telepresenceAdapter](Language.md#telepresenceadapter)

### Methods

- [interactions](Language.md#interactions)
- [isImmutableExpression](Language.md#isimmutableexpression)

## Properties

### directMessageAdapter

• `Optional` `Readonly` **directMessageAdapter**: [`DirectMessageAdapter`](DirectMessageAdapter.md)

Optional adapter for direct messaging between agents

#### Defined in

[language/Language.ts:65](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L65)

___

### expressionAdapter

• `Optional` `Readonly` **expressionAdapter**: [`ExpressionAdapter`](ExpressionAdapter.md)

ExpressionAdapter implements means of getting an Expression
by address and putting an expression

#### Defined in

[language/Language.ts:39](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L39)

___

### expressionUI

• `Optional` `Readonly` **expressionUI**: [`ExpressionUI`](ExpressionUI.md)

Interface for getting UI/web components for rendering Expressions of this Language

#### Defined in

[language/Language.ts:42](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L42)

___

### getAllAdapter

• `Optional` `Readonly` **getAllAdapter**: [`GetAllAdapter`](GetAllAdapter.md)

Optional adapter for getting all Expressions

#### Defined in

[language/Language.ts:62](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L62)

___

### getByAuthorAdapter

• `Optional` `Readonly` **getByAuthorAdapter**: [`GetByAuthorAdapter`](GetByAuthorAdapter.md)

Optional adapter for getting Expressions by author

#### Defined in

[language/Language.ts:60](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L60)

___

### languageAdapter

• `Optional` `Readonly` **languageAdapter**: [`LanguageAdapter`](LanguageAdapter.md)

Implementation of a Language that defines and stores Languages

#### Defined in

[language/Language.ts:57](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L57)

___

### linksAdapter

• `Optional` `Readonly` **linksAdapter**: [`LinkSyncAdapter`](LinkSyncAdapter.md)

Interface of LinkLanguages for the core implementation of Neighbourhoods

#### Defined in

[language/Language.ts:45](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L45)

___

### name

• `Readonly` **name**: `string`

#### Defined in

[language/Language.ts:27](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L27)

___

### settingsUI

• `Optional` `Readonly` **settingsUI**: [`SettingsUI`](SettingsUI.md)

Interface for providing UI components for the settings of this Language

#### Defined in

[language/Language.ts:68](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L68)

___

### teardown

• `Optional` `Readonly` **teardown**: () => `void`

#### Type declaration

▸ (): `void`

Optional function to make any cleanup/teardown if your language gets deleting in the ad4m-executor

##### Returns

`void`

#### Defined in

[language/Language.ts:71](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L71)

___

### telepresenceAdapter

• `Optional` `Readonly` **telepresenceAdapter**: [`TelepresenceAdapter`](TelepresenceAdapter.md)

Additional Interface of LinkLanguages that support telepresence features, 
that is: 
 - seeing who is online and getting a status
 - sending/receiveing p2p signals to other online agents without affecting
   the shared Perspective of the Neighbourhood
 (see TelepresenceAdapter for more details)

#### Defined in

[language/Language.ts:54](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L54)

## Methods

### interactions

▸ **interactions**(`expression`): [`Interaction`](Interaction.md)[]

All available interactions this agent could execute on given expression

#### Parameters

| Name | Type |
| :------ | :------ |
| `expression` | `string` |

#### Returns

[`Interaction`](Interaction.md)[]

#### Defined in

[language/Language.ts:74](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L74)

___

### isImmutableExpression

▸ `Optional` **isImmutableExpression**(`expression`): `boolean`

Flagging expressions as immutable to enable
expression caching in the ad4m-executor

#### Parameters

| Name | Type |
| :------ | :------ |
| `expression` | `string` |

#### Returns

`boolean`

#### Defined in

[language/Language.ts:32](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L32)
