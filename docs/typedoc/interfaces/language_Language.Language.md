[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / [language/Language](../modules/language_Language.md) / Language

# Interface: Language

[language/Language](../modules/language_Language.md).Language

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

- [directMessageAdapter](language_Language.Language.md#directmessageadapter)
- [expressionAdapter](language_Language.Language.md#expressionadapter)
- [expressionUI](language_Language.Language.md#expressionui)
- [getAllAdapter](language_Language.Language.md#getalladapter)
- [getByAuthorAdapter](language_Language.Language.md#getbyauthoradapter)
- [languageAdapter](language_Language.Language.md#languageadapter)
- [linksAdapter](language_Language.Language.md#linksadapter)
- [name](language_Language.Language.md#name)
- [settingsUI](language_Language.Language.md#settingsui)
- [teardown](language_Language.Language.md#teardown)
- [telepresenceAdapter](language_Language.Language.md#telepresenceadapter)

### Methods

- [interactions](language_Language.Language.md#interactions)
- [isImmutableExpression](language_Language.Language.md#isimmutableexpression)

## Properties

### directMessageAdapter

• `Optional` `Readonly` **directMessageAdapter**: [`DirectMessageAdapter`](language_Language.DirectMessageAdapter.md)

Optional adapter for direct messaging between agents

#### Defined in

[language/Language.ts:64](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L64)

___

### expressionAdapter

• `Optional` `Readonly` **expressionAdapter**: [`ExpressionAdapter`](language_Language.ExpressionAdapter.md)

ExpressionAdapter implements means of getting an Expression
by address and putting an expression

#### Defined in

[language/Language.ts:38](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L38)

___

### expressionUI

• `Optional` `Readonly` **expressionUI**: [`ExpressionUI`](language_Language.ExpressionUI.md)

Interface for getting UI/web components for rendering Expressions of this Language

#### Defined in

[language/Language.ts:41](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L41)

___

### getAllAdapter

• `Optional` `Readonly` **getAllAdapter**: [`GetAllAdapter`](language_Language.GetAllAdapter.md)

Optional adapter for getting all Expressions

#### Defined in

[language/Language.ts:61](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L61)

___

### getByAuthorAdapter

• `Optional` `Readonly` **getByAuthorAdapter**: [`GetByAuthorAdapter`](language_Language.GetByAuthorAdapter.md)

Optional adapter for getting Expressions by author

#### Defined in

[language/Language.ts:59](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L59)

___

### languageAdapter

• `Optional` `Readonly` **languageAdapter**: [`LanguageAdapter`](language_Language.LanguageAdapter.md)

Implementation of a Language that defines and stores Languages

#### Defined in

[language/Language.ts:56](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L56)

___

### linksAdapter

• `Optional` `Readonly` **linksAdapter**: [`LinkSyncAdapter`](language_Language.LinkSyncAdapter.md)

Interface of LinkLanguages for the core implementation of Neighbourhoods

#### Defined in

[language/Language.ts:44](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L44)

___

### name

• `Readonly` **name**: `string`

#### Defined in

[language/Language.ts:26](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L26)

___

### settingsUI

• `Optional` `Readonly` **settingsUI**: [`SettingsUI`](language_Language.SettingsUI.md)

Interface for providing UI components for the settings of this Language

#### Defined in

[language/Language.ts:67](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L67)

___

### teardown

• `Optional` `Readonly` **teardown**: () => `void`

#### Type declaration

▸ (): `void`

Optional function to make any cleanup/teardown if your language gets deleting in the ad4m-executor

##### Returns

`void`

#### Defined in

[language/Language.ts:70](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L70)

___

### telepresenceAdapter

• `Optional` `Readonly` **telepresenceAdapter**: [`TelepresenceAdapter`](language_Language.TelepresenceAdapter.md)

Additional Interface of LinkLanguages that support telepresence features, 
that is: 
 - seeing who is online and getting a status
 - sending/receiveing p2p signals to other online agents without affecting
   the shared Perspective of the Neighbourhood
 (see TelepresenceAdapter for more details)

#### Defined in

[language/Language.ts:53](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L53)

## Methods

### interactions

▸ **interactions**(`expression`): [`Interaction`](language_Language.Interaction.md)[]

All available interactions this agent could execute on given expression

#### Parameters

| Name | Type |
| :------ | :------ |
| `expression` | `string` |

#### Returns

[`Interaction`](language_Language.Interaction.md)[]

#### Defined in

[language/Language.ts:73](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L73)

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

[language/Language.ts:31](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/language/Language.ts#L31)
