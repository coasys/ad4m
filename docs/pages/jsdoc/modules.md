[@perspect3vism/ad4m](README.md) / Exports

# @perspect3vism/ad4m

## Table of contents

### Enumerations

- [ExceptionType](enums/ExceptionType.md)
- [PerspectiveState](enums/PerspectiveState.md)

### Classes

- [Ad4mClient](classes/Ad4mClient.md)
- [Agent](classes/Agent.md)
- [AgentExpression](classes/AgentExpression.md)
- [AgentSignature](classes/AgentSignature.md)
- [AgentStatus](classes/AgentStatus.md)
- [Apps](classes/Apps.md)
- [AuthInfo](classes/AuthInfo.md)
- [AuthInfoInput](classes/AuthInfoInput.md)
- [Capability](classes/Capability.md)
- [CapabilityInput](classes/CapabilityInput.md)
- [Dna](classes/Dna.md)
- [EntanglementProof](classes/EntanglementProof.md)
- [EntanglementProofInput](classes/EntanglementProofInput.md)
- [Expression](classes/Expression.md)
- [ExpressionProof](classes/ExpressionProof.md)
- [ExpressionProofInput](classes/ExpressionProofInput.md)
- [ExpressionRef](classes/ExpressionRef.md)
- [ExpressionRendered](classes/ExpressionRendered.md)
- [Icon](classes/Icon.md)
- [InteractionCall](classes/InteractionCall.md)
- [InteractionMeta](classes/InteractionMeta.md)
- [InteractionParameter](classes/InteractionParameter.md)
- [LanguageExpression](classes/LanguageExpression.md)
- [LanguageHandle](classes/LanguageHandle.md)
- [LanguageLanguageInput](classes/LanguageLanguageInput.md)
- [LanguageMeta](classes/LanguageMeta.md)
- [LanguageMetaInput](classes/LanguageMetaInput.md)
- [LanguageMetaInternal](classes/LanguageMetaInternal.md)
- [LanguageRef](classes/LanguageRef.md)
- [Link](classes/Link.md)
- [LinkExpression](classes/LinkExpression.md)
- [LinkExpressionInput](classes/LinkExpressionInput.md)
- [LinkExpressionMutations](classes/LinkExpressionMutations.md)
- [LinkExpressionUpdated](classes/LinkExpressionUpdated.md)
- [LinkInput](classes/LinkInput.md)
- [LinkMutations](classes/LinkMutations.md)
- [LinkQuery](classes/LinkQuery.md)
- [Literal](classes/Literal.md)
- [Neighbourhood](classes/Neighbourhood.md)
- [NeighbourhoodExpression](classes/NeighbourhoodExpression.md)
- [NeighbourhoodProxy](classes/NeighbourhoodProxy.md)
- [OnlineAgent](classes/OnlineAgent.md)
- [Perspective](classes/Perspective.md)
- [PerspectiveAction](classes/PerspectiveAction.md)
- [PerspectiveDiff](classes/PerspectiveDiff.md)
- [PerspectiveDiffExpression](classes/PerspectiveDiffExpression.md)
- [PerspectiveExpression](classes/PerspectiveExpression.md)
- [PerspectiveHandle](classes/PerspectiveHandle.md)
- [PerspectiveInput](classes/PerspectiveInput.md)
- [PerspectiveProxy](classes/PerspectiveProxy.md)
- [PerspectiveUnsignedInput](classes/PerspectiveUnsignedInput.md)
- [Resource](classes/Resource.md)
- [ResourceInput](classes/ResourceInput.md)
- [SmartLiteral](classes/SmartLiteral.md)
- [Subject](classes/Subject.md)
- [SubjectEntity](classes/SubjectEntity.md)

### Interfaces

- [AgentService](interfaces/AgentService.md)
- [DirectMessageAdapter](interfaces/DirectMessageAdapter.md)
- [ExpressionAdapter](interfaces/ExpressionAdapter.md)
- [ExpressionUI](interfaces/ExpressionUI.md)
- [GetAllAdapter](interfaces/GetAllAdapter.md)
- [GetByAuthorAdapter](interfaces/GetByAuthorAdapter.md)
- [HolochainLanguageDelegate](interfaces/HolochainLanguageDelegate.md)
- [Interaction](interfaces/Interaction.md)
- [Language](interfaces/Language.md)
- [LanguageAdapter](interfaces/LanguageAdapter.md)
- [LanguageContext](interfaces/LanguageContext.md)
- [LinkSyncAdapter](interfaces/LinkSyncAdapter.md)
- [PublicSharing](interfaces/PublicSharing.md)
- [ReadOnlyLanguage](interfaces/ReadOnlyLanguage.md)
- [SettingsUI](interfaces/SettingsUI.md)
- [SignaturesService](interfaces/SignaturesService.md)
- [TelepresenceAdapter](interfaces/TelepresenceAdapter.md)

### Type Aliases

- [Ad4mSignalCB](modules.md#ad4msignalcb)
- [Address](modules.md#address)
- [DID](modules.md#did)
- [MessageCallback](modules.md#messagecallback)
- [PerspectiveDiffObserver](modules.md#perspectivediffobserver)
- [QueryPartialEntity](modules.md#querypartialentity)
- [StatusCallback](modules.md#statuscallback)
- [SubjectArray](modules.md#subjectarray)
- [SyncStateChangeObserver](modules.md#syncstatechangeobserver)
- [TelepresenceSignalCallback](modules.md#telepresencesignalcallback)

### Variables

- [SMART\_LITERAL\_CONTENT\_PREDICATE](modules.md#smart_literal_content_predicate)
- [typeDefsString](modules.md#typedefsstring)

### Functions

- [ExpressionGeneric](modules.md#expressiongeneric)
- [ExpressionGenericInput](modules.md#expressiongenericinput)
- [SDNAClass](modules.md#sdnaclass)
- [addLink](modules.md#addlink)
- [capSentence](modules.md#capsentence)
- [exprRef2String](modules.md#exprref2string)
- [formatList](modules.md#formatlist)
- [hasLink](modules.md#haslink)
- [instanceQuery](modules.md#instancequery)
- [isExpression](modules.md#isexpression)
- [isLink](modules.md#islink)
- [linkEqual](modules.md#linkequal)
- [makeRandomPrologAtom](modules.md#makerandomprologatom)
- [parseExprUrl](modules.md#parseexprurl)
- [subjectCollection](modules.md#subjectcollection)
- [subjectFlag](modules.md#subjectflag)
- [subjectProperty](modules.md#subjectproperty)

## Type Aliases

### Ad4mSignalCB

Ƭ **Ad4mSignalCB**: (`signal`: `any`) => `void`

#### Type declaration

▸ (`signal`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `signal` | `any` |

##### Returns

`void`

#### Defined in

[language/LanguageContext.ts:39](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/LanguageContext.ts#L39)

___

### Address

Ƭ **Address**: `string`

#### Defined in

[Address.ts:1](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/Address.ts#L1)

___

### DID

Ƭ **DID**: `string`

#### Defined in

[DID.ts:1](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/DID.ts#L1)

___

### MessageCallback

Ƭ **MessageCallback**: (`message`: [`PerspectiveExpression`](classes/PerspectiveExpression.md)) => `void`

#### Type declaration

▸ (`message`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `message` | [`PerspectiveExpression`](classes/PerspectiveExpression.md) |

##### Returns

`void`

#### Defined in

[language/Language.ts:192](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L192)

___

### PerspectiveDiffObserver

Ƭ **PerspectiveDiffObserver**: (`diff`: [`PerspectiveDiff`](classes/PerspectiveDiff.md)) => `void`

#### Type declaration

▸ (`diff`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `diff` | [`PerspectiveDiff`](classes/PerspectiveDiff.md) |

##### Returns

`void`

#### Defined in

[language/Language.ts:151](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L151)

___

### QueryPartialEntity

Ƭ **QueryPartialEntity**<`T`\>: { [P in keyof T]?: T[P] \| Function }

#### Type parameters

| Name |
| :------ |
| `T` |

#### Defined in

[subject/SubjectEntity.ts:15](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L15)

___

### StatusCallback

Ƭ **StatusCallback**: (`caller`: [`DID`](modules.md#did)) => [`Perspective`](classes/Perspective.md)

#### Type declaration

▸ (`caller`): [`Perspective`](classes/Perspective.md)

##### Parameters

| Name | Type |
| :------ | :------ |
| `caller` | [`DID`](modules.md#did) |

##### Returns

[`Perspective`](classes/Perspective.md)

#### Defined in

[language/Language.ts:193](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L193)

___

### SubjectArray

Ƭ **SubjectArray**<`T`\>: `T`[] \| { `action`: ``"setter"`` \| ``"adder"`` ; `value`: `T`[]  }

#### Type parameters

| Name |
| :------ |
| `T` |

#### Defined in

[subject/SubjectEntity.ts:210](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L210)

___

### SyncStateChangeObserver

Ƭ **SyncStateChangeObserver**: (`state`: [`PerspectiveState`](enums/PerspectiveState.md)) => `void`

#### Type declaration

▸ (`state`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `state` | [`PerspectiveState`](enums/PerspectiveState.md) |

##### Returns

`void`

#### Defined in

[language/Language.ts:152](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L152)

___

### TelepresenceSignalCallback

Ƭ **TelepresenceSignalCallback**: (`payload`: [`PerspectiveExpression`](classes/PerspectiveExpression.md)) => `void`

#### Type declaration

▸ (`payload`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `payload` | [`PerspectiveExpression`](classes/PerspectiveExpression.md) |

##### Returns

`void`

#### Defined in

[language/Language.ts:258](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/language/Language.ts#L258)

## Variables

### SMART\_LITERAL\_CONTENT\_PREDICATE

• `Const` **SMART\_LITERAL\_CONTENT\_PREDICATE**: ``"smart_literal://content"``

#### Defined in

[SmartLiteral.ts:6](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/SmartLiteral.ts#L6)

___

### typeDefsString

• `Const` **typeDefsString**: ``""``

#### Defined in

[typeDefs.ts:6](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/typeDefs.ts#L6)

## Functions

### ExpressionGeneric

▸ **ExpressionGeneric**<`DataType`\>(`DataTypeClass`): `any`

#### Type parameters

| Name |
| :------ |
| `DataType` |

#### Parameters

| Name | Type |
| :------ | :------ |
| `DataTypeClass` | `ClassType`<`DataType`\> |

#### Returns

`any`

#### Defined in

[expression/Expression.ts:42](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/expression/Expression.ts#L42)

___

### ExpressionGenericInput

▸ **ExpressionGenericInput**<`DataType`\>(`DataTypeClass`): `any`

#### Type parameters

| Name |
| :------ |
| `DataType` |

#### Parameters

| Name | Type |
| :------ | :------ |
| `DataTypeClass` | `ClassType`<`DataType`\> |

#### Returns

`any`

#### Defined in

[expression/Expression.ts:67](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/expression/Expression.ts#L67)

___

### SDNAClass

▸ **SDNAClass**(`opts`): (`target`: `any`) => `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `opts` | `SDNAClassOptions` |

#### Returns

`fn`

▸ (`target`): `void`

##### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `any` |

##### Returns

`void`

#### Defined in

[subject/SDNADecorators.ts:156](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SDNADecorators.ts#L156)

___

### addLink

▸ **addLink**(`source`, `predicate`, `target`): [`PerspectiveAction`](classes/PerspectiveAction.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `source` | `string` |
| `predicate` | `string` |
| `target` | `string` |

#### Returns

[`PerspectiveAction`](classes/PerspectiveAction.md)

#### Defined in

[subject/SDNADecorators.ts:12](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SDNADecorators.ts#L12)

___

### capSentence

▸ **capSentence**(`cap`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `cap` | `any` |

#### Returns

`string`

#### Defined in

[utils.ts:15](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/utils.ts#L15)

___

### exprRef2String

▸ **exprRef2String**(`ref`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `ref` | [`ExpressionRef`](classes/ExpressionRef.md) |

#### Returns

`string`

#### Defined in

[expression/ExpressionRef.ts:22](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/expression/ExpressionRef.ts#L22)

___

### formatList

▸ **formatList**(`list`): `any`

#### Parameters

| Name | Type |
| :------ | :------ |
| `list` | `any` |

#### Returns

`any`

#### Defined in

[utils.ts:1](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/utils.ts#L1)

___

### hasLink

▸ **hasLink**(`predicate`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `predicate` | `string` |

#### Returns

`string`

#### Defined in

[subject/SDNADecorators.ts:21](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SDNADecorators.ts#L21)

___

### instanceQuery

▸ **instanceQuery**(`options?`): <T\>(`target`: `T`, `key`: keyof `T`, `descriptor`: `PropertyDescriptor`) => `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `options?` | `InstanceQueryParams` |

#### Returns

`fn`

▸ <`T`\>(`target`, `key`, `descriptor`): `void`

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `T` |
| `key` | keyof `T` |
| `descriptor` | `PropertyDescriptor` |

##### Returns

`void`

#### Defined in

[subject/SDNADecorators.ts:30](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SDNADecorators.ts#L30)

___

### isExpression

▸ **isExpression**(`e`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `e` | `any` |

#### Returns

`boolean`

#### Defined in

[expression/Expression.ts:97](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/expression/Expression.ts#L97)

___

### isLink

▸ **isLink**(`l`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `l` | `any` |

#### Returns

`boolean`

#### Defined in

[links/Links.ts:82](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/links/Links.ts#L82)

___

### linkEqual

▸ **linkEqual**(`l1`, `l2`): `boolean`

#### Parameters

| Name | Type |
| :------ | :------ |
| `l1` | [`LinkExpression`](classes/LinkExpression.md) |
| `l2` | [`LinkExpression`](classes/LinkExpression.md) |

#### Returns

`boolean`

#### Defined in

[links/Links.ts:74](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/links/Links.ts#L74)

___

### makeRandomPrologAtom

▸ **makeRandomPrologAtom**(`length`): `string`

#### Parameters

| Name | Type |
| :------ | :------ |
| `length` | `number` |

#### Returns

`string`

#### Defined in

[subject/SDNADecorators.ts:142](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SDNADecorators.ts#L142)

___

### parseExprUrl

▸ **parseExprUrl**(`url`): [`ExpressionRef`](classes/ExpressionRef.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `url` | `string` |

#### Returns

[`ExpressionRef`](classes/ExpressionRef.md)

#### Defined in

[expression/ExpressionRef.ts:29](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/expression/ExpressionRef.ts#L29)

___

### subjectCollection

▸ **subjectCollection**(`opts`): <T\>(`target`: `T`, `key`: keyof `T`) => `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `opts` | `CollectionOptions` |

#### Returns

`fn`

▸ <`T`\>(`target`, `key`): `void`

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `T` |
| `key` | keyof `T` |

##### Returns

`void`

#### Defined in

[subject/SDNADecorators.ts:129](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SDNADecorators.ts#L129)

___

### subjectFlag

▸ **subjectFlag**(`opts`): <T\>(`target`: `T`, `key`: keyof `T`) => `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `opts` | `FlagOptions` |

#### Returns

`fn`

▸ <`T`\>(`target`, `key`): `void`

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `T` |
| `key` | keyof `T` |

##### Returns

`void`

#### Defined in

[subject/SDNADecorators.ts:101](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SDNADecorators.ts#L101)

___

### subjectProperty

▸ **subjectProperty**(`opts`): <T\>(`target`: `T`, `key`: keyof `T`) => `void`

#### Parameters

| Name | Type |
| :------ | :------ |
| `opts` | `PropertyOptions` |

#### Returns

`fn`

▸ <`T`\>(`target`, `key`): `void`

##### Type parameters

| Name |
| :------ |
| `T` |

##### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `T` |
| `key` | keyof `T` |

##### Returns

`void`

#### Defined in

[subject/SDNADecorators.ts:82](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SDNADecorators.ts#L82)
