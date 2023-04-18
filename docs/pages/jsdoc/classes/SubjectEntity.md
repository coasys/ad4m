[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / SubjectEntity

# Class: SubjectEntity

## Table of contents

### Constructors

- [constructor](SubjectEntity.md#constructor)

### Properties

- [#baseExpression](SubjectEntity.md##baseexpression)
- [#perspective](SubjectEntity.md##perspective)
- [#subjectClass](SubjectEntity.md##subjectclass)
- [author](SubjectEntity.md#author)
- [timestamp](SubjectEntity.md#timestamp)

### Accessors

- [baseExpression](SubjectEntity.md#baseexpression)

### Methods

- [get](SubjectEntity.md#get)
- [getData](SubjectEntity.md#getdata)
- [save](SubjectEntity.md#save)
- [setCollectionAdder](SubjectEntity.md#setcollectionadder)
- [setCollectionSetter](SubjectEntity.md#setcollectionsetter)
- [setProperty](SubjectEntity.md#setproperty)
- [update](SubjectEntity.md#update)
- [all](SubjectEntity.md#all)

## Constructors

### constructor

• **new SubjectEntity**(`perspective`, `baseExpression?`)

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](PerspectiveProxy.md) |
| `baseExpression?` | `string` |

#### Defined in

[subject/SubjectEntity.ts:27](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L27)

## Properties

### #baseExpression

• `Private` **#baseExpression**: `string`

#### Defined in

[subject/SubjectEntity.ts:21](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L21)

___

### #perspective

• `Private` **#perspective**: [`PerspectiveProxy`](PerspectiveProxy.md)

#### Defined in

[subject/SubjectEntity.ts:23](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L23)

___

### #subjectClass

• `Private` **#subjectClass**: `string`

#### Defined in

[subject/SubjectEntity.ts:22](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L22)

___

### author

• **author**: `string`

#### Defined in

[subject/SubjectEntity.ts:24](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L24)

___

### timestamp

• **timestamp**: `string`

#### Defined in

[subject/SubjectEntity.ts:25](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L25)

## Accessors

### baseExpression

• `get` **baseExpression**(): `string`

#### Returns

`string`

#### Defined in

[subject/SubjectEntity.ts:32](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L32)

## Methods

### get

▸ **get**(): `Promise`<[`SubjectEntity`](SubjectEntity.md)\>

#### Returns

`Promise`<[`SubjectEntity`](SubjectEntity.md)\>

#### Defined in

[subject/SubjectEntity.ts:182](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L182)

___

### getData

▸ `Private` **getData**(`id?`): `Promise`<[`SubjectEntity`](SubjectEntity.md)\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `id?` | `string` |

#### Returns

`Promise`<[`SubjectEntity`](SubjectEntity.md)\>

#### Defined in

[subject/SubjectEntity.ts:36](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L36)

___

### save

▸ **save**(): `Promise`<`void`\>

#### Returns

`Promise`<`void`\>

#### Defined in

[subject/SubjectEntity.ts:146](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L146)

___

### setCollectionAdder

▸ `Private` **setCollectionAdder**(`key`, `value`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `key` | `string` |
| `value` | `any` |

#### Returns

`Promise`<`void`\>

#### Defined in

[subject/SubjectEntity.ts:132](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L132)

___

### setCollectionSetter

▸ `Private` **setCollectionSetter**(`key`, `value`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `key` | `string` |
| `value` | `any` |

#### Returns

`Promise`<`void`\>

#### Defined in

[subject/SubjectEntity.ts:117](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L117)

___

### setProperty

▸ `Private` **setProperty**(`key`, `value`): `Promise`<`void`\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `key` | `string` |
| `value` | `any` |

#### Returns

`Promise`<`void`\>

#### Defined in

[subject/SubjectEntity.ts:99](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L99)

___

### update

▸ **update**(): `Promise`<`void`\>

#### Returns

`Promise`<`void`\>

#### Defined in

[subject/SubjectEntity.ts:154](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L154)

___

### all

▸ `Static` **all**(`perspective`): `Promise`<`any`[]\>

#### Parameters

| Name | Type |
| :------ | :------ |
| `perspective` | [`PerspectiveProxy`](PerspectiveProxy.md) |

#### Returns

`Promise`<`any`[]\>

#### Defined in

[subject/SubjectEntity.ts:189](https://github.com/perspect3vism/ad4m/blob/0f993b76/core/src/subject/SubjectEntity.ts#L189)
