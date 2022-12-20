[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / subject/SDNADecorators

# Module: subject/SDNADecorators

## Table of contents

### Classes

- [PerspectiveAction](../classes/subject_SDNADecorators.PerspectiveAction.md)

### Functions

- [addLink](subject_SDNADecorators.md#addlink)
- [hasLink](subject_SDNADecorators.md#haslink)
- [sdnaOutput](subject_SDNADecorators.md#sdnaoutput)
- [subjectCollection](subject_SDNADecorators.md#subjectcollection)
- [subjectProperty](subject_SDNADecorators.md#subjectproperty)

## Functions

### addLink

▸ **addLink**(`source`, `predicate`, `target`): [`PerspectiveAction`](../classes/subject_SDNADecorators.PerspectiveAction.md)

#### Parameters

| Name | Type |
| :------ | :------ |
| `source` | `string` |
| `predicate` | `string` |
| `target` | `string` |

#### Returns

[`PerspectiveAction`](../classes/subject_SDNADecorators.PerspectiveAction.md)

#### Defined in

[subject/SDNADecorators.ts:10](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/subject/SDNADecorators.ts#L10)

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

[subject/SDNADecorators.ts:19](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/subject/SDNADecorators.ts#L19)

___

### sdnaOutput

▸ **sdnaOutput**(`target`, `key`, `descriptor`): `PropertyDescriptor`

#### Parameters

| Name | Type |
| :------ | :------ |
| `target` | `any` |
| `key` | `string` |
| `descriptor` | `PropertyDescriptor` |

#### Returns

`PropertyDescriptor`

#### Defined in

[subject/SDNADecorators.ts:46](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/subject/SDNADecorators.ts#L46)

___

### subjectCollection

▸ **subjectCollection**(`opts`): <T\>(`target`: `T`, `key`: keyof `T`) => `void`

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

[subject/SDNADecorators.ts:37](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/subject/SDNADecorators.ts#L37)

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

[subject/SDNADecorators.ts:28](https://github.com/perspect3vism/ad4m/blob/e76a46f1/core/src/subject/SDNADecorators.ts#L28)
