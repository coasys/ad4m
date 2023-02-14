[@perspect3vism/ad4m](../README.md) / [Exports](../modules.md) / subject/SDNADecorators

# Module: subject/SDNADecorators

## Table of contents

### Classes

- [PerspectiveAction](../classes/subject_SDNADecorators.PerspectiveAction.md)

### Functions

- [SDNAClass](subject_SDNADecorators.md#sdnaclass)
- [addLink](subject_SDNADecorators.md#addlink)
- [hasLink](subject_SDNADecorators.md#haslink)
- [instanceQuery](subject_SDNADecorators.md#instancequery)
- [subjectCollection](subject_SDNADecorators.md#subjectcollection)
- [subjectFlag](subject_SDNADecorators.md#subjectflag)
- [subjectProperty](subject_SDNADecorators.md#subjectproperty)

## Functions

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

[subject/SDNADecorators.ts:153](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/subject/SDNADecorators.ts#L153)

___

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

[subject/SDNADecorators.ts:12](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/subject/SDNADecorators.ts#L12)

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

[subject/SDNADecorators.ts:21](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/subject/SDNADecorators.ts#L21)

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

[subject/SDNADecorators.ts:30](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/subject/SDNADecorators.ts#L30)

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

[subject/SDNADecorators.ts:126](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/subject/SDNADecorators.ts#L126)

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

[subject/SDNADecorators.ts:99](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/subject/SDNADecorators.ts#L99)

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

[subject/SDNADecorators.ts:80](https://github.com/perspect3vism/ad4m/blob/d9ddd7e2/core/src/subject/SDNADecorators.ts#L80)
