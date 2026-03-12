/**
 * Prolog query building helpers for the Ad4mModel query pipeline.
 *
 * These pure functions translate the Ad4mModel `Query` DSL into Prolog clause
 * fragments that are assembled by `Ad4mModel.queryToProlog()`.
 */

import type { Where, WhereCondition, Order, ParentScope } from "./types";
import { getRelationsMetadata } from "./decorators";
import { capitalize } from "./util";

// ── Parent resolution ────────────────────────────────────────────────────────

/**
 * Resolves the predicate for a parent query.
 *
 * Uses TS discriminated union narrowing:
 * - Raw form (`{ id, predicate }`) → predicate used as-is
 * - Model form (`{ model, id, field? }`) → lookup from relation metadata
 *   - With `field`: direct key lookup
 *   - Without `field`: scan for a relation whose `target()` matches `childCtor`
 */
export function resolveParentPredicate(
  parent: ParentScope,
  childCtor: Function,
): string {
  // Raw form — explicit predicate
  if ('predicate' in parent) return parent.predicate;

  // Model form — resolve from relation metadata
  const { model } = parent;
  const relMeta = getRelationsMetadata(model);

  // Direct lookup by field name when provided
  if (parent.field) {
    const entry = relMeta[parent.field];
    if (!entry) {
      throw new Error(
        `parent(): field "${parent.field}" is not a registered relation on ${model.name}`,
      );
    }
    return entry.predicate;
  }

  // Fallback: scan for a relation whose target matches the child class
  for (const [, entry] of Object.entries(relMeta)) {
    if (entry.target && entry.target() === childCtor) {
      return entry.predicate;
    }
  }
  throw new Error(
    `parent(): could not resolve predicate — no relation on ${model.name} targets ${(childCtor as any).name || 'the queried class'}`,
  );
}

// ── Query clause builders ────────────────────────────────────────────────────

export function buildParentQuery(
  parent: ParentScope | undefined,
  resolvedPredicate?: string,
): string {
  if (!parent || !resolvedPredicate) return '';
  return `triple("${parent.id}", "${resolvedPredicate}", Base)`;
}

// todo: only return Timestamp & Author from query (Base, AllLinks, and SortLinks not required)
export function buildAuthorAndTimestampQuery(): string {
  // Gets the author and timestamp of a Ad4mModel instance (based on the first link mentioning the base)
  return `
    findall(
      [T, A],
      link(Base, _, _, T, A),
      AllLinks
    ),
    sort(AllLinks, SortedLinks),
    SortedLinks = [[Timestamp, Author]|_]
  `;
}

export function buildPropertiesQuery(properties?: string[]): string {
  // Gets the name, value, and resolve boolean for all (or some) properties on a Ad4mModel instance
  // Resolves literals (if property_resolve/2 is true) to their value - either the data field if it is
  // an Expression in JSON literal, or the direct literal value if it is a simple literal
  // If no properties are provided, all are included
  return `
    findall([PropertyName, PropertyValue, Resolve], (
      % Constrain to specified properties if provided
      ${properties ? `member(PropertyName, [${properties.map((name) => `"${name}"`).join(", ")}]),` : ""}
      resolve_property(SubjectClass, Base, PropertyName, PropertyValue, Resolve)
    ), Properties)
  `;
}

export function buildWhereQuery(where: Where = {}): string {
  // Constrains the query to instances that match the provided where conditions
  // 'id' maps to the Prolog 'Base' variable (the base expression of the instance).

  function formatValue(value) {
    // Wrap strings in quotes
    return typeof value === "string" ? `"${value}"` : value;
  }

  return (Object.entries(where) as [string, WhereCondition][])
    .map(([key, value]) => {
      const isSpecial = ["id", "author", "timestamp"].includes(key);
      const getter = `resolve_property(SubjectClass, Base, "${key}", Value${key}, _)`;
      // For 'id' the Prolog variable is always 'Base'
      const field = key === "id" ? "Base" : capitalize(key);

      // Handle direct array values (for IN conditions)
      if (Array.isArray(value)) {
        const formattedValues = value.map((v) => formatValue(v)).join(", ");
        if (isSpecial) return `member(${field}, [${formattedValues}])`;
        else return `${getter}, member(Value${key}, [${formattedValues}])`;
      }

      // Handle operation object
      if (typeof value === "object" && value !== null) {
        const { not, between, lt, lte, gt, gte } = value;

        // Handle NOT operation
        if (not !== undefined) {
          if (Array.isArray(not)) {
            // NOT IN array
            const formattedValues = not.map((v) => formatValue(v)).join(", ");
            if (isSpecial) return `\\+ member(${field}, [${formattedValues}])`;
            else return `${getter}, \\+ member(Value${key}, [${formattedValues}])`;
          } else {
            // NOT EQUAL
            if (isSpecial) return `${field} \\= ${formatValue(not)}`;
            else return `${getter}, Value${key} \\= ${formatValue(not)}`;
          }
        }

        // Handle BETWEEN
        if (between !== undefined && Array.isArray(between) && between.length === 2) {
          if (isSpecial) return `${field} >= ${between[0]}, ${field} =< ${between[1]}`;
          else return `${getter}, Value${key} >= ${between[0]}, Value${key} =< ${between[1]}`;
        }

        // Handle lt, lte, gt, & gte operations
        const operators = [
          { value: lt, symbol: "<" }, // LESS THAN
          { value: lte, symbol: "=<" }, // LESS THAN OR EQUAL TO
          { value: gt, symbol: ">" }, // GREATER THAN
          { value: gte, symbol: ">=" }, // GREATER THAN OR EQUAL TO
        ];

        for (const { value, symbol } of operators) {
          if (value !== undefined)
            return isSpecial ? `${field} ${symbol} ${value}` : `${getter}, Value${key} ${symbol} ${value}`;
        }
      }

      // Default to direct equality
      if (isSpecial) return `${field} = ${formatValue(value)}`;
      else return `${getter}, Value${key} = ${formatValue(value)}`;
    })
    .join(", ");
}

export function buildCountQuery(count?: boolean): string {
  return count ? "length(UnsortedInstances, TotalCount)" : "";
}

export function buildOrderQuery(order?: Order): string {
  if (!order) return "SortedInstances = UnsortedInstances";
  const entries = Object.entries(order);
  if (entries.length === 1) {
    const [propertyName, direction] = entries[0];
    return `sort_instances(UnsortedInstances, "${propertyName}", "${direction}", SortedInstances)`;
  }
  // Multi-field sort: sort from least-significant to most-significant key
  // so that the final (primary) sort preserves secondary-key ordering for equal values.
  // The merge_sort implementation is stable (equal elements keep original order).
  const clauses: string[] = [];
  for (let i = entries.length - 1; i >= 0; i--) {
    const [propertyName, direction] = entries[i];
    const inputVar = i === entries.length - 1 ? "UnsortedInstances" : `MultiSortIntermediate${i + 1}`;
    const outputVar = i === 0 ? "SortedInstances" : `MultiSortIntermediate${i}`;
    clauses.push(`sort_instances(${inputVar}, "${propertyName}", "${direction}", ${outputVar})`);
  }
  return clauses.join(",\n      ");
}

export function buildOffsetQuery(offset?: number): string {
  if (!offset || offset < 0) return "InstancesWithOffset = SortedInstances";
  return `skipN(SortedInstances, ${offset}, InstancesWithOffset)`;
}

export function buildLimitQuery(limit?: number): string {
  if (!limit || limit < 0) return "AllInstances = InstancesWithOffset";
  return `takeN(InstancesWithOffset, ${limit}, AllInstances)`;
}
