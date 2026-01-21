export function formatList(list) {
  if (!list?.length) {
      return "";
  }
  if (list.length === 1) {
      return list.toString();
  }
  if (list.length === 2) {
      return list.join(' and ');
  }

  return list.slice(0, -1).join(', ') + ', and ' + list.slice(-1);
};

export function capSentence(cap) {
  const can = cap.can.includes("*") ? ["READ", "WRITE", "UPDATE"] : cap.can;
  const domain = cap.with.domain === "*" ? "" : cap.with.domain;
  const pointers = cap.with.pointers.includes("*")
    ? ["all AD4M data"]
    : cap.with.pointers;

  return `${formatList(
    can
  )} your ${domain} actions, with access to ${formatList(pointers)}`;
}

/**
 * Escapes a string value for safe use in SurrealQL queries.
 * 
 * @description
 * Prevents SQL injection by properly escaping special characters in string values
 * that will be interpolated into SurrealQL queries. This handles the most common
 * special characters that could break SQL queries or enable injection attacks.
 * 
 * Single quotes, backslashes, and other special characters are escaped using
 * backslash notation, which is the standard escaping mechanism for SurrealQL.
 * 
 * @param value - The string value to escape
 * @returns The escaped string safe for SurrealQL interpolation (without surrounding quotes)
 * 
 * @example
 * ```typescript
 * const userInput = "user's input with 'quotes'";
 * const escaped = escapeSurrealString(userInput);
 * const query = `SELECT * FROM link WHERE uri = '${escaped}'`;
 * // Results in: SELECT * FROM link WHERE uri = 'user\'s input with \'quotes\''
 * ```
 */
export function escapeSurrealString(value: string): string {
    return value
        .replace(/\\/g, '\\\\')   // Backslash -> \\
        .replace(/'/g, "\\'")      // Single quote -> \'
        .replace(/"/g, '\\"')      // Double quote -> \"
        .replace(/\n/g, '\\n')     // Newline -> \n
        .replace(/\r/g, '\\r')     // Carriage return -> \r
        .replace(/\t/g, '\\t');    // Tab -> \t
}
