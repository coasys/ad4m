/**
 * WebSocket close code 1006 (abnormal closure) is emitted on every active
 * GraphQL subscription when the server process shuts down — it simply means
 * the server didn't send a clean close frame.  This is expected at the end
 * of every test run and whenever the executor is stopped, so it must not be
 * treated as an application error.  All other errors are still surfaced.
 */
export function isSocketCloseError(e: any): boolean {
  if (!e) return false;
  if (typeof e.code === "number" && e.code === 1006) return true;
  const msg = String(e?.message ?? e);
  return msg.startsWith("Socket closed with event 1006");
}

export function formatList(list) {
  if (!list?.length) {
    return "";
  }
  if (list.length === 1) {
    return list.toString();
  }
  if (list.length === 2) {
    return list.join(" and ");
  }

  return list.slice(0, -1).join(", ") + ", and " + list.slice(-1);
}

export function capSentence(cap) {
  const can = cap.can.includes("*") ? ["READ", "WRITE", "UPDATE"] : cap.can;
  const domain = cap.with.domain === "*" ? "" : cap.with.domain;
  const pointers = cap.with.pointers.includes("*")
    ? ["all AD4M data"]
    : cap.with.pointers;

  return `${formatList(
    can,
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
    .replace(/\\/g, "\\\\") // Backslash -> \\
    .replace(/'/g, "\\'") // Single quote -> \'
    .replace(/"/g, '\\"') // Double quote -> \"
    .replace(/\n/g, "\\n") // Newline -> \n
    .replace(/\r/g, "\\r") // Carriage return -> \r
    .replace(/\t/g, "\\t"); // Tab -> \t
}
