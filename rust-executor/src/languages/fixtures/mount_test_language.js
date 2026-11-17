import { languageAddress, emitSignal } from "ad4m:host";

export const name = "mount-test";
const CLASS = "MountThing";

// address -> data object { title, done, tags, children? }
const records = new Map();
// every executed interaction, in order: { address, name, params }
const calls = [];
let counter = 0;
// version reported by mountShapes(); setVersion changes it so a remount
// can be told apart from the first install
let mountVersion = "2";
// the classes mountShapes() reports: [{ name, target }]; setShapes changes
// them so tests can collide with another mount's names and class IRIs
let shapeClasses = [{ name: CLASS, target: "mounttest://Thing" }];
// mountSnapshot() throws this message while it is set (setSnapshotFault)
let snapshotFailure = null;
// entries mountSnapshot() lists after the records (setSnapshotFault)
let extraEntries = [];

export async function init() {}
export function isPublic() { return false; }

function delta(address) {
  return { address, changed: [{ class: CLASS, address, data: records.get(address) }], removed: [] };
}

function plain(params) {
  return params && typeof params === "object" ? params : {};
}

export function interactions(address) {
  if (address === "class/" + CLASS) {
    return [{
      label: "Create", name: "create",
      parameters: [{ name: "title", type: "string" }, { name: "done", type: "boolean" }, { name: "tags", type: "array" }, { name: "children", type: "array" }],
      execute: async (params) => {
        const p = plain(params);
        calls.push({ address, name: "create", params: p });
        if (p.fail) throw new Error("create: " + p.fail);
        counter += 1;
        const id = "t" + counter;
        const record = { title: p.title ?? null, done: p.done ?? false, tags: p.tags ?? [] };
        if (p.children !== undefined) record.children = p.children;
        records.set(id, record);
        // A result that is not a MountDelta: its changed entry has no class.
        if (p.malformed) return { address: id, changed: [{ address: id, data: records.get(id) }] };
        return delta(id);
      },
    }];
  }
  if (address === "project") {
    return [
      { label: "Calls", name: "calls", parameters: [],
        execute: async () => calls.slice() },
      { label: "Reset", name: "reset", parameters: [],
        execute: async () => { records.clear(); calls.length = 0; counter = 0; return true; } },
      { label: "Address", name: "address", parameters: [],
        execute: async () => languageAddress() },
      // Change the version mountShapes() reports (tests re-install on version change).
      { label: "Set version", name: "setVersion", parameters: [],
        execute: async (params) => { mountVersion = plain(params).version; return true; } },
      // Change the classes mountShapes() reports: { shapes: [{ name, target }], version }.
      { label: "Set shapes", name: "setShapes", parameters: [],
        execute: async (params) => {
          const p = plain(params);
          shapeClasses = p.shapes;
          if (p.version !== undefined) mountVersion = p.version;
          return true;
        } },
      // Make mountSnapshot() throw ({ fail: "msg" }) or list extra entries
      // after the records ({ extra: [entry] }); {} clears both.
      { label: "Set snapshot fault", name: "setSnapshotFault", parameters: [],
        execute: async (params) => {
          const p = plain(params);
          snapshotFailure = p.fail ?? null;
          extraEntries = p.extra ?? [];
          return true;
        } },
      // Change a record "from outside" (like a stock Acorn user would). Emits a delta only if emit is true.
      { label: "Outside change", name: "outsideChange", parameters: [],
        execute: async (params) => {
          const p = plain(params);
          records.set(p.address, { ...(records.get(p.address) ?? {}), ...(p.data ?? {}) });
          if (p.emit) emitSignal({ mountDelta: { changed: [{ class: CLASS, address: p.address, data: records.get(p.address) }], removed: [] } });
          return true;
        } },
      { label: "Outside remove", name: "outsideRemove", parameters: [],
        execute: async (params) => {
          const p = plain(params);
          records.delete(p.address);
          if (p.emit) emitSignal({ mountDelta: { changed: [], removed: [p.address] } });
          return true;
        } },
      // Emit the params object as the signal payload unchanged (tests push
      // malformed mountDelta shapes through this).
      { label: "Emit raw", name: "emitRaw", parameters: [],
        execute: async (params) => { emitSignal(params); return true; } },
    ];
  }
  if (records.has(address)) {
    return [
      { label: "Update", name: "update", parameters: [],
        execute: async (params) => {
          const p = plain(params);
          calls.push({ address, name: "update", params: p });
          if (p.fail) throw new Error("update: " + p.fail);
          records.set(address, { ...records.get(address), ...p });
          return delta(address);
        } },
      { label: "Delete", name: "delete", parameters: [],
        execute: async (params) => {
          calls.push({ address, name: "delete", params: plain(params) });
          records.delete(address);
          return { address, changed: [], removed: [address] };
        } },
    ];
  }
  return [];
}

export function mountShapes() {
  const lang = languageAddress();
  const update = [{ action: "call", source: "this", predicate: "update", target: "{}" }];
  return {
    version: mountVersion,
    shapes: shapeClasses.map(({ name, target }) => ({
      name,
      shape: {
        target_class: target,
        constructor_actions: [{ action: "call", source: lang + "://class/" + name, predicate: "create", target: "{}" }],
        destructor_actions: [{ action: "call", source: "this", predicate: "delete", target: "{}" }],
        properties: [
          { path: "rdf://type", name: "type", has_value: target, min_count: 1, max_count: 1 },
          { path: "mounttest://title", name: "title", max_count: 1, writable: true, alias: { TextContent: "text" }, setter: update },
          { path: "mounttest://done", name: "done", max_count: 1, writable: true, setter: update },
          { path: "mounttest://tag", name: "tags", collection: true, datatype: "xsd://string", writable: true, setter: update, ordering: "linkedList" },
          { path: "mounttest://child", name: "children", collection: true, node_kind: "IRI", relation_kind: "hasMany", alias: { Container: "children" } },
          { path: "ad4m://collection_order", name: "_collectionOrder" },
        ],
      },
    })),
  };
}

export function mountSnapshot() {
  if (snapshotFailure) throw new Error("mountSnapshot: " + snapshotFailure);
  const entries = [...records.entries()].map(([address, data]) => ({ class: CLASS, address, data }));
  return { entries: entries.concat(extraEntries) };
}

export async function expressionGet(address) {
  if (!records.has(address)) return null;
  return { author: "did:key:mount-test", timestamp: new Date(0).toISOString(), data: records.get(address), proof: { signature: "", key: "" } };
}
