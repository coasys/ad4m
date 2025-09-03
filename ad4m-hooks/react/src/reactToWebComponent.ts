// Minimal runtime contract for React/ReactDOM (so this file is framework-agnostic)
type ReactLike = {
  createElement: (...args: any[]) => any;
};
type ReactDOMLike = {
  createRoot: (container: Element | DocumentFragment) => {
    render: (el: any) => void;
    unmount: () => void;
  };
};

type Options = {
  /** Tag name to register (you can also call `customElements.define` yourself). */
  tagName?: string;
  /** Use Shadow DOM? If false, render into the element itself. */
  shadow?: boolean | ShadowRootInit;
  /** Attribute prefix to treat as props (e.g. data-foo -> foo). Empty = all attributes. */
  attrPrefix?: string; // default: '' (accept all)
  /** Element property names to expose as pass-through React props (object/class friendly). */
  observedProps?: string[];
  /** Initial props (merged shallowly before anything else). */
  initialProps?: Record<string, any>;
  /** Optional styles to inject (adoptedStyleSheets or <style> text). */
  styles?: CSSStyleSheet[] | string;
};

/**
 * Turn a React component into a Web Component (Custom Element).
 * - Arbitrary props (objects/classes/functions) supported via `el.props = {...}` or `el.setProps({...})`.
 * - Optionally define specific element properties that map directly to React props via `observedProps`.
 * - Attribute values are parsed into primitives/JSON when possible.
 * - Light-DOM children are supported via <slot/> passed as `children`.
 */
export function reactToWebComponent<P extends object>(
  ReactComponent: (props: P) => any | any,
  React: ReactLike,
  ReactDOM: ReactDOMLike,
  {
    tagName,
    shadow = { mode: "open" },
    attrPrefix = "",
    observedProps = [],
    initialProps = {},
    styles,
  }: Options = {}
) {
  const RESERVED_KEYS = new Set([
    "_root",
    "_mount",
    "_props",
    "_renderQueued",
    "_observer",
    "_cleanup",
    "props",
    "setProps",
    "forceUpdate",
  ]);

  // Util: convert kebab-case to camelCase
  const toCamel = (s: string) =>
    s.replace(/-([a-z])/g, (_m, c) => c.toUpperCase());

  // Util: parse attribute text into a value (boolean/number/json)
  const parseAttr = (raw: string) => {
    const v = raw.trim();
    if (v === "") return ""; // empty string stays empty
    if (v === "true") return true;
    if (v === "false") return false;
    if (v === "null") return null;
    if (v === "undefined") return undefined;
    // number?
    if (/^[+-]?\d+(\.\d+)?$/.test(v)) return Number(v);
    // try JSON (objects/arrays)
    if (
      (v.startsWith("{") && v.endsWith("}")) ||
      (v.startsWith("[") && v.endsWith("]"))
    ) {
      try {
        return JSON.parse(v);
      } catch {
        /* fallthrough */
      }
    }
    return raw; // as-is string
  };

  // Util: schedule a single render per microtask
  function enqueueRender(el: any) {
    if (el._renderQueued) return;
    el._renderQueued = true;
    queueMicrotask(() => {
      el._renderQueued = false;
      el._render();
    });
  }

  // Define the custom element class
  class ReactCustomElement extends HTMLElement {
    private _root!: ReturnType<ReactDOMLike["createRoot"]>;
    private _mount!: HTMLElement;
    private _props: Record<string, any> = {};
    private _observer?: MutationObserver;
    private _renderQueued = false;

    constructor() {
      super();

      // Where React renders
      let host: ShadowRoot | HTMLElement;
      if (shadow) {
        const init: ShadowRootInit =
          typeof shadow === "object" ? shadow : { mode: "open" };
        host = this.attachShadow(init);
        // Inject styles
        if (styles) {
          if (
            Array.isArray(styles) &&
            (host as any).adoptedStyleSheets !== undefined
          ) {
            (host as any).adoptedStyleSheets = [
              ...(host as any).adoptedStyleSheets,
              ...styles,
            ];
          } else {
            const styleEl = document.createElement("style");
            styleEl.textContent = Array.isArray(styles) ? "" : String(styles);
            host.appendChild(styleEl);
          }
        }
      } else {
        host = this;
      }

      this._mount = document.createElement("div");
      host.appendChild(this._mount);
      this._root = ReactDOM.createRoot(this._mount);

      // Initialize props
      this._props = { ...initialProps };

      // Define a unified props setter/getter for arbitrary values
      Object.defineProperty(this, "props", {
        get: () => ({ ...this._props }),
        set: (next: Record<string, any>) => {
          if (next && typeof next === "object") {
            Object.assign(this._props, next);
            enqueueRender(this);
          }
        },
      });

      // A convenience method for partial updates
      (this as any).setProps = (patch: Record<string, any>) => {
        if (patch && typeof patch === "object") {
          Object.assign(this._props, patch);
          enqueueRender(this);
        }
      };

      // Expose a forceUpdate if someone needs it
      (this as any).forceUpdate = () => enqueueRender(this);

      // Define pass-through element properties (object/class safe)
      for (const key of observedProps) {
        if (RESERVED_KEYS.has(key)) continue;
        if (Object.prototype.hasOwnProperty.call(this, key)) continue; // don't clobber instance fields
        Object.defineProperty(this, key, {
          get: () => this._props[key],
          set: (val: any) => {
            this._props[key] = val;
            enqueueRender(this);
          },
          configurable: true,
          enumerable: true,
        });
      }

      // Attribute observer (prefix-aware; empty prefix = all)
      this._observer = new MutationObserver((records) => {
        for (const r of records) {
          if (r.type !== "attributes" || !r.attributeName) continue;
          const attr = r.attributeName;
          if (attrPrefix && !attr.startsWith(attrPrefix)) continue;

          const logical = toCamel(
            attrPrefix ? attr.slice(attrPrefix.length) : attr
          );
          const raw = this.getAttribute(attr);
          const val = raw === null ? undefined : parseAttr(raw);
          // undefined deletes the prop (useful when attribute removed)
          if (val === undefined) delete this._props[logical];
          else this._props[logical] = val;
        }
        enqueueRender(this);
      });
      this._observer.observe(this, { attributes: true });

      // Bootstrap from existing attributes once
      for (const attr of Array.from(this.attributes)) {
        const name = attr.name;
        if (attrPrefix && !name.startsWith(attrPrefix)) continue;
        const logical = toCamel(
          attrPrefix ? name.slice(attrPrefix.length) : name
        );
        this._props[logical] = parseAttr(attr.value);
      }
    }

    connectedCallback() {
      // Kick initial render
      enqueueRender(this);
    }

    disconnectedCallback() {
      this._observer?.disconnect();
      this._root.unmount();
    }

    // Render React with <slot/> as children (works in shadow and light DOM)
    private _render() {
      const element = React.createElement(ReactComponent as any, {
        ...this._props,
        host: this,
        children: React.createElement("slot"),
      });
      this._root.render(element);
    }
  }

  // Optionally register
  if (tagName) {
    if (!customElements.get(tagName)) {
      customElements.define(tagName, ReactCustomElement);
    }
  }

  return ReactCustomElement;
}
