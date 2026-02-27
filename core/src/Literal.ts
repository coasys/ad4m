function encodeRFC3986URIComponent(str) {
  return encodeURIComponent(str).replace(
    /[!'()*]/g,
    (c) => `%${c.charCodeAt(0).toString(16).toUpperCase()}`,
  );
}

export class Literal {
  private _literal?: any;
  private _url?: string;

  public static fromUrl(url: string) {
    if (!url || !url.startsWith("literal://"))
      throw new Error("Can't create Literal from non-literal URL");
    const l = new Literal();
    l._url = url;
    return l;
  }

  public static from(literal: any) {
    const l = new Literal();
    l._literal = literal;
    return l;
  }

  toUrl(): string {
    if (this._url && !this._literal) return this._url;
    if (
      !this._url &&
      (this._literal === undefined ||
        this._literal === "" ||
        this._literal === null)
    )
      throw new Error("Can't turn empty Literal into URL");

    let encoded;
    switch (typeof this._literal) {
      case "string":
        encoded = `string:${encodeRFC3986URIComponent(this._literal)}`;
        break;
      case "number":
        encoded = `number:${encodeRFC3986URIComponent(this._literal)}`;
        break;
      case "boolean":
        encoded = `boolean:${encodeRFC3986URIComponent(this._literal)}`;
        break;
      case "object":
        encoded = `json:${encodeRFC3986URIComponent(JSON.stringify(this._literal))}`;
        break;
      default:
        throw new Error(
          `Literal.toUrl(): unsupported type "${typeof this._literal}" (value: ${String(this._literal)})`,
        );
    }

    return `literal://${encoded}`;
  }

  get(): any {
    if (
      this._literal !== undefined &&
      this._literal !== null &&
      this._literal !== ""
    )
      return this._literal;

    if (!this._url) throw new Error("Can't render empty Literal");

    if (!this._url.startsWith("literal://"))
      throw new Error("Can't render Literal from non-literal URL");

    // get rid of "literal://"
    const body = this._url.substring(10);

    if (body.startsWith("string:")) {
      return decodeURIComponent(body.substring(7));
    }

    if (body.startsWith("number:")) {
      const numberString = body.substring(7);
      return parseFloat(numberString);
    }

    if (body.startsWith("boolean:")) {
      const boolStr = body.substring(8).trim();
      if (boolStr === "true") return true;
      if (boolStr === "false") return false;
      throw new Error(
        `Literal.get(): malformed boolean payload "${boolStr}" — expected "true" or "false"`,
      );
    }

    if (body.startsWith("json:")) {
      const json = body.substring(5);
      return JSON.parse(decodeURIComponent(json));
    }

    throw new Error(`Can't parse unknown literal: ${body}`);
  }
}
