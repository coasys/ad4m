import { Link } from "./links/Links";
import { Literal } from "./Literal";
import { LinkQuery } from "./perspectives/LinkQuery";
import { PerspectiveProxy } from "./perspectives/PerspectiveProxy";

export const SMART_LITERAL_CONTENT_PREDICATE = "smart_literal://content";

function makeRandomStringID(length: number): string {
  let result = "";
  let characters =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  let charactersLength = characters.length;
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * charactersLength));
  }
  return result;
}

export class SmartLiteral {
  private _perspective: PerspectiveProxy;
  private _base: string;

  constructor(perspective: PerspectiveProxy, base: string) {
    this._perspective = perspective;
    this._base = base;
  }

  get base() {
    return this._base;
  }

  public static async create(
    perspective: PerspectiveProxy,
    literal: any,
  ): Promise<SmartLiteral> {
    const base = Literal.from(makeRandomStringID(10)).toUrl();
    const smartLiteral = new SmartLiteral(perspective, base);
    await smartLiteral.set(literal);
    return smartLiteral;
  }

  public static async isSmartLiteralBase(
    perspective: PerspectiveProxy,
    base: string,
  ): Promise<boolean> {
    let links = await perspective.get(
      new LinkQuery({
        source: base,
        predicate: SMART_LITERAL_CONTENT_PREDICATE,
      }),
    );
    return links.length > 0;
  }

  public static async getAllSmartLiterals(
    perspective: PerspectiveProxy,
  ): Promise<SmartLiteral[]> {
    let links = await perspective.get(
      new LinkQuery({
        predicate: SMART_LITERAL_CONTENT_PREDICATE,
      }),
    );
    return links.map((link) => new SmartLiteral(perspective, link.data.source));
  }

  async get(): Promise<any> {
    let link = await this._perspective.getSingleTarget(
      new LinkQuery({
        source: this._base,
        predicate: SMART_LITERAL_CONTENT_PREDICATE,
      }),
    );

    if (!link) {
      throw `No content for smart literal ${this._base}`;
    }

    return Literal.fromUrl(link).get();
  }

  async set(content: any) {
    let literal = Literal.from(content);
    await this._perspective.setSingleTarget(
      new Link({
        source: this._base,
        predicate: SMART_LITERAL_CONTENT_PREDICATE,
        target: literal.toUrl(),
      }),
    );
  }
}
