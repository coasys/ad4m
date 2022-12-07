import { Link } from "./links/Links";
import { Literal } from "./Literal";
import { LinkQuery } from "./perspectives/LinkQuery";
import { PerspectiveProxy } from "./perspectives/PerspectiveProxy";
import { nanoid } from "nanoid";

export const SMART_LITERAL_CONTENT_PREDICATE = "smart_literal://content"

export class SmartLiteral {
    #perspective: PerspectiveProxy
    #base: string

    constructor(perspective: PerspectiveProxy, base: string) {
        this.#perspective = perspective
        this.#base = base
    }

    get base() {
        return this.#base
    }

    public static async create(perspective: PerspectiveProxy, literal: any): Promise<SmartLiteral> {
        const base = Literal.from(nanoid()).toUrl()
        //const base = Literal.from("nanoid()").toUrl()
        const smartLiteral = new SmartLiteral(perspective, base)
        await smartLiteral.set(literal)
        return smartLiteral
    }

    public static async isSmartLiteralBase(perspective: PerspectiveProxy, base: string): Promise<boolean> {
        let links = await perspective.get(new LinkQuery({
            source: base,
            predicate: SMART_LITERAL_CONTENT_PREDICATE
        }))
        return links.length > 0
    }

    public static async getAllSmartLiterals(perspective: PerspectiveProxy): Promise<SmartLiteral[]> {
        let links = await perspective.get(new LinkQuery({
            predicate: SMART_LITERAL_CONTENT_PREDICATE
        }))
        return links.map(link => new SmartLiteral(perspective, link.data.source))
    }

    async get(): Promise<any> {
        let link = await this.#perspective.getSingleTarget(new LinkQuery({
            source: this.#base,
            predicate: SMART_LITERAL_CONTENT_PREDICATE
        }))

        if(!link) {
            throw `No content for smart literal ${this.#base}`
        }

        return Literal.fromUrl(link).get()
    }
    
    async set(content: any) {
        let literal = Literal.from(content)
        await this.#perspective.setSingleTarget(new Link({
            source: this.#base,
            predicate: SMART_LITERAL_CONTENT_PREDICATE,
            target: literal.toUrl()
        }))
    }
}