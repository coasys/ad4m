function encodeRFC3986URIComponent(str) {
    return encodeURIComponent(str)
        .replace(
            /[!'()*]/g,
            (c) => `%${c.charCodeAt(0).toString(16).toUpperCase()}`
        );
}

export class Literal {
    #literal?: any
    #url?: string

    public static fromUrl(url: string) {
        if(!url || !url.startsWith("literal://"))
            throw new Error("Can't create Literal from non-literal URL")
        const l = new Literal()
        l.#url = url
        return l
    }

    public static from(literal: any) {
        const l = new Literal()
        l.#literal = literal
        return l
    }

    toUrl(): string {
        if(this.#url && !this.#literal)
            return this.#url
        if(!this.#url && (this.#literal === undefined || this.#literal === "" || this.#literal === null))
            throw new Error("Can't turn empty Literal into URL")

        let encoded
        switch(typeof this.#literal) {
            case 'string':
                encoded = `string:${encodeRFC3986URIComponent(this.#literal)}`
                break;
            case 'number':
                encoded = `number:${encodeRFC3986URIComponent(this.#literal)}`
                break;
            case 'boolean':
                encoded = `boolean:${encodeRFC3986URIComponent(this.#literal)}`
                break;
            case 'object':
                encoded = `json:${encodeRFC3986URIComponent(JSON.stringify(this.#literal))}`
                break;
        }

        return `literal://${encoded}`
    }

    get(): any {
        if(this.#literal)
            return this.#literal
            
        if(!this.#url)
            throw new Error("Can't render empty Literal")

        if(!this.#url.startsWith("literal://"))
            throw new Error("Can't render Literal from non-literal URL")
        
        // get rid of "literal://"
        const body = this.#url.substring(10)
        

        if(body.startsWith("string:")) {
            return decodeURIComponent(body.substring(7))
        }

        if(body.startsWith("number:")) {
            const numberString = body.substring(7)
            return parseFloat(numberString)
        }

        if(body.startsWith("json:")) {
            const json = body.substring(5)
            return JSON.parse(decodeURIComponent(json))
        }

        throw new Error(`Can't parse unknown literal: ${body}`)
    }


}