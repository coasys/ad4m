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

    encodeSingleQuote(input: string) {
        //@ts-ignore
        input = input.split("'").join("\\'")
        return input
    }

    decodeSingleQuote(input: string) {
        //@ts-ignore
        input = input.split("\\'").join("'")
        return input
    }

    toUrl(): string {
        if(this.#url && !this.#literal)
            return this.#url
        if(!this.#url && !this.#literal)
            throw new Error("Can't turn empty Literal into URL")

        let encoded
        switch(typeof this.#literal) {
            case 'string':
                encoded = `string:${this.#literal}`
                break;
            case 'number':
                encoded = `number:${this.#literal}`
                break;
            case 'object':
                encoded = `json:${JSON.stringify(this.#literal)}`
                break;
        }

        return this.encodeSingleQuote(encodeURI(`literal://${encoded}`))
    }

    get(): any {
        if(this.#literal)
            return this.#literal
            
        if(!this.#url)
            throw new Error("Can't render empty Literal")

        if(!this.#url.startsWith("literal://"))
            throw new Error("Can't render Literal from non-literal URL")
        
        // get rid of "literal://"
        const encoded = decodeURI(this.decodeSingleQuote(this.#url.substring(10)))

        if(encoded.startsWith("string:")) {
            return encoded.substring(7)
        }

        if(encoded.startsWith("number:")) {
            const numberString = encoded.substring(7)
            return parseFloat(numberString)
        }

        if(encoded.startsWith("json:")) {
            const json = encoded.substring(5)
            return JSON.parse(json)
        }

        throw new Error(`Can't parse unknown literal: ${encoded}`)
    }


}