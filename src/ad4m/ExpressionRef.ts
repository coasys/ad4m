import type Address from './Address';
import LanguageRef from './LanguageRef'

// Expression address + unique Language ID = global expression URL
export default class ExpressionRef {
    language: LanguageRef;
    expression: Address;

    constructor(lang: LanguageRef, expr: Address) {
        this.language = lang
        this.expression = expr
    }
}

// Creates unique expression URI like this:
// expr:Qm3490gfwe489hf34:Qm90ghjoaw4iehioefwe4ort
export function exprRef2String(ref: ExpressionRef): string {
    if(ref.language.address === 'did')
        return ref.expression.toString()
    else
        return `${ref.language.address}://${ref.expression}`
}

export function parseExprURL(url: string): ExpressionRef {
    const URIregexp = /^([^:^\s]+):\/\/([^\s]+)$/
    const URImatches = URIregexp.exec(url)

    if(URImatches && URImatches.length == 3) {
        const language = URImatches[1]
        const expression = URImatches[2]
    
        const languageRef = new LanguageRef()
        languageRef.address = language
    
        const ref = new ExpressionRef(languageRef, expression)
        return ref
    }

    const DIDregexp = /^did:([^\s]+)$/
    const DIDmatches = DIDregexp.exec(url)

    if(DIDmatches && DIDmatches.length == 2) {
        const languageRef = new LanguageRef()
        languageRef.address = 'did'
        const ref = new ExpressionRef(languageRef, url)
        return ref
    }

    throw new Error("Couldn't parse string as expression URL or DID: " + url)
}