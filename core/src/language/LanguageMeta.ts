import { ExpressionGeneric } from "../expression/Expression";
export class LanguageMeta {
    name: string;
    address: string;
    description: string;
    author: string;
    templated: boolean
    templateSourceLanguageAddress?: string;
    templateAppliedParams?: string;
    possibleTemplateParams?: string[];
    sourceCodeLink?: string;
}
export class LanguageMetaInput {
    name: string;
    description: string;
    possibleTemplateParams?: string[];
    sourceCodeLink?: string;

    constructor(name?: string, description?: string) {
        this.name = name
        this.description = description
        if(!this.description) this.description = ""
    }
}

export class LanguageMetaInternal {
    name: string;
    address: string;
    description: string;
    templateSourceLanguageAddress?: string;
    templateAppliedParams?: string;
    possibleTemplateParams?: string[];
    sourceCodeLink?: string;
}

export class LanguageExpression extends ExpressionGeneric(LanguageMetaInternal) {};

export class LanguageLanguageInput {
    bundle: string;
    meta: LanguageMetaInternal;
}