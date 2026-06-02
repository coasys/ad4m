import { ExpressionGeneric } from "../expression/Expression";
import { LanguageRef } from "../language/LanguageRef";
import { Perspective } from "../perspectives/Perspective";
export class Neighbourhood {
    linkLanguage: string
    meta: Perspective

    constructor(linkLanguage: string, meta: Perspective) {
        this.linkLanguage = linkLanguage;
        this.meta = meta;
    }
}
export class NeighbourhoodExpression extends ExpressionGeneric(Neighbourhood) {};
