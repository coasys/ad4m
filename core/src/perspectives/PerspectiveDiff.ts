import { ExpressionGeneric } from "../expression/Expression";
import { LinkExpression } from "../links/Links";
export class PerspectiveDiff {
    additions: LinkExpression[]
    removals: LinkExpression[]
}
export class PerspectiveDiffExpression extends ExpressionGeneric(PerspectiveDiff) {};
