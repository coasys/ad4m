import { LinkExpression, linkExpressionToWire } from "../links/Links";
import { Perspective, PerspectiveExpression } from "../perspectives/Perspective";
import type { DecoratedPerspective } from "../generated/api/DecoratedPerspective";
import type { Perspective as WirePerspective } from "../generated/api/Perspective";
import type { PerspectiveExpression as WirePerspectiveExpression } from "../generated/api/PerspectiveExpression";

/** Build a `Perspective` (with its query helpers) from the executor's wire shape. */
export function perspectiveFromWire(wire: DecoratedPerspective | null): Perspective {
    return new Perspective((wire?.links ?? []).map(LinkExpression.fromWire));
}

export function perspectiveToWire(perspective: Perspective): WirePerspective {
    return { links: perspective.links.map(linkExpressionToWire) };
}

export function perspectiveExpressionFromWire(wire: WirePerspectiveExpression): PerspectiveExpression {
    return new PerspectiveExpression(wire.author, wire.timestamp, perspectiveFromWire(wire.data), wire.proof);
}
