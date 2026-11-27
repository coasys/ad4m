/**
 * `@coasys/ad4m/model` under `moduleResolution: "node"`: resolves only
 * because the package's `typesVersions` maps `model` to
 * `lib/src/model-entry.d.ts`. Without it this import is TS2307.
 */
import { Model, Optional, getPropertiesMetadata } from "@coasys/ad4m/model";

@Model({ name: "NodeResolutionProbe", namespace: "probe://" })
export class NodeResolutionProbe {
    @Optional({ through: "probe://title" })
    title!: string;
}

export const probeTitle = getPropertiesMetadata(NodeResolutionProbe).title;
