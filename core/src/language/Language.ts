import { DID } from "../DID";
import { PerspectiveExpression } from "../perspectives/Perspective";

/**
 * # AD4M Language — client-side types
 *
 * This file only contains the client-visible types that the AD4M REST
 * API exposes on top of a running Language — interaction metadata, online
 * agent records, and the telepresence-signal callback shape.
 *
 * **If you're authoring an AD4M Language**, you don't consume types from
 * here — use [`@coasys/ad4m-ldk`](https://www.npmjs.com/package/@coasys/ad4m-ldk)
 * (or the [Rust ALDK](https://crates.io/crates/ad4m-ldk)), which provide
 * the capability traits, typed runtime-import wrappers, and authoring
 * macros/helpers.
 *
 * The normative interface definition lives in
 * [`docs-src/ad4m-lang.wit`](https://github.com/coasys/ad4m/blob/dev/docs-src/ad4m-lang.wit).
 * The prose spec is
 * [`docs-src/language-interface-spec.md`](https://github.com/coasys/ad4m/blob/dev/docs-src/language-interface-spec.md).
 */

// ----- Interaction metadata --------------------------------------------------
//
// Languages optionally publish a list of user-invocable actions per
// Expression ("interactions"). The runtime surfaces these to UIs via
// the API as first-class objects.

export class InteractionParameter {
    name: string;
    type: string;
}

export class InteractionMeta {
    label: string;
    name: string;
    parameters: InteractionParameter[];
}

/**
 * Runtime-side interaction object. The runtime constructs one of these
 * from a Language's `interactions(address)` description and exposes
 * `execute()` to UI callers. Language authors describe interactions
 * via the ALDK, not by constructing this type directly.
 */
export interface Interaction {
    readonly label: string;
    readonly name: string;
    readonly parameters: InteractionParameter[];
    execute(parameters: object): Promise<string | null>;
}

export class InteractionCall {
    name: string;
    parametersStringified: string;

    public get parameters(): object {
        return JSON.parse(this.parametersStringified);
    }

    constructor(name: string, parameters: object) {
        this.name = name;
        this.parametersStringified = JSON.stringify(parameters);
    }
}

// ----- Telepresence ----------------------------------------------------------

export class OnlineAgent {
    did: DID;
    status: PerspectiveExpression;
}

/**
 * Callback shape used by the client to receive telepresence signals
 * from a Neighbourhood's link Language.
 */
export type TelepresenceSignalCallback = (
    payload: PerspectiveExpression,
    recipientDid?: string,
) => void;
