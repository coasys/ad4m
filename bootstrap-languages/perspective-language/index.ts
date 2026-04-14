/**
 * # Perspective Language
 *
 * Expression language for storing/retrieving Perspective objects.
 * Currently a stub — get() and create() are not implemented.
 */

import Icon from './build/Icon.js'
import ConstructorIcon from './build/ConstructorIcon.js'

// =============================================================================
// Required metadata
// =============================================================================

export const name = "perspective-language";
export const version = "0.1.0";

// =============================================================================
// Lifecycle
// =============================================================================

export async function init(): Promise<void> {
    // No state to initialize
}

export async function teardown(): Promise<void> {
    // No state to clean up
}

export function interactions(): any[] {
    return [];
}

// =============================================================================
// Expression UI
// =============================================================================

export function expressionIcon(): string {
    return Icon;
}

export function expressionConstructorIcon(): string {
    return ConstructorIcon;
}

// =============================================================================
// Expression capability (stub — not yet implemented)
// =============================================================================

export async function expressionCreate(_perspective: object): Promise<string> {
    console.log("PerspectiveLanguage: Sorry language has not been implemented yet!");
    // @ts-ignore
    return "" as string;
}

export async function expressionGet(_address: string): Promise<any> {
    console.log("PerspectiveLanguage: Sorry language has not been implemented yet!");
    return null;
}
