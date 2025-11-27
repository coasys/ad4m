import type { Address, Interaction, Expression, Language, LanguageContext } from "https://esm.sh/v135/@coasys/ad4m@0.5.0";
import { exists } from "https://deno.land/std@0.184.0/fs/mod.ts";
import { join } from "https://deno.land/std@0.184.0/path/mod.ts";
import pako from "https://esm.sh/v135/pako@2.0.4";
import base64js from "https://esm.sh/v135/base64-js@1.5.1";

function compressUri(uri: string): string {
    const compressed = pako.deflate(uri);
    return base64js.fromByteArray(compressed);
}

function decompressUri(compressedString: string): string {
    const compressed = base64js.toByteArray(compressedString);
    const decompressed = pako.inflate(compressed);
    return new TextDecoder().decode(decompressed);
}

export default function create(context: LanguageContext): Language {
    function interactions(expressionAddress: Address): Interaction[] {
        return []
    }

    return {
        name: "embedding-vector-language",
        interactions,
        expressionAdapter: {
            get: async (address: Address) => {
                try {
                    const decompressedAddress = decompressUri(address);
                    const expr = JSON.parse(decompressedAddress) as Expression;
                    return expr
                } catch (e) {
                    console.error("caught error", e);
                    return null;
                }
            },
            putAdapter: {
                createPublic: async (content: object): Promise<Address> => {
                    try {
                        const expr = context.agent.createSignedExpression(content)
                        const exprString = JSON.stringify(expr)
                        const compressedExprString = compressUri(exprString);
                        return compressedExprString
                    } catch (e) {
                        console.error("caught error", e);
                        return null;
                    }
                }
            }
        }
    } as Language
}

