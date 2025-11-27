import type { Address, Interaction, Expression, Language, LanguageContext } from "https://esm.sh/v135/@coasys/ad4m@0.5.0";
import { exists } from "https://deno.land/std@0.184.0/fs/mod.ts";
import { join } from "https://deno.land/std@0.184.0/path/mod.ts";

export default function create(context: LanguageContext): Language {
    const expressions = new Array<Expression>()

    let storagePath = "";

    //@ts-ignore
    if ("storagePath" in context.customSettings) { storagePath = context.customSettings["storagePath"] } else { storagePath = "./tst-tmp/note" };

    function interactions(expressionAddress: Address): Interaction[] {
        return []
    }

    return {
        name: "note-store",
        interactions,
        expressionAdapter: {
            get: async (address: Address) => {
                let path = join(storagePath, `${address}.txt`)
                console.log("note-store language trying to get at path:", path);
                try {
                    await exists(path)
                    return JSON.parse(Deno.readTextFileSync(path));
                } catch (e) {
                    console.error("caught error", e);
                    return null;
                }
            },
            putAdapter: {
                createPublic: async (content: object): Promise<Address> => {
                    const expr = context.agent.createSignedExpression(content)
                    const exprString = JSON.stringify(expr)
                    // @ts-ignore
                    const hash = UTILS.hash(exprString);
                    Deno.writeTextFileSync(join(storagePath, `${hash}.txt`), exprString);
                    return hash
                }
            }
        }
    } as Language
}

