/**
 * Bundle proof for `@coasys/ad4m/model`: the smallest mountable language.
 * It imports the LDK and the model-only SDK entry — nothing from the
 * client SDK — and `esbuild.ts` bundles it exactly the way the bootstrap
 * languages bundle theirs (Deno esbuild + esbuild_deno_loader, `ad4m:host`
 * external, workspace aliases for `@coasys/ad4m-ldk` and `@coasys/ad4m/model`).
 */
import { defineLanguage } from "@coasys/ad4m-ldk";
import { Model, Optional, Flag } from "@coasys/ad4m/model";

@Model({
    name: "BundleThing",
    namespace: "bundle://",
    targetClass: "bundle://Thing",
    mount: true,
    constructorActions: { interaction: "create", source: "{language}://class/BundleThing" },
    destructorActions: { interaction: "delete" },
})
class BundleThing {
    @Flag({ through: "rdf://type", value: "bundle://Thing" })
    type!: string;

    @Optional({ through: "bundle://title", setter: { interaction: "update" } })
    title!: string;
}

const lang = defineLanguage({
    name: "bundle-proof",
    async init() {},
    mount: {
        classes: [BundleThing],
        version: "1",
        snapshot: () => ({ entries: [] }),
    },
});

export default lang;
export const { name, init, mountShapes, mountSnapshot } = lang;
