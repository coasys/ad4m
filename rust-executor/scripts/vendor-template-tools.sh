#!/usr/bin/env bash
# Copy WE's template tools into the executor: build the single-file bundle `@we/agent-tools` ships
# from a WE checkout, put it where `tool_bundles/builtin.rs` compiles it in, and write beside it
# what the bundle's tools are and which WE commit it came from.
#
# The executor runs this copy for every agent that edits a WE template, so it should follow WE:
# run it whenever a WE release changes the template system (components, validation, patches, the
# reference), and open a PR with the two files it writes. `cargo test tool_bundles` checks the two
# agree.
#
# Usage: rust-executor/scripts/vendor-template-tools.sh <path to a WE checkout>
set -euo pipefail

we="${1:?usage: vendor-template-tools.sh <path to a WE checkout>}"
we="$(cd "$we" && pwd)"
here="$(cd "$(dirname "$0")/.." && pwd)"
out="$here/src/tool_bundles"

if ! git -C "$we" diff --quiet HEAD -- packages; then
  echo "The WE checkout has uncommitted changes under packages/; vendor from a clean commit." >&2
  exit 1
fi

(cd "$we" && pnpm --filter @we/agent-tools build >/dev/null)
cp "$we/packages/agent-tools/dist/agent-tools.bundle.js" "$out/template_tools.bundle.js"

# Node reads the bundle as a module only under an .mjs name.
module="$(mktemp -d)/bundle.mjs"
cp "$out/template_tools.bundle.js" "$module"
node --input-type=module - "$module" \
  "$(node -p "require('$we/packages/agent-tools/package.json').version")" \
  "$(git -C "$we" rev-parse HEAD)" >"$out/template_tools.json" <<'JS'
const [module, version, commit] = process.argv.slice(2);
const { toolsets } = await import(module);
const { appliesTo, tools } = toolsets.template;
console.log(JSON.stringify({ package: '@we/agent-tools', version, commit, appliesTo, tools }, null, 2));
JS
rm -r "$(dirname "$module")"

echo "Vendored @we/agent-tools from $(git -C "$we" rev-parse --short HEAD) ($(wc -c <"$out/template_tools.bundle.js") bytes)."
