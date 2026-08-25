#!/bin/bash
[ -d "tst-tmp" ] && rm -rf tst-tmp
mkdir tst-tmp
cd tst-tmp
mkdir agents
mkdir languages
mkdir note

# Prefer the workspace-local, version-pinned `hc` installed by
# scripts/install-hc-toolchain.sh over the host's global one. The version
# pinned in Cargo.lock must match the `holochain_cli_bundle` the executor
# links against, otherwise `install_app_bundle` rejects the packed happ with
# schema-mismatch errors (e.g. `unknown field 'signal_url'`).
REPO_ROOT="$(cd "$(dirname "$0")/../../.." && pwd)"
LOCAL_HC="$REPO_ROOT/.hc-toolchain/bin/hc"
if [ -x "$LOCAL_HC" ]; then
    HC_BIN="$LOCAL_HC"
else
    HC_BIN="$(which hc)"
fi
if [ -z "$HC_BIN" ]; then
    echo "prepareTestDirectory: no hc binary found (looked at $LOCAL_HC and \$PATH). Run scripts/install-hc-toolchain.sh first." >&2
    exit 1
fi
echo "prepareTestDirectory: using hc from $HC_BIN"
"$HC_BIN" --version
ln -s "$HC_BIN" ./hc
# ln -s ../../../executor/temp/binary/holochain ./holochain
#ln -s ../../../executor/temp/swipl/bin/swipl ./swipl
# homedir=`echo "$(cd ../../../executor/temp/swipl/lib/swipl; pwd)"`
# echo '#!/bin/bash' > ./swipl
# echo 'cd ../../../executor/temp/swipl/bin' >> ./swipl
# echo -n "./swipl --home=${homedir} " >> ./swipl
# echo '"$@"' >> ./swipl
# chmod +x ./swipl
