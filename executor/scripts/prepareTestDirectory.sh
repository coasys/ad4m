#!/bin/bash
[ -d "src/tst-tmp" ] && rm -rf src/tst-tmp
mkdir src/tst-tmp
cd src/tst-tmp
mkdir agents
mkdir languages
mkdir languages/test-language
cp -r ../tests/test-language/build languages/test-language/build

ln -s ../../temp/binary/hc ./hc
ln -s ../../temp/binary/holochain ./holochain
#ln -s ../../temp/swipl/bin/swipl ./swipl
homedir=`echo "$(cd ../../temp/swipl/lib/swipl; pwd)"`
echo '#!/bin/bash' > ./swipl
echo 'cd ../../temp/swipl/bin' >> ./swipl
echo -n "./swipl --home=${homedir} " >> ./swipl
echo '"$@"' >> ./swipl
chmod +x ./swipl
