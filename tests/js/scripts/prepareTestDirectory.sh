#!/bin/bash
[ -d "tst-tmp" ] && rm -rf tst-tmp
mkdir tst-tmp
cd tst-tmp
mkdir agents
mkdir languages
mkdir languages/test-language
cp -r ../test-language/build languages/test-language/build

ln -s ../../../executor/temp/binary/hc ./hc
ln -s ../../../executor/temp/binary/holochain ./holochain
#ln -s ../../../executor/temp/swipl/bin/swipl ./swipl
homedir=`echo "$(cd ../../../executor/temp/swipl/lib/swipl; pwd)"`
echo '#!/bin/bash' > ./swipl
echo 'cd ../../../executor/temp/swipl/bin' >> ./swipl
echo -n "./swipl --home=${homedir} " >> ./swipl
echo '"$@"' >> ./swipl
chmod +x ./swipl
