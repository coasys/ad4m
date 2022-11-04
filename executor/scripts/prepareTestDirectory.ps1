choco install wget --no-progress

mkdir src\tst-tmp
cd src\tst-tmp
mkdir agents
mkdir languages
mkdir languages\test-language
cp -r ..\tests\test-language\build languages\test-language\build

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-windows-0.0.56.exe -O ./hc.exe

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-windows-0.0.161.exe -O ./holochain.exe

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-windows-0.2.0.exe -O ./lair-keystore.exe

$SwiplPath = Get-Command swipl.exe | Select-Object -ExpandProperty Definition
Copy-Item $SwiplPath -Destination swipl.exe