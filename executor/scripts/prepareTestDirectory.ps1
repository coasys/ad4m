choco install wget --no-progress

mkdir src\tst-tmp
cd src\tst-tmp
mkdir agents
mkdir languages
mkdir languages\test-language
cp -r ..\tests\test-language\build languages\test-language\build

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-windows-0.1.0.exe -O ./hc.exe

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/holochain-windows-0.1.0.exe -O ./holochain.exe

$SwiplPath = Get-Command swipl.exe | Select-Object -ExpandProperty Definition
Copy-Item $SwiplPath -Destination swipl.exe
