choco install wget --no-progress
# $env:path += ";C:\Program Files\swipl\bin"

mkdir tst-tmp
cd tst-tmp
mkdir agents
mkdir languages
mkdir note

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-windows-0.1.0.exe -O ./hc.exe

# wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/holochain-windows-0.1.0.exe -O ./holochain.exe

# $SwiplPath = Get-Command swipl.exe | Select-Object -ExpandProperty Definition
# Copy-Item $SwiplPath -Destination swipl.exe
