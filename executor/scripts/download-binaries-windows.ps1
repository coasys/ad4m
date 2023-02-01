choco install wget --no-progress
        
mkdir temp/binary

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps/hc-windows.exe -O ./temp/binary/hc.exe

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps/holochain-windows.exe -O ./temp/binary/holochain.exe

$SwiplPath = Get-Command swipl.exe | Select-Object -ExpandProperty Definition
Copy-Item $SwiplPath -Destination ./temp/binary/swipl.exe