choco install wget --no-progress
        
mkdir temp/binary

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0-beta-rc.1/hc-windows-0.1.0-beta-rc.1.exe -O ./temp/binary/hc.exe

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0-beta-rc.1/holochain-windows-0.1.0-beta-rc.1.exe -O ./temp/binary/holochain.exe

$SwiplPath = Get-Command swipl.exe | Select-Object -ExpandProperty Definition
Copy-Item $SwiplPath -Destination ./temp/binary/swipl.exe