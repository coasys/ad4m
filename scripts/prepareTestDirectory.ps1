Remove-Item -Recurse src\test-temp
mkdir src\test-temp
cd src\test-temp
mkdir agents
mkdir languages
mkdir languages\test-language
cp -r ..\tests\test-language\build languages\test-language\build

Remove-Item hc.exe
$HcPath = Get-Command hc.exe | Select-Object -ExpandProperty Definition
Copy-Item $HcPath -Destination hc.exe

Remove-Item holochain.exe
$HoloPath = Get-Command holochain.exe | Select-Object -ExpandProperty Definition
Copy-Item $HoloPath -Destination holochain.exe

Remove-Item lair-keystore.exe
$LkPath = Get-Command lair-keystore.exe | Select-Object -ExpandProperty Definition
Copy-Item $LkPath -Destination lair-keystore.exe

Remove-Item swipl.exe
$LkPath = Get-Command swipl.exe | Select-Object -ExpandProperty Definition
Copy-Item $LkPath -Destination swipl.exe