mkdir src\tst-tmp
cd src\tst-tmp
mkdir agents
mkdir languages
mkdir languages\test-language
cp -r ..\tests\test-language\build languages\test-language\build

if (Test-Path hc.exe) {
  Remove-Item hc.exe
}
$HcPath = Get-Command hc.exe | Select-Object -ExpandProperty Definition
Copy-Item $HcPath -Destination hc.exe

if (Test-Path holochain.exe) {
  Remove-Item holochain.exe
}
$HoloPath = Get-Command holochain.exe | Select-Object -ExpandProperty Definition
Copy-Item $HoloPath -Destination holochain.exe

if (Test-Path lair-keystore.exe) {
  Remove-Item lair-keystore.exe
}
$LkPath = Get-Command lair-keystore.exe | Select-Object -ExpandProperty Definition
Copy-Item $LkPath -Destination lair-keystore.exe

if (Test-Path swipl.exe) {
  Remove-Item swipl.exe
}
$SwiplPath = Get-Command swipl.exe | Select-Object -ExpandProperty Definition
Copy-Item $SwiplPath -Destination swipl.exe