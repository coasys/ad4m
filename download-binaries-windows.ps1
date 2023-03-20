choco install wget --no-progress
        
mkdir executor/temp/binary

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-windows-0.1.0.exe -O ./executor/temp/binary/hc.exe

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/holochain-windows-0.1.0.exe -O ./executor/temp/binary/holochain.exe

$SwiplPath = Get-Command swipl.exe | Select-Object -ExpandProperty Definition
Copy-Item $SwiplPath -Destination ./executor/temp/binary/swipl.exe

# Global path where the binary should be copied to
$global_path = "C:\Windows\System32\"

# Check if the global path exists
if (-not (Test-Path $global_path -PathType Container)) {
  Write-Host "Global path not found."
  exit 1
}

Copy-Item ./executor/temp/binary/hc.exe $global_path
Copy-Item ./executor/temp/binary/holochain.exe $global_path
Copy-Item ./executor/temp/binary/swipl.exe $global_path