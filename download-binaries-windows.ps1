mkdir rust-executor/temp/binary

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.0/hc-windows-0.1.0.exe -O ./executor/temp/binary/hc.exe

# Global path where the binary should be copied to
$global_path = "C:\Windows\System32\"

# Check if the global path exists
if (-not (Test-Path $global_path -PathType Container)) {
  Write-Host "Global path not found."
  exit 1
}

Copy-Item ./rust-executor/temp/binary/hc.exe $global_path