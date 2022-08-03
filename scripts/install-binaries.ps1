rustup target add wasm32-unknown-unknown

$HoloPath = Get-Command holochain | Select-Object -ExpandProperty Definition
echo $HoloPath
if (Test-Path $HoloPath) {
  $Version = Invoke-Expression "$HoloPath --version"

  echo $Version

  if($Version -notlike '*0.0.151*') {
    cargo install --locked holochain --git https://github.com/holochain/holochain.git --tag holochain-0.0.151 --force
  }
}

$HcPath = Get-Command hc | Select-Object -ExpandProperty Definition
echo $HcPath

if (Test-Path $HcPath) {
  $Version = Invoke-Expression "$HcPath --version"

  echo $Version

  if($Version -notlike '*0.0.49*') {
    echo $Version
    cargo install holochain_cli --version 0.0.49 --force
  }
}

$LKPath = Get-Command lair-keystore | Select-Object -ExpandProperty Definition
echo $LKPath

if (Test-Path $LKPath) {
  $Version = Invoke-Expression "$LKPath --version"

  echo $Version

  if($Version -notlike '*0.2.0*') {
    cargo install lair_keystore --version 0.2.0 --force
  }
}