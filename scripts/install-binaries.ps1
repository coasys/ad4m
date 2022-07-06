rustup target add wasm32-unknown-unknown

$HoloPath = Get-Command holochain | Select-Object -ExpandProperty Definition

if (Test-Path $HoloPath) {
  $Version = Invoke-Expression "$HoloPath --version"

  if($Version -notlike '*0.0.146*') {
    cargo install --locked holochain --git https://github.com/holochain/holochain.git --tag holochain-0.0.146 --force
  }
}

$HcPath = Get-Command hc | Select-Object -ExpandProperty Definition

if (Test-Path $HcPath) {
  $Version = Invoke-Expression "$HcPath --version"

  if($Version -notlike '*0.0.44*') {
    cargo install holochain_cli --version 0.0.44 --force
  }
}

$LKPath = Get-Command lair-keystore | Select-Object -ExpandProperty Definition

if (Test-Path $LKPath) {
  $Version = Invoke-Expression "$LKPath --version"

  if($Version -notlike '*0.2.0*') {
    cargo install lair_keystore --version 0.2.0 --force
  }
}