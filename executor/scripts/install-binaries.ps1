rustup target add wasm32-unknown-unknown

$HoloPath = Get-Command holochain | Select-Object -ExpandProperty Definition

if ($HoloPath -ne $null) {
  if (Test-Path $HoloPath) {
    $Version = Invoke-Expression "$HoloPath --version"
  
    if($Version -notlike '*0.1.3*') {
      cargo install --locked holochain --git https://github.com/holochain/holochain.git --tag holochain-0.1.3 --force --locked
    }
  }
} else {
  cargo install --locked holochain --git https://github.com/holochain/holochain.git --tag holochain-0.1.3 --force --locked
}

$HcPath = Get-Command hc | Select-Object -ExpandProperty Definition

if ($HcPath -ne $null) {
  if (Test-Path $HcPath) {
    $Version = Invoke-Expression "$HcPath --version"
  
    if($Version -notlike '*0.1.3*') {
      cargo install holochain_cli --version 0.1.3 --force --locked
    }
  }
} else {
  cargo install holochain_cli --version 0.1.3 --force --locked
}