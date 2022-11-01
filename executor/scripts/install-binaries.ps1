rustup target add wasm32-unknown-unknown

$HoloPath = Get-Command holochain | Select-Object -ExpandProperty Definition

if ($HoloPath -ne $null) {
  if (Test-Path $HoloPath) {
    $Version = Invoke-Expression "$HoloPath --version"
  
    if($Version -notlike '*0.0.161*') {
      cargo install --locked holochain --git https://github.com/holochain/holochain.git --tag holochain-0.0.161 --force
    }
  }
} else {
  cargo install --locked holochain --git https://github.com/holochain/holochain.git --tag holochain-0.0.161 --force
}

$HcPath = Get-Command hc | Select-Object -ExpandProperty Definition

if ($HcPath -ne $null) {
  if (Test-Path $HcPath) {
    $Version = Invoke-Expression "$HcPath --version"
  
    if($Version -notlike '*0.0.56*') {
      cargo install holochain_cli --version 0.0.56 --force
    }
  }
} else {
  cargo install holochain_cli --version 0.0.56 --force
}

$LKPath = Get-Command lair-keystore | Select-Object -ExpandProperty Definition

if ($LKPath -ne $null) {
  if (Test-Path $LKPath) {
    $Version = Invoke-Expression "$LKPath --version"
  
    if($Version -notlike '*0.2.0*') {
      cargo install lair_keystore --version 0.2.0 --force
    }
  }
} else {
  cargo install lair_keystore --version 0.2.0 --force
}