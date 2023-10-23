(Get-Content -Path 'package.json' -Raw) -replace '"version": "(.*)",', '"version": "$1-prerelease",' | Set-Content -Path 'package.json'

(Get-Content -Path 'src-tauri/tauri.conf.json' -Raw) -replace '"version": "(.*)",', '"version": "$1-prerelease",' | Set-Content -Path 'src-tauri/tauri.conf.json'

(Get-Content -Path 'src-tauri/Cargo.toml' -Raw) -replace 'version = "(.*)"', 'version = "$1-prerelease.0"' | Set-Content -Path 'src-tauri/Cargo.toml'
