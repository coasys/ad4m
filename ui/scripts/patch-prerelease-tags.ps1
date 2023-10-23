(Get-Content -Path 'package.json' -Raw) -replace '"version": "(.*)-prerelease",', '"version": "$1",' | Set-Content -Path 'package.json'

(Get-Content -Path 'src-tauri/tauri.conf.json' -Raw) -replace '"version": "(.*)-prerelease"', '"version": "$1"' | Set-Content -Path 'src-tauri/tauri.conf.json'

(Get-Content -Path 'src-tauri/Cargo.toml' -Raw) -replace 'version = "(.*)-prerelease\.0"', 'version = "$1"' | Set-Content -Path 'src-tauri/Cargo.toml'
