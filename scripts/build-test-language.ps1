Remove-Item -Recurse -Force src/test-temp
Set-Location src/tests/test-language
npm i
npm run build
