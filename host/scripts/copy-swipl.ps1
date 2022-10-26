choco install swi-prolog
Copy-Item -Recurse -Path "C:\Program Files\swipl" -Destination temp/
Copy-Item -Path node_modules/swipl-stdio/top.pl -Destination temp/swipl