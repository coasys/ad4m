choco install wget --no-progress
        
mkdir temp/binary

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-windows-0.0.56.exe
Copy-Item hc-windows-0.0.56.exe -Destination temp/binary/hc.exe

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-windows-0.0.161.exe
Copy-Item holochain-windows-0.0.161.exe -Destination temp/binary/holochain.exe

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-windows-0.2.0.exe
Copy-Item lair-keystore-windows-0.2.0.exe -Destination temp/binary/lair-keystore.exe