choco install wget --no-progress
        
mkdir temp/binary

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-windows-0.0.56.exe -O ./temp/binary/hc.exe

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-windows-0.0.161.exe -O ./temp/binary/holochain.exe

wget https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-windows-0.2.0.exe -O ./temp/binary/lair-keystore.exe