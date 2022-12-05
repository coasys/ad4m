choco install wget --no-progress
        
mkdir temp/binary

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.0.174/hc-windows-0.0.69.exe -O ./temp/binary/hc.exe

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.0.174/holochain-windows-0.0.174.exe -O ./temp/binary/holochain.exe
