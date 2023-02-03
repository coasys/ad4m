choco install wget --no-progress
        
mkdir temp/binary

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.3/hc-windows-0.1.3.exe -O ./temp/binary/hc.exe

wget https://github.com/perspect3vism/ad4m/releases/download/binary-deps-0.1.3/holochain-windows-0.1.3.exe -O ./temp/binary/holochain.exe
