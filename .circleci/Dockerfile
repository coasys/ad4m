# Use the CircleCI base image - Ubuntu 22.04 for glibc compatibility with release binaries
FROM cimg/base:edge-22.04

USER root

# Install system dependencies or any other tools you need
RUN apt-get update && apt-get install -y software-properties-common && add-apt-repository universe && apt-get update && apt-get install -y \
    libgtk-3-dev \
    webkit2gtk-4.0 \
    libwebkit2gtk-4.1-dev \
    libjavascriptcoregtk-4.1-dev \
    libappindicator3-dev \
    librsvg2-dev \
    patchelf \
    protobuf-compiler \
    cmake \
    gobjc \
    clang \
    gnustep-devel \
    libobjc4 \
    libgnustep-base-dev \
    libasound2-dev \
    pkg-config \
    fuse \
    libfuse2 \
    mesa-utils \
    mesa-vulkan-drivers \
    squashfs-tools

# Install Go
ENV GO_VERSION=1.24.6
RUN wget https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz -O go.tar.gz \
    && tar -C /usr/local -xzf go.tar.gz \
    && rm go.tar.gz

# Set Go environment variables
ENV GOPATH=/go
ENV PATH="$GOPATH/bin:/usr/local/go/bin:$PATH"

# Install Node
RUN curl -fsSL https://deb.nodesource.com/setup_24.x | bash - && apt-get install -y nodejs


USER circleci

# Add /usr/local/bin to the PATH for the circleci user
RUN echo 'export PATH="/usr/local/bin:$PATH"' >> $HOME/.bashrc

# Install Rust
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
RUN echo 'export PATH="$HOME/.cargo/bin:$PATH"' >> /home/circleci/.bashrc
RUN ~/.cargo/bin/rustup install 1.95
RUN ~/.cargo/bin/rustup default 1.95
RUN ~/.cargo/bin/rustup target add wasm32-unknown-unknown

# Install hc
RUN ~/.cargo/bin/cargo install holochain_cli --git https://github.com/coasys/holochain.git --branch 0.7.0-dev.24-space-override-coasys
RUN ~/.cargo/bin/cargo install kitsune2_bootstrap_srv@0.5.0-dev.2 --features "iroh-relay"

# Install wasm-bindgen-cli (required by @coasys/centralized-agent-language)
RUN ~/.cargo/bin/cargo install wasm-bindgen-cli --version 0.2.118

# Install Deno
RUN curl -fsSL https://deno.land/install.sh | sh
RUN echo 'export DENO_INSTALL="/home/circleci/.deno"' >> /home/circleci/.bashrc
RUN echo 'export PATH="/home/circleci/.deno/bin:$PATH"' >> /home/circleci/.bashrc

# Configure npm to use a directory in the circleci user's home for global packages
RUN mkdir ~/.npm-global \
    && npm config set prefix '~/.npm-global' \
    && echo 'export PATH="~/.npm-global/bin:$PATH"' >> /home/circleci/.bashrc

# Install pnpm
RUN npm install -g pnpm

# Add other setup commands you require

USER root
# Clean up to reduce image size
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Switch back to the circleci user for any non-root actions
USER circleci

# Set the PATH environment variable
ENV PATH="/home/circleci/.cargo/bin:/home/circleci/.deno/bin:/home/circleci/.npm-global/bin:$PATH"

# We need killall for the CLI tests
USER root
RUN apt-get update && apt-get install -y psmisc

# Install CUDA toolkit 12.5 (for --features cuda builds)
RUN apt-get install -y gnupg2 wget && \
    wget https://developer.download.nvidia.com/compute/cuda/repos/ubuntu2204/x86_64/cuda-keyring_1.1-1_all.deb && \
    dpkg -i cuda-keyring_1.1-1_all.deb && \
    rm cuda-keyring_1.1-1_all.deb && \
    apt-get update && \
    apt-get install -y cuda-toolkit-12-5 libcudnn9-dev-cuda-12 && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

# cuDNN headers land in the Debian multiarch path /usr/include/x86_64-linux-gnu/
# but cudarc v0.13.9 only searches /usr/local/cuda*/include/cudnn.h.
# Symlink every cudnn header into the CUDA include dir so cudarc finds them.
RUN for f in /usr/include/x86_64-linux-gnu/cudnn*.h; do \
        ln -sf "$f" "/usr/local/cuda-12.5/include/$(basename $f)"; \
    done

ENV PATH="/usr/local/cuda-12.5/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/local/cuda-12.5/lib64"


# nvidia-smi is part of the NVIDIA driver, not the toolkit.
# In a CI build container (no GPU), create a stub so build scripts that probe for it don't fail.
# bindgen_cuda v0.1.5 calls `nvidia-smi --query-gpu=compute_cap --format=csv`
# and expects a CSV header + value. The stub handles both plain and query modes.
RUN printf '#!/bin/bash\nif echo "$*" | grep -q "query-gpu"; then\n  FIELDS=$(echo "$*" | sed -n "s/.*--query-gpu=\\([^ ]*\\).*/\\1/p")\n  echo "$FIELDS"\n  echo "8.0"\nelse\n  echo "NVIDIA-SMI 535.00  Driver Version: 535.00  CUDA Version: 12.5"\nfi\nexit 0\n' > /usr/local/bin/nvidia-smi && \
    chmod +x /usr/local/bin/nvidia-smi

USER circleci
