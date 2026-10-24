# The shared identity store (AR-01 4.5) as a container image. Build from the repo root:
#
#   docker build -f identity-store/Dockerfile -t identity-store .
#   docker run -p 8080:8080 -v identity-store-data:/data identity-store
#
# The store keeps its SQLite file and its head key under /data. On first start it makes the
# key and prints its public multikey on the "listening" line; AR-09 keeps the key as a secret
# and mounts it instead (IDENTITY_STORE_KEY). `identity-store keygen` makes a key without
# serving.

FROM rust:1.96.1-bookworm AS build
RUN apt-get update && apt-get install -y --no-install-recommends libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*
WORKDIR /src
# Only the store and the crates it builds on. The repo's workspace also holds the executor,
# the CLI and the launcher, which the store does not need, so the image builds them out of a
# one-member workspace on the repo's lockfile.
COPY Cargo.lock ./
COPY ad4m-hash-chain ad4m-hash-chain
COPY ad4m-identity-core ad4m-identity-core
COPY ad4m-ldk/rust ad4m-ldk/rust
COPY identity-store identity-store
RUN printf '[workspace]\nresolver = "2"\nmembers = ["identity-store"]\nexclude = ["ad4m-ldk/rust"]\n' > Cargo.toml \
    && cargo build --release -p identity-store --bin identity-store

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y --no-install-recommends libsqlite3-0 ca-certificates \
    && rm -rf /var/lib/apt/lists/* \
    && useradd --system --home /data --create-home identity-store
COPY --from=build /src/target/release/identity-store /usr/local/bin/identity-store
USER identity-store
WORKDIR /data
ENV IDENTITY_STORE_DB=/data/identity-store.sqlite \
    IDENTITY_STORE_KEY=/data/store-key \
    IDENTITY_STORE_LISTEN=0.0.0.0:8080
EXPOSE 8080
VOLUME ["/data"]
ENTRYPOINT ["identity-store"]
CMD ["serve"]
