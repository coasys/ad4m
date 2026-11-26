# Thin AD4M executor image for the agent-harness scenarios.
#
# Wraps binaries that were already built on the host (target/release/ad4m-executor
# and target/release/ad4m) with the repo's docker-entrypoint.sh and the local
# bootstrap-language seed, so the scenarios test the executor of this checkout
# without the root Dockerfile's full toolchain build. Built by build-image.sh,
# which stages the build context; do not `docker build` this directory directly.
#
# The base must ship a glibc at least as new as the host that compiled the
# binaries (CI: Ubuntu 25.04), hence 26.04.
FROM ubuntu:26.04@sha256:6a06111c3b0a8600feadb270aec771dc239bf407b25625f93f44ab00e11f4d3c

ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
 && apt-get install -y --no-install-recommends ca-certificates curl jq gosu \
 && rm -rf /var/lib/apt/lists/*
# /usr/local/lib/ad4m exists before the COPY below: a COPY --chmod=644 that
# creates it gives the directory 644, and the entrypoint cannot then read ai-env.sh.
RUN useradd -m -s /bin/bash ad4m && mkdir -p /data /opt/ad4m /usr/local/lib/ad4m && chown ad4m:ad4m /data

COPY ad4m-executor ad4m /usr/local/bin/
COPY seed/docker_seed.json /opt/ad4m/docker_seed.json
COPY seed/languages/ /opt/ad4m/bootstrap-languages/
COPY seed/language-language-kv/ /opt/ad4m/language-language-kv/
COPY --chmod=755 docker-entrypoint.sh /usr/local/bin/docker-entrypoint.sh
# Sourced by docker-entrypoint.sh, at the path the root Dockerfile uses.
COPY --chmod=644 ai-env.sh /usr/local/lib/ad4m/ai-env.sh
COPY SOURCE /opt/ad4m/SOURCE

WORKDIR /data
EXPOSE 12000 3001
ENTRYPOINT ["/usr/local/bin/docker-entrypoint.sh"]
