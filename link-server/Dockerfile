FROM node:22-slim AS build
WORKDIR /app
# build-essential + python3: better-sqlite3 falls back to compiling its
# native addon from source when no prebuilt binary matches the image's
# platform/Node ABI.
RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential python3 \
    && rm -rf /var/lib/apt/lists/*
COPY package*.json ./
RUN npm ci
COPY tsconfig.json ./
COPY src/ ./src/
RUN npm run build
# Strip devDependencies (typescript, tsx, @types/*) now that the build is
# done, so the compiled native addon ships without dragging the toolchain
# (or build-essential) into the runtime image.
RUN npm prune --omit=dev

FROM node:22-slim
WORKDIR /app
COPY package*.json ./
COPY --from=build /app/node_modules/ ./node_modules/
COPY --from=build /app/dist/ ./dist/
EXPOSE 3456
VOLUME /data
# PORT, DATA_DIR, AUTO_ADMIT, SKIP_LINK_VERIFICATION read from env vars
# (see src/index.ts parseArgs). Defaults: port 3456, data ./data.
ENV DATA_DIR=/data
HEALTHCHECK --interval=30s --timeout=5s --start-period=5s --retries=3 \
  CMD node -e "fetch('http://localhost:3456/health').then(r=>{if(!r.ok)throw r.status}).catch(()=>process.exit(1))"
CMD ["node", "dist/index.js"]
