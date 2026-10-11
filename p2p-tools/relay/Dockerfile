# Build context: the repository root.
FROM node:22-slim AS build
WORKDIR /build
COPY package.json package-lock.json ./
COPY p2p-tools/ p2p-tools/
RUN --mount=type=cache,target=/root/.npm npm ci --workspace p2p-tools --ignore-scripts
RUN npx tsc -p p2p-tools && npx tsc -p p2p-tools/relay \
    && npm prune --workspace p2p-tools --omit=dev

FROM node:22-slim
LABEL org.opencontainers.image.source="https://github.com/TroupeLang/Troupe" \
      org.opencontainers.image.description="Troupe 2 libp2p circuit relay"
WORKDIR /app
COPY --from=build /build/node_modules ./node_modules
COPY --from=build /build/p2p-tools/package.json ./p2p-tools/package.json
COPY --from=build /build/p2p-tools/built/mkid.mjs ./p2p-tools/built/mkid.mjs
COPY --from=build /build/p2p-tools/relay/relay.mjs ./p2p-tools/relay/relay.mjs
COPY p2p-tools/relay/entrypoint.sh /entrypoint.sh
ENV DEBUG=libp2p:circuit-relay:server
VOLUME /app/keys
EXPOSE 5555
ENTRYPOINT ["/bin/bash", "/entrypoint.sh"]
