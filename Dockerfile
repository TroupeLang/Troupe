FROM node:22-slim AS base
ENV TROUPE=/Troupe
ENV LANG=C.UTF-8
WORKDIR $TROUPE
RUN npm install -g typescript

# Install runtime and test dependencies.
RUN apt-get update && apt-get install -qy procps jq diffutils socat lsof make && rm -rf /var/lib/apt/lists/*

# Image for building everything.
FROM base AS builder
WORKDIR $TROUPE

# Install build dependencies (includes Stack's prerequisites so the installer doesn't need apt).
RUN apt-get update && apt-get install -qy \
    g++ curl make libnuma-dev xz-utils \
    libffi-dev libgmp-dev zlib1g-dev git gnupg netbase \
    && rm -rf /var/lib/apt/lists/*

# Install Stack (GHC is managed by Stack via the resolver in stack.yaml).
RUN curl -sSL https://get.haskellstack.org/ | sh

# Copy dependency manifests and install npm packages (cached unless package.json changes).
COPY package.json package-lock.json ./
COPY p2p-tools/package.json p2p-tools/
RUN --mount=type=cache,target=/root/.npm npm install

# Copy compiler sources and build (cached unless compiler/ changes).
COPY compiler/ compiler/
COPY Makefile ./
RUN --mount=type=cache,target=/root/.stack make compiler

# Copy runtime sources and build (cached unless rt/ changes).
COPY rt/ rt/
RUN make rt

# Copy p2p-tools and build (cached unless p2p-tools/ changes).
COPY p2p-tools/ p2p-tools/
RUN make p2p-tools

# Copy everything else for lib, trp-rt, and notebook.
COPY . .
RUN make lib
RUN make trp-rt
RUN make notebook

# Create runner image.
FROM base
ENV TROUPE=/Troupe
WORKDIR $TROUPE

# Copy files from builder image.
COPY --from=builder $TROUPE/bin $TROUPE/bin
COPY --from=builder $TROUPE/examples $TROUPE/examples
COPY --from=builder $TROUPE/lib $TROUPE/lib
COPY --from=builder $TROUPE/p2p-tools $TROUPE/p2p-tools
COPY --from=builder $TROUPE/rt $TROUPE/rt
COPY --from=builder $TROUPE/tests $TROUPE/tests
COPY --from=builder $TROUPE/trp-rt $TROUPE/trp-rt
COPY --from=builder $TROUPE/node_modules $TROUPE/node_modules
COPY --from=builder $TROUPE/local.sh $TROUPE/local.sh
COPY --from=builder $TROUPE/network.sh $TROUPE/network.sh
COPY --from=builder $TROUPE/pini.sh $TROUPE/pini.sh
COPY --from=builder $TROUPE/rollup.config.js $TROUPE/rollup.config.js
COPY --from=builder $TROUPE/trustmap.json $TROUPE/trustmap.json
COPY --from=builder $TROUPE/scripts $TROUPE/scripts
COPY --from=builder $TROUPE/Makefile $TROUPE/Makefile
COPY --from=builder $TROUPE/notebook $TROUPE/notebook
COPY --from=builder $TROUPE/notebook.sh $TROUPE/notebook.sh
COPY --from=builder $TROUPE/.troupe-root $TROUPE/.troupe-root

# Create necessary directories
RUN mkdir -p $TROUPE/out

# Expose notebook server port
EXPOSE 8888

# Command to overwrite the node image command, that starts in node.
CMD ["bash"]

