FROM haskell:8.4.3 as BUILD_COMPILER
RUN apt-get update && apt-get install -y \
    alex \
    happy \
    curl \
    gnupg
RUN curl -sL https://deb.nodesource.com/setup_14.x  | bash -
RUN apt-get -y install nodejs
RUN npm install -g yarn typescript
WORKDIR /workspace
ENV STACK_OPTS=--system-ghc
ENV TROUPE /workspace
COPY . .
RUN make stack
RUN yarn install
RUN make rt
RUN make test
RUN make dist

FROM node:lts-buster
ENV TROUPE=/Troupe \
    PATH=/Troupe/bin:${PATH}
COPY --from=BUILD_COMPILER /workspace/build/Troupe /Troupe