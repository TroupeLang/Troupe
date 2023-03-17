FROM fpco/stack-build:lts-20.14
ENV TROUPE /Troupe
ENV STACK_OPTS --system-ghc
WORKDIR $TROUPE
COPY compiler compiler
COPY Makefile .
RUN make stack

FROM ubuntu:bionic
ENV TROUPE /Troupe
RUN apt-get update \
  && apt-get install -y curl gnupg build-essential \
  && curl --silent --location https://deb.nodesource.com/setup_14.x | bash - \
  && curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add - \
  && echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
  && apt-get remove -y --purge cmdtest \
  && apt-get update \
  && apt-get install -y nodejs yarn
RUN apt-get install -y curl python3.7 python3.7-dev python3.7-distutils
RUN update-alternatives --install /usr/bin/python python /usr/bin/python3.7 1
RUN update-alternatives --set python /usr/bin/python3.7
RUN curl -s https://bootstrap.pypa.io/get-pip.py -o get-pip.py && \
    python get-pip.py --force-reinstall && \
    rm get-pip.py
RUN npm install -g typescript    
COPY package.json .
RUN yarn install
WORKDIR /Troupe/
COPY --from=0 /Troupe/bin bin
COPY rt rt
COPY trp-rt trp-rt
COPY patches patches
COPY relay relay
COPY lib lib
COPY Makefile .
COPY local.sh .
COPY network.sh .
COPY examples examples
COPY pini.sh .
RUN yarn
RUN make rt 
RUN make libs 
RUN make service

