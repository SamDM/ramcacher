FROM debian:12

# -------------
# General setup

# Set a non-interactive mode for apt and install necessary tools
ENV TZ=UTC
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update && apt-get install -y --no-install-recommends \
      # needed for later
      build-essential \
      ca-certificates \
      curl \
      dirmngr \
      gnupg \
      locales \
      lsb-release \
      openssh-client \
      software-properties-common \
      sudo \
      wget \
      xz-utils \
      # for dev user convenience
      git \
      ncdu \
    && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen \
    && locale-gen \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Some install scripts assume bash, this makes following RUN commands use `bash` instead of `sh`.
# The `-c` flag says: "accept a command instead of starting interactive mode".
SHELL ["/bin/bash", "-c"]

# -------------
# Helix editor

ARG HELIX_VERSION=25.07.1
RUN HELIX_FILE_VERSION=$(echo ${HELIX_VERSION} | sed 's/\.0/./g') \
    && curl -LO "https://github.com/helix-editor/helix/releases/download/${HELIX_VERSION}/helix_${HELIX_FILE_VERSION}-1_amd64.deb" \
    && dpkg -i helix_${HELIX_FILE_VERSION}-1_amd64.deb \
    && rm helix_${HELIX_FILE_VERSION}-1_amd64.deb

# -------------
# AI assistants

ARG NVM_VERSION=0.40.3
ARG NODEJS_VERSION=24

ENV NVM_DIR=/opt/nvm
ENV NODE_PATH=$NVM_DIR/versions/node/v${NODEJS_VERSION}/lib/node_modules
ENV PATH=$NVM_DIR/versions/node/v${NODEJS_VERSION}/bin:$PATH

RUN mkdir -p $NVM_DIR \
    && curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v${NVM_VERSION}/install.sh | bash \
    && . "$NVM_DIR/nvm.sh" \
    && nvm install $NODEJS_VERSION \
    && nvm alias default $NODEJS_VERSION \
    && nvm use $NODEJS_VERSION \
    && npm install -g @anthropic-ai/claude-code @google/gemini-cli \
    && nvm cache clear \
    && npm cache clean --force

RUN ln -s $NVM_DIR/versions/node/v${NODEJS_VERSION}*/bin/node /usr/local/bin/node \
    && ln -s $NVM_DIR/versions/node/v${NODEJS_VERSION}*/bin/npm /usr/local/bin/npm \
    && ln -s $NVM_DIR/versions/node/v${NODEJS_VERSION}*/bin/npx /usr/local/bin/npx \
    && ln -s $NVM_DIR/versions/node/v${NODEJS_VERSION}*/bin/claude /usr/local/bin/claude \
    && ln -s $NVM_DIR/versions/node/v${NODEJS_VERSION}*/bin/gemini /usr/local/bin/gemini

# --------
# R (cran)

RUN wget -qO- https://cloud.r-project.org/bin/linux/debian/marutter_pubkey.asc | tee /etc/apt/trusted.gpg.d/cran_debian_key.asc \
    && add-apt-repository "deb https://cloud.r-project.org/bin/linux/debian $(lsb_release -cs)-cran40/" \
    && apt-get update && apt-get install -y --no-install-recommends \
      r-base \
      libcurl4-openssl-dev \
      libfontconfig1-dev \
      libfreetype6-dev \
      libfribidi-dev \
      libharfbuzz-dev \
      libjpeg-dev \
      libpng-dev \
      libssh-dev \
      libssl-dev \
      libtiff5-dev \
      libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# R packages: VSCode integration
# See https://github.com/REditorSupport/vscode-R/issues/1588 for httpgd repo
RUN R -e "install.packages(c('languageserver', 'httpgd'), Ncpus=parallel::detectCores())" \
    && R -e "install.packages('httpgd', repos = c('https://community.r-multiverse.org', 'https://cloud.r-project.org'), Ncpus=parallel::detectCores())"

# R packages: project dependencies
RUN R -e "install.packages('devtools', Ncpus=parallel::detectCores())"


# -----
# Done!

# Set the default command for the container to be a bash shell
# This is ideal for a devcontainer as it allows for interactive use
ENTRYPOINT ["bash"]
