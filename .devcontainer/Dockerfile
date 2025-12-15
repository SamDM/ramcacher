FROM debian:12

# -------------
# General setup

# Some install scripts assume bash, this makes following RUN commands use `bash` instead of `sh`.
# The `-c` flag says: "accept a command instead of starting interactive mode".
SHELL ["/bin/bash", "-c"]

# Set sane defaults for the year 2025
ENV TZ=UTC
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8
ENV TERM=xterm-256color
ENV COLORTERM=truecolor

# Set a non-interactive mode for apt and install necessary tools
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

# ------------
# helix editor

ENV EDITOR=hx
ARG HELIX_VERSION=25.07.1
RUN HELIX_FILE_VERSION=$(echo ${HELIX_VERSION} | sed 's/\.0/./g') \
    && curl -LO "https://github.com/helix-editor/helix/releases/download/${HELIX_VERSION}/helix_${HELIX_FILE_VERSION}-1_amd64.deb" \
    && dpkg -i helix_${HELIX_FILE_VERSION}-1_amd64.deb \
    && rm helix_${HELIX_FILE_VERSION}-1_amd64.deb

# ----------
# GitHub CLI
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
    && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
    && apt-get update \
    && apt-get install -y gh \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# --------
# R (cran)

RUN apt-get update && apt-get install -y --no-install-recommends \
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
      pandoc \
      r-base \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# R packages: VSCode integration
# See https://github.com/REditorSupport/vscode-R/issues/1588 for httpgd repo
RUN R -e "install.packages(c('languageserver', 'httpgd'), Ncpus=parallel::detectCores())" \
    && R -e "install.packages('httpgd', repos = c('https://community.r-multiverse.org', 'https://cloud.r-project.org'), Ncpus=parallel::detectCores())"

# R packages: project dependencies
RUN R -e "install.packages('devtools', Ncpus=parallel::detectCores())"

# ----------------
# Dev user account

# Create a user 'vscode' with a UID that matches the host (default 1000)
ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create the user
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m -s /bin/bash $USERNAME \
    # Allow no-password sudo
    && apt-get update \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME
    
# Switch to dev user
USER $USERNAME

# Set the default command for the container to be a bash shell
# This is ideal for a devcontainer as it allows for interactive use
ENTRYPOINT ["bash"]
