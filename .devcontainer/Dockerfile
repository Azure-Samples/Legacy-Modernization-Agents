FROM mcr.microsoft.com/devcontainers/dotnet:8.0

# Install OpenJDK and Maven for Quarkus development
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends gnupg curl ca-certificates \
    && apt-get -y install --no-install-recommends openjdk-17-jdk maven \
    && apt-get clean -y && rm -rf /var/lib/apt/lists/*

# Set JAVA_HOME
ENV JAVA_HOME=/usr/lib/jvm/java-17-openjdk-arm64

# [Optional] Uncomment this section to install additional OS packages.
# RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
#     && apt-get -y install --no-install-recommends <your-package-list-here>

# [Optional] Uncomment if you want to install more global node packages
# RUN su vscode -c "source /usr/local/share/nvm/nvm.sh && npm install -g <your-package-list-here>"
