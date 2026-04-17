FROM mcr.microsoft.com/devcontainers/dotnet:10.0

# Install system dependencies (including ICU for .NET globalization)
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get -y install --no-install-recommends \
       gnupg \
       curl \
       ca-certificates \
       wget \
       git \
       sqlite3 \
       openjdk-17-jdk \
       maven \
       jq \
       vim \
       nano \
       htop \
       libicu-dev \
    && apt-get clean -y && rm -rf /var/lib/apt/lists/*

# Add vscode user to docker group for Docker socket access
RUN groupadd -f docker && usermod -aG docker vscode

# Set JAVA_HOME (architecture-agnostic)
RUN if [ "$(uname -m)" = "aarch64" ]; then \
      export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-arm64; \
    else \
      export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64; \
    fi && \
    echo "export JAVA_HOME=$JAVA_HOME" >> /etc/environment && \
    echo "export PATH=$JAVA_HOME/bin:\$PATH" >> /etc/bash.bashrc

# Create helpful aliases for the dev container user
RUN echo 'alias neo4j-status="docker ps | grep neo4j"' >> /home/vscode/.bashrc && \
    echo 'alias neo4j-start="docker-compose up -d neo4j"' >> /home/vscode/.bashrc && \
    echo 'alias neo4j-stop="docker-compose down neo4j"' >> /home/vscode/.bashrc && \
    echo 'alias neo4j-logs="docker logs cobol-migration-neo4j"' >> /home/vscode/.bashrc && \
    echo 'alias portal-start="cd /workspaces/Legacy-Modernization-Agents/McpChatWeb && dotnet run --urls http://localhost:5028"' >> /home/vscode/.bashrc && \
    echo 'alias migration-run="cd /workspaces/Legacy-Modernization-Agents && ./doctor.sh run"' >> /home/vscode/.bashrc && \
    echo 'alias reverse-eng="cd /workspaces/Legacy-Modernization-Agents && ./doctor.sh reverse-eng"' >> /home/vscode/.bashrc && \
    echo 'alias demo="cd /workspaces/Legacy-Modernization-Agents/helper-scripts && ./demo.sh"' >> /home/vscode/.bashrc && \
    echo 'alias verify-setup="cd /workspaces/Legacy-Modernization-Agents && bash .devcontainer/verify-setup.sh"' >> /home/vscode/.bashrc

ENV ASPNETCORE_ENVIRONMENT=Development
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1
ENV DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1
ENV ASPNETCORE_URLS=http://localhost:5028
ENV MIGRATION_DB_PATH=/workspaces/Legacy-Modernization-Agents/Data/migration.db

# Welcome message with status check
RUN echo 'echo ""' >> /home/vscode/.bashrc && \
    echo 'echo "╔══════════════════════════════════════════════════════════╗"' >> /home/vscode/.bashrc && \
    echo 'echo "║  🚀 COBOL Migration Dev Container Ready!              ║"' >> /home/vscode/.bashrc && \
    echo 'echo "╚══════════════════════════════════════════════════════════╝"' >> /home/vscode/.bashrc && \
    echo 'echo ""' >> /home/vscode/.bashrc && \
    echo 'echo "📊 Quick Commands:"' >> /home/vscode/.bashrc && \
    echo 'echo "  demo           - Start demo with existing data (runs ./helper-scripts/demo.sh)"' >> /home/vscode/.bashrc && \
    echo 'echo "  migration-run  - Run full COBOL migration"' >> /home/vscode/.bashrc && \
    echo 'echo "  reverse-eng    - Run reverse engineering"' >> /home/vscode/.bashrc && \
    echo 'echo "  portal-start   - Start web portal manually"' >> /home/vscode/.bashrc && \
    echo 'echo "  verify-setup   - Check if everything is configured"' >> /home/vscode/.bashrc && \
    echo 'echo "  neo4j-status   - Check Neo4j container status"' >> /home/vscode/.bashrc && \
    echo 'echo ""' >> /home/vscode/.bashrc && \
    echo 'echo "🌐 Endpoints:"' >> /home/vscode/.bashrc && \
    echo 'echo "  Portal:        http://localhost:5028"' >> /home/vscode/.bashrc && \
    echo 'echo "  Neo4j Browser: http://localhost:7474"' >> /home/vscode/.bashrc && \
    echo 'echo "  Neo4j Bolt:    bolt://localhost:7687"' >> /home/vscode/.bashrc && \
    echo 'echo ""' >> /home/vscode/.bashrc
