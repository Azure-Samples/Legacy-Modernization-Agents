FROM mcr.microsoft.com/devcontainers/dotnet:9.0

# Install system dependencies
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
    && apt-get clean -y && rm -rf /var/lib/apt/lists/*

# Set JAVA_HOME (architecture-agnostic)
RUN if [ "$(uname -m)" = "aarch64" ]; then \
      export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-arm64; \
    else \
      export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64; \
    fi && \
    echo "export JAVA_HOME=$JAVA_HOME" >> /etc/environment && \
    echo "export PATH=$JAVA_HOME/bin:\$PATH" >> /etc/bash.bashrc

# Install Docker CLI and Docker Compose (for docker-compose in dev container)
RUN curl -fsSL https://get.docker.com | sh && \
    curl -L "https://github.com/docker/compose/releases/latest/download/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose && \
    chmod +x /usr/local/bin/docker-compose

# Install Neo4j cypher-shell for database queries
RUN wget -O - https://debian.neo4j.com/neotechnology.gpg.key | apt-key add - && \
    echo 'deb https://debian.neo4j.com stable latest' > /etc/apt/sources.list.d/neo4j.list && \
    apt-get update && \
    apt-get install -y cypher-shell && \
    apt-get clean -y && rm -rf /var/lib/apt/lists/*

# Set up working directory permissions and create necessary folders
RUN mkdir -p /workspace/Data \
             /workspace/Logs \
             /workspace/source \
             /workspace/output \
             /workspace/McpChatWeb/wwwroot \
             /workspace/neo4j_data \
             /workspace/neo4j_logs \
             /workspace/neo4j_import && \
    chown -R vscode:vscode /workspace

# Create helpful aliases for the dev container user
RUN echo 'alias neo4j-status="docker ps | grep neo4j"' >> /home/vscode/.bashrc && \
    echo 'alias neo4j-start="docker-compose up -d neo4j"' >> /home/vscode/.bashrc && \
    echo 'alias neo4j-stop="docker-compose down neo4j"' >> /home/vscode/.bashrc && \
    echo 'alias neo4j-logs="docker logs cobol-migration-neo4j"' >> /home/vscode/.bashrc && \
    echo 'alias portal-start="cd /workspace/McpChatWeb && dotnet run --urls http://localhost:5028"' >> /home/vscode/.bashrc && \
    echo 'alias migration-run="cd /workspace && ./doctor.sh run"' >> /home/vscode/.bashrc && \
    echo 'alias reverse-eng="cd /workspace && ./doctor.sh reverse-eng"' >> /home/vscode/.bashrc && \
    echo 'alias demo="cd /workspace && ./demo.sh"' >> /home/vscode/.bashrc && \
    echo 'alias verify-setup="echo \"🔍 Checking setup...\" && docker ps | grep neo4j > /dev/null && echo \"✅ Neo4j running\" || echo \"❌ Neo4j not running (run: docker-compose up -d neo4j)\" && dotnet --version > /dev/null && echo \"✅ .NET SDK installed\" && ls /workspace/Data/migration.db > /dev/null 2>&1 && echo \"✅ Database exists\" || echo \"⚠️  No migration data yet (run: ./demo.sh)\""' >> /home/vscode/.bashrc

# Environment variables
ENV ASPNETCORE_ENVIRONMENT=Development
ENV DOTNET_CLI_TELEMETRY_OPTOUT=1
ENV DOTNET_SKIP_FIRST_TIME_EXPERIENCE=1
ENV ASPNETCORE_URLS=http://localhost:5028
ENV MIGRATION_DB_PATH=/workspace/Data/migration.db

# Welcome message with status check
RUN echo 'echo ""' >> /home/vscode/.bashrc && \
    echo 'echo "╔══════════════════════════════════════════════════════════╗"' >> /home/vscode/.bashrc && \
    echo 'echo "║  🚀 COBOL Migration Dev Container Ready!              ║"' >> /home/vscode/.bashrc && \
    echo 'echo "╚══════════════════════════════════════════════════════════╝"' >> /home/vscode/.bashrc && \
    echo 'echo ""' >> /home/vscode/.bashrc && \
    echo 'echo "📊 Quick Commands:"' >> /home/vscode/.bashrc && \
    echo 'echo "  demo           - Start demo with existing data"' >> /home/vscode/.bashrc && \
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
