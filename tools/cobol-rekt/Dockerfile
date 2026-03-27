# Cobol-REKT sidecar container
# Builds smojol-cli.jar from source for COBOL parsing, AST/CFG/Data export.
# Pinned to v0.1.0-RC6 for stability.

FROM eclipse-temurin:21-jdk AS builder

RUN apt-get update && apt-get install -y git maven graphviz && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Clone rekt at pinned tag with submodules
RUN git clone --recurse-submodules --depth 1 --branch v0.1.0-RC6 \
    https://github.com/avishek-sen-gupta/cobol-rekt.git .

# Build (skip tests + checkstyle for faster image)
RUN mvn clean verify -Dmaven.test.skip=true -Dcheckstyle.skip=true -q

# ── Runtime stage ────────────────────────────────────────────────────
FROM eclipse-temurin:21-jre

RUN apt-get update && apt-get install -y graphviz && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy the built CLI JAR and dialect JAR
COPY --from=builder /build/smojol-cli/target/smojol-cli.jar /app/smojol-cli.jar
COPY --from=builder /build/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar /app/dialect-idms.jar

# Create source/output mount points
RUN mkdir -p /source /output

# Healthcheck: verify JAR is runnable
HEALTHCHECK --interval=30s --timeout=10s --retries=3 \
    CMD java -jar /app/smojol-cli.jar --version || exit 1

ENTRYPOINT ["tail", "-f", "/dev/null"]
