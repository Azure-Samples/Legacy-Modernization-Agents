# Cobol-REKT sidecar container — rekt-oss-mma
# Builds smojol-cli.jar from source for COBOL parsing, AST/CFG/Data export.
# Pinned to v0.1.0-RC6 for stability. See ./NOTICE for full attribution
# to Avishek Sen Gupta and the cobol-rekt contributors — this image would
# not exist without their excellent upstream work.

FROM eclipse-temurin:21-jdk AS builder

RUN apt-get update && apt-get install -y git maven graphviz && rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Clone rekt at pinned tag with submodules
RUN git clone --recurse-submodules --depth 1 --branch v0.1.0-RC6 \
    https://github.com/avishek-sen-gupta/cobol-rekt.git .

# Apply local leniency patches (see ./patches/README.md for rationale)
COPY patches/ /build/patches/
RUN set -e; for p in /build/patches/*.patch; do \
        echo ">> Applying $p"; \
        git apply --verbose "$p"; \
    done

# Build (skip tests + checkstyle for faster image)
RUN mvn clean verify -Dmaven.test.skip=true -Dcheckstyle.skip=true -q

# ── Runtime stage ────────────────────────────────────────────────────
FROM eclipse-temurin:21-jre

RUN apt-get update && apt-get install -y graphviz && rm -rf /var/lib/apt/lists/*

WORKDIR /app

LABEL org.opencontainers.image.title="rekt-oss-mma"
LABEL org.opencontainers.image.description="COBOL parser (smojol) packaged with leniency patches for the Azure-Samples/COBOL-Modernization-Agents project."
LABEL org.opencontainers.image.source="https://github.com/Azure-Samples/COBOL-Modernization-Agents"
LABEL org.opencontainers.image.licenses="MIT"
LABEL org.opencontainers.image.authors="Microsoft Corporation; patches by COBOL-Modernization-Agents contributors. Upstream COBOL toolkit by Avishek Sen Gupta (https://github.com/avishek-sen-gupta/cobol-rekt) — see /app/NOTICE."

# Copy the built CLI JAR and dialect JAR
COPY --from=builder /build/smojol-cli/target/smojol-cli.jar /app/smojol-cli.jar
COPY --from=builder /build/che-che4z-lsp-for-cobol-integration/server/dialect-idms/target/dialect-idms.jar /app/dialect-idms.jar

# Create source/output mount points
RUN mkdir -p /source /output

# Ship the license + attribution inside the image so it travels with any
# pulled copy — required for MIT redistribution.
COPY LICENSE NOTICE README.md /app/

# Healthcheck: verify JAR is runnable
HEALTHCHECK --interval=30s --timeout=10s --retries=3 \
    CMD java -jar /app/smojol-cli.jar --version || exit 1

ENTRYPOINT ["tail", "-f", "/dev/null"]
