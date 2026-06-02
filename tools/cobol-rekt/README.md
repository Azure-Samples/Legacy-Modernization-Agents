# rekt-oss-mma

**Docker image**: `rekt-oss-mma:latest`
**Built by**: `tools/cobol-rekt/Dockerfile`
**Consumed by**: the `cobol-rekt` service in `docker-compose.yml`

This is the COBOL parsing sidecar used by `Azure-Samples/COBOL-Modernization-Agents`.
It packages the upstream **smojol** COBOL toolkit
([avishek-sen-gupta/cobol-rekt](https://github.com/avishek-sen-gupta/cobol-rekt),
tag `v0.1.0-RC6`) plus a small set of leniency patches that allow
enterprise-scale corpora to parse without aborting on benign upstream
diagnostics. **All credit for the underlying COBOL toolkit goes to
Avishek Sen Gupta and the cobol-rekt contributors** — see [`NOTICE`](./NOTICE).

## Layout

```
tools/cobol-rekt/
├── Dockerfile          # Multi-stage build: Maven build + slim JRE runtime
├── LICENSE             # MIT — covers the patches + image packaging
├── NOTICE              # Attribution to upstream smojol + bundled components
├── README.md           # ← you are here
└── patches/            # Locally applied unified diffs (see patches/README.md)
    ├── 0001-lenient-parse-pipeline.patch
    ├── 0002-null-safe-data-division.patch
    ├── 0003-null-safe-entry-name.patch
    ├── 0004-tolerate-unknown-class-condition.patch
    ├── 0005-skip-null-ast-children.patch
    ├── 0006-safe-data-spec.patch
    ├── 0007-tolerate-null-procedure-division.patch
    └── 0008-skip-null-flow-writers.patch
```

## Build

```bash
docker compose build cobol-rekt        # tags rekt-oss-mma:latest
# or, standalone:
docker build -t rekt-oss-mma:latest tools/cobol-rekt/
```

Build takes ~5–10 minutes the first time (Maven downloads its world).
The resulting runtime image is ~600 MB and contains:

- Eclipse Temurin 21 JRE
- `smojol-cli.jar`
- `dialect-idms.jar`
- Graphviz (for CFG rendering)

## Run / health-check

The image's `ENTRYPOINT` is `tail -f /dev/null` because the agents call
the JAR via `docker exec`:

```bash
docker exec cobol-rekt java -jar /app/smojol-cli.jar --version
```

The compose service mounts `./source` (read-only) and `./output/rekt`.

## Patches

See [`patches/README.md`](./patches/README.md) for a per-patch rationale.
Net effect on an enterprise corpus (FUENTES, ~65 programs): **deps-only
parses drop from 38/65 to 5–8/65**, recovering full AST/CFG/Data fidelity
for the remaining programs.

Patches are unified diffs against the upstream tree at tag `v0.1.0-RC6`,
applied with `git apply` during the Docker build. They are intended to
be upstreamed; until then this image keeps the patch set reproducible.

## License

The local packaging and patches are **MIT** — see [`LICENSE`](./LICENSE).
Bundled third-party components retain their own licenses; full attribution
in [`NOTICE`](./NOTICE).
