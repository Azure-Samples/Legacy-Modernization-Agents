# rekt-oss-mma

`rekt-oss-mma` is the COBOL parser image used by the `cobol-rekt` service in
`docker-compose.yml`.

The image build:

1. Clones [cobol-rekt](https://github.com/avishek-sen-gupta/cobol-rekt) at
   `v0.1.0-RC6`.
2. Applies the local fixes under [`patches/`](./patches/).
3. Compiles `smojol-cli.jar` and packages it with the IDMS dialect and Graphviz.

## Build

```bash
docker compose build cobol-rekt
```

The container stays running so the pipeline can invoke the parser with
`docker exec`. Source files are mounted at `/source` and parser output is
written to `/output`.

```bash
docker exec cobol-rekt java -jar /app/smojol-cli.jar --version
```

## Files

- [`patches/README.md`](./patches/README.md): purpose of each local patch.
- [`LICENSE`](./LICENSE): local patches and image packaging.
- [`LICENSE.cobol-rekt`](./LICENSE.cobol-rekt): upstream cobol-rekt license.
- [`NOTICE`](./NOTICE): third-party components and licenses.
