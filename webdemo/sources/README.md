# webdemo source slots

These three folders are intentionally empty. Drop your own COBOL programs and
the conversion artefacts the Convert workflow produces here:

```
webdemo/sources/
├── cobol/           # original COBOL programs (.cbl)
├── java/            # converted Java services
│   └── <prog>/<File>Service.java
└── dotnet/          # converted .NET services
    └── <prog>/<File>.cs
```

Then register each program in `webdemo/api/endpoints.json` (see the
`$schema` block for the shape) and restart `mock-server.py`. The portal
at `webdemo/index.html` will pick them up automatically.

**No customer source is bundled with this repository.**
