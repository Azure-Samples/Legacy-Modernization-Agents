#!/usr/bin/env python3
"""
Mock REST server for the COBOL → Java/.NET conversion demo portal.

Serves:
  GET  /                              -> webdemo/index.html (and the rest of webdemo/)
  GET  /api/endpoints                 -> webdemo/api/endpoints.json catalog
  POST /api/kyghg003/inquiry          -> returns the Consulta de Límites canned response,
                                          merged with whatever the caller sent so the
                                          demo shows the round-trip.
  POST /api/t660a410/process          -> same, for Räntehantering Service.

This is intentionally a *mock*: the converted Java/.NET classes are still
plain DI beans (no JAX-RS / ASP.NET routes were emitted by the converter
for these programs). The mock returns the response example from
endpoints.json so the demo portal has something to display while you
develop the real adapters.

Run:
    cd webdemo && python3 mock-server.py 8848
Then open http://localhost:8848/ in a browser.
"""

from __future__ import annotations
import http.server
import json
import os
import socketserver
import sys
import urllib.parse
from datetime import datetime, timezone

ROOT = os.path.dirname(os.path.abspath(__file__))
CATALOG_PATH = os.path.join(ROOT, "api", "endpoints.json")


def load_catalog() -> dict:
    with open(CATALOG_PATH, "r", encoding="utf-8") as fh:
        return json.load(fh)


class DemoHandler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *args, **kwargs):
        # Force the static-file root to webdemo/, regardless of where the
        # script was launched from.
        super().__init__(*args, directory=ROOT, **kwargs)

    # --- helpers ---------------------------------------------------------
    def _write_json(self, payload: dict, status: int = 200) -> None:
        body = json.dumps(payload, indent=2, ensure_ascii=False).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.send_header("Access-Control-Allow-Origin", "*")
        self.end_headers()
        self.wfile.write(body)

    def _read_json_body(self) -> dict:
        length = int(self.headers.get("Content-Length", "0"))
        if not length:
            return {}
        raw = self.rfile.read(length)
        try:
            return json.loads(raw.decode("utf-8"))
        except json.JSONDecodeError:
            return {"_warning": "request body was not valid JSON", "_raw": raw.decode("utf-8", "replace")}

    def _find_endpoint(self, method: str, path: str):
        catalog = load_catalog()
        for prog in catalog.get("programs", []):
            ep = prog.get("restEndpoint", {})
            if ep.get("method") == method and ep.get("path") == path:
                return prog, ep
        return None, None

    # --- request handlers -----------------------------------------------
    def do_OPTIONS(self):
        self.send_response(204)
        self.send_header("Access-Control-Allow-Origin", "*")
        self.send_header("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        self.send_header("Access-Control-Allow-Headers", "Content-Type")
        self.end_headers()

    def do_GET(self):
        parsed = urllib.parse.urlparse(self.path)
        if parsed.path == "/api/endpoints":
            self._write_json(load_catalog())
            return
        # Default: serve static files from webdemo/ (handled by parent).
        super().do_GET()

    def do_POST(self):
        parsed = urllib.parse.urlparse(self.path)
        prog, ep = self._find_endpoint("POST", parsed.path)
        if not prog:
            self._write_json({"error": f"Unknown endpoint: POST {parsed.path}"}, 404)
            return
        body = self._read_json_body()
        canned = ep.get("responseExample", {})
        merged = {
            "endpoint": ep.get("path"),
            "summary": ep.get("summary"),
            "program": prog.get("id"),
            "language": prog.get("language"),
            "frameworks": {
                "java":   prog.get("java", {}).get("framework"),
                "dotnet": prog.get("dotnet", {}).get("framework"),
            },
            "request":  body,
            "response": canned,
            "serverNote": (
                "MOCK response. The converted Java/.NET classes are plain DI "
                "services without HTTP routes — this mock returns the example "
                "payload from endpoints.json so the demo portal has something "
                "live to display. Add a JAX-RS / [HttpPost] wrapper in the "
                "converted projects to make the real services hand-off."
            ),
            "servedAt": datetime.now(timezone.utc).isoformat(timespec="seconds"),
        }
        self._write_json(merged)


def main():
    port = 8848
    if len(sys.argv) > 1:
        try:
            port = int(sys.argv[1])
        except ValueError:
            print(f"WARNING: ignoring non-numeric port arg {sys.argv[1]!r}", file=sys.stderr)
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer(("127.0.0.1", port), DemoHandler) as httpd:
        print(f"COBOL→Java/.NET demo portal serving on http://127.0.0.1:{port}/")
        print(f"  Catalog: http://127.0.0.1:{port}/api/endpoints")
        try:
            httpd.serve_forever()
        except KeyboardInterrupt:
            pass


if __name__ == "__main__":
    main()
