#!/usr/bin/env bash
set -euo pipefail

python3 - <<'PY'
import pathlib
import re
import sys

roots = ["runtime/src", "runtime/ffi/src", "stdlib/src", "compiler"]
pattern = re.compile(r"pub\s+unsafe\s+(extern\s+\"C\"\s+)?fn")
missing = []

for root in roots:
    p = pathlib.Path(root)
    if not p.exists():
        continue
    for f in p.rglob("*.rs"):
        lines = f.read_text(encoding="utf-8", errors="ignore").splitlines()
        for i, line in enumerate(lines):
            if not pattern.search(line):
                continue
            window = "\n".join(lines[max(0, i - 30):i])
            if "# Safety" not in window:
                missing.append((str(f), i + 1, line.strip()))

if missing:
    print("Missing `# Safety` docs for unsafe public APIs:")
    for path, line, sig in missing:
        print(f"  {path}:{line}: {sig}")
    sys.exit(1)

print("Safety docs check passed.")
PY
