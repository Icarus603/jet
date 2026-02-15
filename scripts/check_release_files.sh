#!/usr/bin/env bash
set -euo pipefail

required_files=(
  "README.md"
  "LICENSE"
  "CHANGELOG.md"
  "SECURITY.md"
)

missing=0
for file in "${required_files[@]}"; do
  if [[ ! -s "${file}" ]]; then
    echo "missing or empty required release file: ${file}" >&2
    missing=1
  fi
done

if [[ "${missing}" -ne 0 ]]; then
  exit 1
fi

echo "Release file check passed."
