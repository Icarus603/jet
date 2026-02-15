#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 3 ]]; then
  echo "Usage: $0 <version-tag> <sha256> <output-path>"
  echo "Example: $0 v1.0.0 abcdef... packaging/homebrew/jet.rb"
  exit 1
fi

VERSION_TAG="$1"
SOURCE_SHA256="$2"
OUTPUT_PATH="$3"

if [[ ! "$VERSION_TAG" =~ ^v[0-9]+\.[0-9]+\.[0-9]+([.-][A-Za-z0-9]+)*$ ]]; then
  echo "Error: version tag must look like vX.Y.Z (got: $VERSION_TAG)"
  exit 1
fi

if [[ ! "$SOURCE_SHA256" =~ ^[0-9a-f]{64}$ ]]; then
  echo "Error: sha256 must be 64 lowercase hex characters"
  exit 1
fi

cat > "$OUTPUT_PATH" <<EOF
class Jet < Formula
  desc "Statically-typed systems programming language with Python-like syntax"
  homepage "https://github.com/Icarus603/jet"
  url "https://github.com/Icarus603/jet/archive/refs/tags/${VERSION_TAG}.tar.gz"
  sha256 "${SOURCE_SHA256}"
  license "MIT"
  head "https://github.com/Icarus603/jet.git", branch: "main"

  depends_on "llvm@21" => :build
  depends_on "rust" => :build

  def install
    llvm = Formula["llvm@21"]
    ENV["LLVM_SYS_211_PREFIX"] = llvm.opt_prefix.to_s

    system "cargo", "build", "--release", "--package", "jet-cli"
    bin.install "target/release/jet"
    pkgshare.install "examples" if Dir.exist?("examples")
  end

  test do
    assert_match "The Jet programming language", shell_output("#{bin}/jet --help")
  end
end
EOF

echo "Wrote Homebrew formula to ${OUTPUT_PATH}"
