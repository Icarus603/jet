#!/bin/bash
# Build script for Jet release binaries
# Usage: ./scripts/build-release.sh [version]

set -e

VERSION=${1:-$(git describe --tags --always)}
TARGET=${2:-native}

echo "Building Jet release ${VERSION} for ${TARGET}"

# Detect OS
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    PLATFORM="linux"
    ARCH=$(uname -m)
    export LLVM_SYS_211_PREFIX=${LLVM_SYS_211_PREFIX:-/usr/lib/llvm-21}
elif [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="macos"
    ARCH=$(uname -m)
    if [ "$ARCH" = "arm64" ]; then
        export LLVM_SYS_211_PREFIX=${LLVM_SYS_211_PREFIX:-/opt/homebrew/opt/llvm@21}
    else
        export LLVM_SYS_211_PREFIX=${LLVM_SYS_211_PREFIX:-/usr/local/opt/llvm@21}
    fi
else
    echo "Unsupported platform: $OSTYPE"
    exit 1
fi

echo "Platform: ${PLATFORM}-${ARCH}"
echo "LLVM prefix: ${LLVM_SYS_211_PREFIX}"

# Clean previous builds
cargo clean

# Build release binary
echo "Building release binary..."
cargo build --workspace --release

# Create distribution directory
DIST_DIR="jet-${VERSION}-${PLATFORM}-${ARCH}"
rm -rf "${DIST_DIR}"
mkdir -p "${DIST_DIR}/bin"
mkdir -p "${DIST_DIR}/share/jet/examples"

# Copy binaries
cp target/release/jet "${DIST_DIR}/bin/"

# Copy examples
cp -r examples/* "${DIST_DIR}/share/jet/examples/"

# Copy documentation
cp README.md "${DIST_DIR}/"
cp LICENSE "${DIST_DIR}/"
cp -r docs "${DIST_DIR}/share/jet/"

# Create archive
if [ "$PLATFORM" = "linux" ]; then
    tar czvf "${DIST_DIR}.tar.gz" "${DIST_DIR}"
    echo "Created ${DIST_DIR}.tar.gz"
else
    tar czvf "${DIST_DIR}.tar.gz" "${DIST_DIR}"
    echo "Created ${DIST_DIR}.tar.gz"
fi

# Clean up
rm -rf "${DIST_DIR}"

echo "Release build complete!"
echo "Artifact: ${DIST_DIR}.tar.gz"
