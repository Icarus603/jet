#!/bin/bash
# Release dry-run script for Jet
# Simulates the release process locally without publishing
# Usage: ./scripts/release-dry-run.sh [version]

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Get version from argument or generate one
if [ -n "${1:-}" ]; then
    VERSION="$1"
else
    # Generate a dry-run version based on git describe
    if git describe --tags --always >/dev/null 2>&1; then
        VERSION="$(git describe --tags --always)-dryrun"
    else
        VERSION="v0.0.0-dryrun"
    fi
fi

log_info "Starting release dry-run for version: ${VERSION}"

# Detect platform
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    PLATFORM="linux"
    ARCH=$(uname -m)
    if [ "$ARCH" = "x86_64" ]; then
        ARCH="x64"
    fi
    export LLVM_SYS_211_PREFIX=${LLVM_SYS_211_PREFIX:-/usr/lib/llvm-21}
elif [[ "$OSTYPE" == "darwin"* ]]; then
    PLATFORM="macos"
    ARCH=$(uname -m)
    if [ "$ARCH" = "arm64" ]; then
        export LLVM_SYS_211_PREFIX=${LLVM_SYS_211_PREFIX:-/opt/homebrew/opt/llvm@21}
    else
        ARCH="x64"
        export LLVM_SYS_211_PREFIX=${LLVM_SYS_211_PREFIX:-/usr/local/opt/llvm@21}
    fi
else
    log_error "Unsupported platform: $OSTYPE"
    exit 1
fi

log_info "Detected platform: ${PLATFORM}-${ARCH}"
log_info "LLVM prefix: ${LLVM_SYS_211_PREFIX}"

# Step 1: Run quality gates
echo ""
log_info "Step 1: Running quality gates..."

log_info "Checking formatting..."
if ! cargo fmt --all -- --check; then
    log_error "Formatting check failed. Run 'cargo fmt --all' to fix."
    exit 1
fi

log_info "Running clippy..."
if ! cargo clippy --workspace --all-targets -- -D warnings; then
    log_error "Clippy check failed. Fix warnings before release."
    exit 1
fi

log_info "Checking safety docs..."
if ! bash scripts/check_safety_docs.sh; then
    log_error "Safety docs check failed."
    exit 1
fi

log_info "Checking release files..."
if ! bash scripts/check_release_files.sh; then
    log_error "Release files check failed."
    exit 1
fi

log_info "Running tests..."
if ! cargo test --workspace; then
    log_error "Tests failed."
    exit 1
fi

# Step 2: Build release binary
echo ""
log_info "Step 2: Building release binary..."

cargo clean
cargo build --workspace --release

# Step 3: Create distribution directory
echo ""
log_info "Step 3: Creating distribution..."

DIST_NAME="jet-${VERSION}-${PLATFORM}-${ARCH}"
DIST_DIR="$(pwd)/${DIST_NAME}"
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

# Step 4: Create archive
echo ""
log_info "Step 4: Creating archive..."

if [ "$PLATFORM" = "linux" ]; then
    tar czvf "${DIST_NAME}.tar.gz" "${DIST_NAME}"
    ARCHIVE="${DIST_NAME}.tar.gz"
else
    tar czvf "${DIST_NAME}.tar.gz" "${DIST_NAME}"
    ARCHIVE="${DIST_NAME}.tar.gz"
fi

log_info "Created archive: ${ARCHIVE}"

# Step 5: Generate checksum
echo ""
log_info "Step 5: Generating checksum..."

if command -v shasum >/dev/null 2>&1; then
    shasum -a 256 "${ARCHIVE}" > "${ARCHIVE}.sha256"
else
    sha256sum "${ARCHIVE}" > "${ARCHIVE}.sha256"
fi

log_info "Checksum: $(cat "${ARCHIVE}.sha256")"

# Step 6: Smoke test
echo ""
log_info "Step 6: Running smoke tests..."

TMP_DIR=$(mktemp -d)
trap "rm -rf ${TMP_DIR}" EXIT

tar xzf "${ARCHIVE}" -C "${TMP_DIR}"

# Test jet --version
log_info "Testing 'jet --version'..."
if ! "${TMP_DIR}/${DIST_NAME}/bin/jet" --version; then
    log_error "jet --version failed"
    exit 1
fi

# Test jet --help
log_info "Testing 'jet --help'..."
if ! "${TMP_DIR}/${DIST_NAME}/bin/jet" --help >/dev/null; then
    log_error "jet --help failed"
    exit 1
fi

# Test jet check on an example
log_info "Testing 'jet check' on hello.jet example..."
if [ -f "${TMP_DIR}/${DIST_NAME}/share/jet/examples/hello.jet" ]; then
    if ! "${TMP_DIR}/${DIST_NAME}/bin/jet" check "${TMP_DIR}/${DIST_NAME}/share/jet/examples/hello.jet"; then
        log_warn "jet check on example failed (may be expected if example uses unimplemented features)"
    fi
else
    log_warn "hello.jet example not found in distribution"
fi

# Step 7: Verify archive contents
echo ""
log_info "Step 7: Verifying archive contents..."

echo "Archive contents:"
tar tzf "${ARCHIVE}" | head -20

echo ""
log_info "Archive size: $(du -h "${ARCHIVE}" | cut -f1)"

# Step 8: Summary
echo ""
echo "========================================"
log_info "Release dry-run completed successfully!"
echo "========================================"
echo ""
echo "Artifacts:"
echo "  Archive: ${ARCHIVE}"
echo "  Checksum: ${ARCHIVE}.sha256"
echo ""
echo "To test installation locally:"
echo "  ./scripts/smoke-test-install.sh ${VERSION} ${PLATFORM}-${ARCH}"
echo ""
echo "To clean up:"
echo "  rm -rf ${DIST_NAME} ${ARCHIVE} ${ARCHIVE}.sha256"
echo ""
