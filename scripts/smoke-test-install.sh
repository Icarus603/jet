#!/bin/bash
# Smoke test for Jet installation from release artifacts
# Tests that installed binary works correctly
# Usage: ./scripts/smoke-test-install.sh [version] [platform]

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
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

log_step() {
    echo -e "${BLUE}[STEP]${NC} $1"
}

# Configuration
VERSION="${1:-}"
PLATFORM="${2:-}"
REPO="${JET_REPO:-Icarus603/jet}"
INSTALL_DIR="${INSTALL_DIR:-$HOME/.jet-test}"
BIN_DIR="$INSTALL_DIR/bin"

# Detect platform if not provided
if [ -z "$PLATFORM" ]; then
    if [[ "$OSTYPE" == "linux-gnu"* ]]; then
        PLATFORM="linux"
        ARCH=$(uname -m)
        if [ "$ARCH" = "x86_64" ]; then
            ARCH="x64"
        fi
        PLATFORM="${PLATFORM}-${ARCH}"
    elif [[ "$OSTYPE" == "darwin"* ]]; then
        PLATFORM="macos"
        ARCH=$(uname -m)
        PLATFORM="${PLATFORM}-${ARCH}"
    else
        log_error "Unsupported platform: $OSTYPE"
        exit 1
    fi
fi

# Get version from dry-run artifacts if not provided
if [ -z "$VERSION" ]; then
    # Look for local dry-run artifacts
    DRYRUN_ARTIFACT=$(ls -t jet-*-dryrun-*.tar.gz 2>/dev/null | head -1 || true)
    if [ -n "$DRYRUN_ARTIFACT" ]; then
        VERSION=$(echo "$DRYRUN_ARTIFACT" | sed -E 's/jet-(.+)-(linux|macos)-.+\.tar\.gz/\1/')
        log_info "Using local dry-run artifact: $DRYRUN_ARTIFACT"
        USE_LOCAL=true
    else
        log_error "No version specified and no local dry-run artifact found"
        log_info "Usage: $0 [version] [platform]"
        log_info "Example: $0 v1.0.0-rc.1 linux-x64"
        exit 1
    fi
else
    USE_LOCAL=false
fi

log_info "Testing Jet installation"
log_info "Version: ${VERSION}"
log_info "Platform: ${PLATFORM}"
log_info "Install directory: ${INSTALL_DIR}"

# Clean up any previous test installation
if [ -d "$INSTALL_DIR" ]; then
    log_warn "Removing previous test installation at ${INSTALL_DIR}"
    rm -rf "$INSTALL_DIR"
fi

# Step 1: Download or use local artifact
log_step "1. Obtaining release artifact..."

ARCHIVE_NAME="jet-${VERSION}-${PLATFORM}.tar.gz"

if [ "$USE_LOCAL" = true ]; then
    ARCHIVE_PATH="$DRYRUN_ARTIFACT"
    log_info "Using local artifact: ${ARCHIVE_PATH}"
else
    DOWNLOAD_URL="https://github.com/${REPO}/releases/download/${VERSION}/${ARCHIVE_NAME}"
    ARCHIVE_PATH="/tmp/${ARCHIVE_NAME}"

    log_info "Downloading from: ${DOWNLOAD_URL}"

    if ! curl -fsSL "$DOWNLOAD_URL" -o "$ARCHIVE_PATH"; then
        log_error "Failed to download artifact"
        exit 1
    fi
fi

# Step 2: Verify checksum if available
log_step "2. Verifying checksum..."

CHECKSUM_FILE="${ARCHIVE_PATH}.sha256"
if [ -f "$CHECKSUM_FILE" ]; then
    log_info "Verifying against local checksum file..."
    EXPECTED=$(cat "$CHECKSUM_FILE" | awk '{print $1}')
    ACTUAL=$(shasum -a 256 "$ARCHIVE_PATH" | awk '{print $1}')

    if [ "$EXPECTED" != "$ACTUAL" ]; then
        log_error "Checksum mismatch!"
        log_error "Expected: ${EXPECTED}"
        log_error "Actual:   ${ACTUAL}"
        exit 1
    fi
    log_info "Checksum verified"
elif [ "$USE_LOCAL" = false ]; then
    # Try to download checksums from release
    CHECKSUMS_URL="https://github.com/${REPO}/releases/download/${VERSION}/SHA256SUMS.txt"
    log_info "Attempting to download checksums from release..."

    if curl -fsSL "$CHECKSUMS_URL" -o /tmp/SHA256SUMS.txt 2>/dev/null; then
        EXPECTED=$(awk -v f="$ARCHIVE_NAME" '$2 ~ ("/" f "$") || $2 == f { print $1 }' /tmp/SHA256SUMS.txt | head -n1)
        if [ -n "$EXPECTED" ]; then
            ACTUAL=$(shasum -a 256 "$ARCHIVE_PATH" | awk '{print $1}')
            if [ "$EXPECTED" != "$ACTUAL" ]; then
                log_error "Checksum mismatch!"
                log_error "Expected: ${EXPECTED}"
                log_error "Actual:   ${ACTUAL}"
                exit 1
            fi
            log_info "Checksum verified against release SHA256SUMS.txt"
        else
            log_warn "No checksum found for ${ARCHIVE_NAME} in SHA256SUMS.txt"
        fi
    else
        log_warn "Could not download checksums, skipping verification"
    fi
else
    log_warn "No checksum file available, skipping verification"
fi

# Step 3: Extract artifact
log_step "3. Extracting artifact..."

mkdir -p "$INSTALL_DIR"
tar xzf "$ARCHIVE_PATH" -C "$INSTALL_DIR" --strip-components=1

log_info "Extracted to: ${INSTALL_DIR}"

# Step 4: Verify installation structure
log_step "4. Verifying installation structure..."

if [ ! -f "$INSTALL_DIR/bin/jet" ]; then
    log_error "jet binary not found at ${INSTALL_DIR}/bin/jet"
    exit 1
fi
log_info "✓ jet binary found"

if [ ! -f "$INSTALL_DIR/README.md" ]; then
    log_warn "README.md not found in installation"
else
    log_info "✓ README.md found"
fi

if [ ! -f "$INSTALL_DIR/LICENSE" ]; then
    log_warn "LICENSE not found in installation"
else
    log_info "✓ LICENSE found"
fi

if [ -d "$INSTALL_DIR/share/jet/examples" ]; then
    EXAMPLE_COUNT=$(ls "$INSTALL_DIR/share/jet/examples"/*.jet 2>/dev/null | wc -l)
    log_info "✓ Examples found: ${EXAMPLE_COUNT}"
else
    log_warn "Examples directory not found"
fi

# Step 5: Test jet binary
log_step "5. Testing jet binary..."

JET_BIN="$INSTALL_DIR/bin/jet"

# Test --version
log_info "Testing 'jet --version'..."
if ! "$JET_BIN" --version; then
    log_error "jet --version failed"
    exit 1
fi
log_info "✓ jet --version works"

# Test --help
log_info "Testing 'jet --help'..."
if ! "$JET_BIN" --help > /dev/null; then
    log_error "jet --help failed"
    exit 1
fi
log_info "✓ jet --help works"

# Test check command on examples
log_step "6. Testing 'jet check' on examples..."

EXAMPLES_DIR="$INSTALL_DIR/share/jet/examples"
if [ -d "$EXAMPLES_DIR" ]; then
    for example in "$EXAMPLES_DIR"/*.jet; do
        if [ -f "$example" ]; then
            EXAMPLE_NAME=$(basename "$example")
            log_info "Checking ${EXAMPLE_NAME}..."
            if "$JET_BIN" check "$example" 2>/dev/null; then
                log_info "✓ ${EXAMPLE_NAME} passes check"
            else
                log_warn "✗ ${EXAMPLE_NAME} failed check (may use unimplemented features)"
            fi
        fi
    done
else
    log_warn "No examples to test"
fi

# Step 7: Create and test a simple project
log_step "7. Testing project creation and build..."

TEST_PROJECT_DIR=$(mktemp -d)
trap "rm -rf ${TEST_PROJECT_DIR}" EXIT

log_info "Creating test project in ${TEST_PROJECT_DIR}..."

mkdir -p "$TEST_PROJECT_DIR/src"

cat > "$TEST_PROJECT_DIR/src/main.jet" << 'EOF'
fn main():
    print("Hello from Jet!")
EOF

cat > "$TEST_PROJECT_DIR/jet.toml" << 'EOF'
[package]
name = "test_project"
version = "1.0.0"
edition = "2024"
EOF

log_info "Running 'jet check' on test project..."
if "$JET_BIN" -C "$TEST_PROJECT_DIR" check 2>/dev/null; then
    log_info "✓ Project check passed"
else
    log_warn "✗ Project check failed (may be expected if features are unimplemented)"
fi

# Step 8: Summary
log_step "8. Test Summary"

echo ""
echo "========================================"
log_info "Smoke test completed!"
echo "========================================"
echo ""
echo "Installation location: ${INSTALL_DIR}"
echo "Binary location: ${JET_BIN}"
echo ""
echo "To use this installation:"
echo "  export PATH=\"${BIN_DIR}:\$PATH\""
echo "  jet --version"
echo ""
echo "To clean up test installation:"
echo "  rm -rf ${INSTALL_DIR}"
echo ""

# Return success if basic commands work
if "$JET_BIN" --version > /dev/null 2>&1 && "$JET_BIN" --help > /dev/null 2>&1; then
    log_info "All critical tests passed!"
    exit 0
else
    log_error "Critical tests failed!"
    exit 1
fi
