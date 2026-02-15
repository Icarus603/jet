#!/bin/bash
# One-line installer for Jet
# Usage: curl -sSL https://jet-lang.org/install.sh | bash

set -e

REPO="${JET_REPO:-Icarus603/jet}"
INSTALL_DIR="${INSTALL_DIR:-$HOME/.jet}"
BIN_DIR="$INSTALL_DIR/bin"

# Detect OS and architecture
detect_platform() {
    local _ostype
    local _cputype

    _ostype="$(uname -s)"
    _cputype="$(uname -m)"

    case "$_ostype" in
        Linux)
            _ostype=linux
            ;;
        Darwin)
            _ostype=macos
            ;;
        MINGW* | MSYS* | CYGWIN*)
            echo "Error: Windows is not supported in Jet 1.0. Use macOS or Linux."
            exit 1
            ;;
        *)
            echo "Error: unsupported OS type: $_ostype"
            exit 1
            ;;
    esac

    case "$_cputype" in
        x86_64 | x86-64 | x64 | amd64)
            _cputype=x64
            ;;
        arm64 | aarch64)
            _cputype=arm64
            ;;
        *)
            echo "Error: unsupported CPU type: $_cputype"
            exit 1
            ;;
    esac

    if [[ "$_ostype" == "linux" && "$_cputype" != "x64" ]]; then
        echo "Error: Linux releases currently support x64 only."
        exit 1
    fi

    echo "${_ostype}-${_cputype}"
}

sha256_file() {
    local path="$1"
    if command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "$path" | awk '{print $1}'
    elif command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$path" | awk '{print $1}'
    else
        echo "Error: neither shasum nor sha256sum is available for checksum verification."
        exit 1
    fi
}

# Get latest release version
get_latest_version() {
    curl -s "https://api.github.com/repos/$REPO/releases/latest" | \
        grep '"tag_name":' | \
        sed -E 's/.*"([^"]+)".*/\1/'
}

# Download and install
download_and_install() {
    local version=$1
    local platform=$2
    local archive_name="jet-${version}-${platform}.tar.gz"
    local download_url="https://github.com/${REPO}/releases/download/${version}/${archive_name}"

    echo "Downloading Jet ${version} for ${platform}..."

    # Create temp directory
    local temp_dir
    temp_dir=$(mktemp -d)
    trap 'rm -rf "$temp_dir"' EXIT

    # Download
    if ! curl -fsSL "$download_url" -o "${temp_dir}/${archive_name}"; then
        echo "Error: failed to download ${download_url}"
        exit 1
    fi

    # Verify checksum against release SHA256SUMS manifest
    local checksums_url="https://github.com/${REPO}/releases/download/${version}/SHA256SUMS.txt"
    echo "Verifying checksum..."
    if ! curl -fsSL "$checksums_url" -o "${temp_dir}/SHA256SUMS.txt"; then
        echo "Error: failed to download checksums from ${checksums_url}"
        exit 1
    fi
    local expected
    expected="$(awk -v f="${archive_name}" '$2 ~ ("/" f "$") || $2 == f { print $1 }' "${temp_dir}/SHA256SUMS.txt" | head -n1)"
    if [ -z "$expected" ]; then
        echo "Error: checksum entry for ${archive_name} not found in SHA256SUMS.txt"
        exit 1
    fi
    local actual
    actual="$(sha256_file "${temp_dir}/${archive_name}")"
    if [ "$expected" != "$actual" ]; then
        echo "Error: checksum mismatch for ${archive_name}"
        echo "Expected: ${expected}"
        echo "Actual:   ${actual}"
        exit 1
    fi

    # Extract
    echo "Extracting..."
    tar -xzf "${temp_dir}/${archive_name}" -C "$temp_dir"

    # Install
    echo "Installing to ${INSTALL_DIR}..."
    rm -rf "$INSTALL_DIR"
    mkdir -p "$INSTALL_DIR"
    cp -r "${temp_dir}/jet-${version}-${platform}"/* "$INSTALL_DIR/"

    # Create bin directory link if it doesn't exist
    mkdir -p "$BIN_DIR"

    echo ""
    echo "Jet ${version} has been installed to ${INSTALL_DIR}"
}

# Update shell configuration
update_shell_config() {
    local shell_config=""

    case "$SHELL" in
        */bash)
            shell_config="$HOME/.bashrc"
            ;;
        */zsh)
            shell_config="$HOME/.zshrc"
            ;;
        */fish)
            shell_config="$HOME/.config/fish/config.fish"
            ;;
    esac

    if [ -n "$shell_config" ]; then
        if ! grep -q "$BIN_DIR" "$shell_config" 2>/dev/null; then
            echo ""
            echo "Adding ${BIN_DIR} to PATH in ${shell_config}..."
            echo "export PATH=\"${BIN_DIR}:\$PATH\"" >> "$shell_config"
            echo "Please run: source ${shell_config}"
        fi
    fi

    echo ""
    echo "Add the following to your shell configuration file:"
    echo "  export PATH=\"${BIN_DIR}:\$PATH\""
}

# Main installation
main() {
    echo "Jet Language Installer"
    echo "======================"
    echo ""

    # Check for required tools
    if ! command -v curl &> /dev/null; then
        echo "Error: curl is required but not installed"
        exit 1
    fi

    # Detect platform
    local platform
    platform=$(detect_platform)
    echo "Detected platform: ${platform}"

    # Get version
    local version
    if [ -n "$1" ]; then
        version="$1"
    else
        echo "Fetching latest version..."
        version=$(get_latest_version)
        if [ -z "$version" ]; then
            echo "Error: could not determine latest version"
            exit 1
        fi
    fi

    echo "Version: ${version}"
    echo ""

    # Download and install
    download_and_install "$version" "$platform"

    # Update shell configuration
    update_shell_config

    # Verify installation
    echo ""
    echo "Verifying installation..."
    if "${BIN_DIR}/jet" --version; then
        echo ""
        echo "Installation successful!"
        echo "Run 'jet --help' to get started"
    else
        echo "Warning: could not verify installation"
    fi
}

main "$@"
