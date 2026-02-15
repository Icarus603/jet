#!/bin/bash
# Test script for Jet examples

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
JET_CLI="$REPO_ROOT/target/release/jet"
EXAMPLES_DIR="$REPO_ROOT/examples"
TEMP_DIR="/tmp/jet_example_tests"
RESULTS_FILE="/tmp/jet_example_test_results.txt"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "Testing Jet Examples" > "$RESULTS_FILE"
echo "===================" >> "$RESULTS_FILE"
echo "" >> "$RESULTS_FILE"

# Test function for standalone .jet files
test_standalone_example() {
    local file="$1"
    local basename=$(basename "$file" .jet)
    local test_dir="$TEMP_DIR/$basename"

    echo -e "${YELLOW}Testing: $file${NC}"

    # Create temporary project
    rm -rf "$test_dir"
    mkdir -p "$test_dir/src"

    # Copy example as main.jet
    cp "$file" "$test_dir/src/main.jet"

    # Create jet.toml
    cat > "$test_dir/jet.toml" <<EOF
[package]
name = "$basename"
version = "1.0.0"
edition = "2024"

[dependencies]
EOF

    # Run check
    cd "$test_dir"
    if $JET_CLI check 2>&1 | tee -a "$RESULTS_FILE"; then
        echo -e "${GREEN}✓ $basename - CHECK PASSED${NC}"
        echo "✓ $basename - CHECK PASSED" >> "$RESULTS_FILE"
        cd - > /dev/null
        return 0
    else
        echo -e "${RED}✗ $basename - CHECK FAILED${NC}"
        echo "✗ $basename - CHECK FAILED" >> "$RESULTS_FILE"
        cd - > /dev/null
        return 1
    fi
}

# Create temp directory
mkdir -p "$TEMP_DIR"

if [ ! -x "$JET_CLI" ]; then
    echo "Jet CLI not found or not executable: $JET_CLI" >&2
    echo "Build it first: cargo build -p jet-cli --release" >&2
    exit 1
fi

# Test all standalone examples
PASSED=0
FAILED=0

for example in "$EXAMPLES_DIR"/*.jet; do
    if [ -f "$example" ]; then
        if test_standalone_example "$example"; then
            ((PASSED++))
        else
            ((FAILED++))
        fi
        echo "" >> "$RESULTS_FILE"
    fi
done

# Test tutorial examples
for example in "$EXAMPLES_DIR"/tutorial/*.jet; do
    if [ -f "$example" ]; then
        if test_standalone_example "$example"; then
            ((PASSED++))
        else
            ((FAILED++))
        fi
        echo "" >> "$RESULTS_FILE"
    fi
done

# Test test_jet_project
echo -e "${YELLOW}Testing: test_jet_project${NC}"
cd "$EXAMPLES_DIR/test_jet_project"
if $JET_CLI check 2>&1 | tee -a "$RESULTS_FILE"; then
    echo -e "${GREEN}✓ test_jet_project - CHECK PASSED${NC}"
    echo "✓ test_jet_project - CHECK PASSED" >> "$RESULTS_FILE"
    ((PASSED++))
else
    echo -e "${RED}✗ test_jet_project - CHECK FAILED${NC}"
    echo "✗ test_jet_project - CHECK FAILED" >> "$RESULTS_FILE"
    ((FAILED++))
fi
cd - > /dev/null

# Print summary
echo ""
echo "==================="
echo "Summary"
echo "==================="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"
echo ""
echo "Full results saved to: $RESULTS_FILE"

# Cleanup
# rm -rf "$TEMP_DIR"

exit $FAILED
