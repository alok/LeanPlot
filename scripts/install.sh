#!/usr/bin/env bash
# LeanPlot installation script
# Usage: ./scripts/install.sh

set -euo pipefail

echo "=== LeanPlot Setup ==="

# Check for elan
if ! command -v elan &> /dev/null; then
    echo "Installing elan (Lean toolchain manager)..."
    curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- -y --default-toolchain none
    export PATH="$HOME/.elan/bin:$PATH"
else
    echo "✓ elan already installed"
fi

# Ensure elan is in PATH
if [[ ":$PATH:" != *":$HOME/.elan/bin:"* ]]; then
    export PATH="$HOME/.elan/bin:$PATH"
fi

# Get dependencies and build
echo "Fetching dependencies..."
lake update

echo "Building LeanPlot..."
lake build

echo ""
echo "=== Setup Complete ==="
echo "Run 'just' to see available commands"

