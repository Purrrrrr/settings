#!/bin/bash

set -e

PACKAGES_URL="https://raw.githubusercontent.com/solita/npm-malwares/refs/heads/master/shai-hulud-the-second-coming_packages.txt"
PACKAGES_URL="https://raw.githubusercontent.com/solita/npm-malwares/refs/heads/master/qix-packages.txt"
PACKAGES=$(curl -fsSL "$PACKAGES_URL" | tr '\n' ' ')
PACKAGE_COUNT=$(echo "$PACKAGES" | wc -w | tr -d ' ')

echo "Checking npm cache for $PACKAGE_COUNT vulnerable packages..."

CACHE_OUTPUT=$(npm cache ls $PACKAGES 2>&1)

echo "$CACHE_OUTPUT"

if [ -n "$CACHE_OUTPUT" ]; then
    echo ""
    echo "⚠️  WARNING: Vulnerable packages found in npm cache!"
else
    echo ""
    echo "✓ No vulnerable packages found in npm cache. This computer might be clean."
fi
