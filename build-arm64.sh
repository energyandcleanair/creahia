#!/bin/bash

# Build script for ARM64 Docker image for local development
# This script builds the creahia package for ARM64 architecture

set -e

echo "Building creahia Docker image for ARM64 architecture..."

# Check if we're on an ARM64 machine
ARCH=$(uname -m)
if [[ "$ARCH" == "arm64" || "$ARCH" == "aarch64" ]]; then
    echo "✓ Running on ARM64 architecture ($ARCH)"
else
    echo "⚠️  Running on $ARCH architecture - image will still be built for ARM64"
fi

# Build the Docker image
docker build \
    --platform linux/arm64 \
    -f Dockerfile.ci.arm64 \
    -t creahia:arm64-latest \
    --progress=plain \
    .

echo "✓ Docker image 'creahia:arm64-latest' built successfully"

# Optional: Run a quick test to verify the build
echo "Running quick verification test..."
docker run --rm --platform linux/arm64 creahia:arm64-latest \
    Rscript -e "library(creahia); cat('✓ creahia package loaded successfully\n')"

echo "✓ Build verification complete"
echo ""
echo "Usage:"
echo "  docker run -it --rm --platform linux/arm64 creahia:arm64-latest /bin/bash"
echo "  docker run --rm --platform linux/arm64 -v \$(pwd):/work creahia:arm64-latest Rscript your_script.R"
