#!/usr/bin/env bash
# Production build script for Aralex frontend
# Builds PureScript and bundles for browser deployment

set -e

echo "🔨 Building Aralex Frontend..."
echo ""

# Step 1: Compile PureScript to output/
echo "📦 Step 1/2: Compiling PureScript..."
spago build

# Step 2: Bundle with Parcel (uses index.js entry point that calls main())
echo "📦 Step 2/2: Bundling with Parcel..."
npx parcel build index.js --dist-dir dist --public-url /

echo ""
echo "✅ Build complete!"
echo "   Output: frontend/dist/index.js"
echo "   Size: $(du -h dist/index.js | cut -f1)"
echo ""
echo "Backend will serve from: frontend/dist/"
