#!/usr/bin/env bash
# Hot reload development for PureScript frontend
# Uses spago-watch (nodemon-based) for file watching

set -e

echo "🔥 Starting PureScript frontend with hot reload..."
echo "   Watching for changes in src/"
echo "   Auto-rebuilding on code changes"
echo ""

# Install nodemon if not present
if ! command -v nodemon &> /dev/null; then
    echo "Installing nodemon for file watching..."
    npm install -g nodemon
fi

# Watch for changes and rebuild
nodemon --watch src --ext purs --exec "npx spago build"
