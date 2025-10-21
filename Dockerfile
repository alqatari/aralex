# Multi-stage build for Aralex application
# Stage 1: Build Haskell backend
FROM haskell:latest AS backend-builder

WORKDIR /build

# Copy backend source
COPY backend/ /build/

# Install dependencies and build
RUN cabal update && \
    cabal build --only-dependencies && \
    cabal build && \
    cabal install --install-method=copy --installdir=/build/bin

# Stage 2: Build PureScript frontend
FROM node:20-slim AS frontend-builder

WORKDIR /build

# Install spago and purescript
RUN npm install -g purescript@0.15.15 spago@0.93.40

# Copy frontend source
COPY frontend/package*.json /build/
COPY frontend/spago.yaml /build/
COPY frontend/spago.lock /build/
COPY frontend/src/ /build/src/
COPY frontend/public/ /build/public/

# Install dependencies and build
RUN npm install && \
    spago build && \
    spago bundle --outfile dist/bundle.js --platform browser

# Stage 3: Runtime
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libgmp10 \
    libtinfo6 \
    ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Copy backend binary
COPY --from=backend-builder /build/bin/aralex-backend /app/

# Copy frontend files
COPY --from=frontend-builder /build/dist/ /app/frontend/dist/
COPY --from=frontend-builder /build/public/ /app/frontend/public/

# Copy databases
COPY backend/aradicts.db /app/
COPY backend/aralex.db /app/

# Expose port
EXPOSE 8181

# Run the backend
CMD ["/app/aralex-backend"]
