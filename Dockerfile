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

# Install git (required by spago), spago, and purescript
RUN apt-get update && \
    apt-get install -y --no-install-recommends git && \
    rm -rf /var/lib/apt/lists/* && \
    npm install -g purescript@0.15.15 spago@0.93.40

# Copy package files for dependency installation
COPY frontend/package*.json /build/

# Install npm dependencies first
RUN npm install

# Copy spago configuration
COPY frontend/spago.yaml /build/
COPY frontend/spago.lock /build/

# Copy pre-built spago dependencies and output (to avoid network issues in Docker)
COPY frontend/.spago/ /build/.spago/
COPY frontend/output/ /build/output/

# Copy frontend source and build files
COPY frontend/src/ /build/src/
COPY frontend/public/ /build/public/
COPY frontend/index.js /build/
COPY frontend/build.sh /build/

# Build the frontend (dependencies already available from copied .spago and output)
RUN chmod +x build.sh && ./build.sh

# Stage 3: Runtime
FROM debian:bookworm-slim

WORKDIR /app

# Install runtime dependencies and locales
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libgmp10 \
    libtinfo6 \
    ca-certificates \
    locales && \
    echo "en_US.UTF-8 UTF-8" > /etc/locale.gen && \
    locale-gen && \
    rm -rf /var/lib/apt/lists/*

# Set UTF-8 locale environment
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

# Copy backend binary
COPY --from=backend-builder /build/bin/aralex-backend /app/backend/

# Copy frontend files
COPY --from=frontend-builder /build/dist/ /app/frontend/dist/
COPY --from=frontend-builder /build/public/ /app/frontend/public/

# Copy databases
COPY backend/aradicts.db /app/backend/
COPY backend/aralex.db /app/backend/

# Expose port
EXPOSE 8080

# Set working directory to backend (so it can find ../frontend)
WORKDIR /app/backend

# Run the backend
CMD ["./aralex-backend"]
