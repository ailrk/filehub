# Stage 1: Build Stage
FROM nixos/nix:2.8.1 as build

# Set work directory
WORKDIR /build

# Copy the Nix configuration
COPY . /build

# Build the project with Nix (Ensure your Nix expression is set up to build the Haskell project)
RUN nix build .#default -o result

# Stage 2: Final Stage (Runtime Image)
FROM alpine:latest

# Install runtime dependencies
RUN apk add --no-cache \
    libcurl \
    zlib \
    ca-certificates \
    bash \
    tzdata

# Copy the built binary from the build stage
COPY --from=build /build/result/bin/filehub /usr/local/bin/filehub

# Expose the port that filehub will listen on
EXPOSE 5000

# Set the entrypoint to the filehub binary
ENTRYPOINT ["/usr/local/bin/filehub"]
