default:
  @just --list

build:
    cabal2nix . > default.nix
    nix build

buildjs:
    #!/usr/bin/env bash
    pushd js/
    just build
    popd
    rm -rf data/filehub
    mv -fT js/dist/ data/filehub

dev:
    just buildjs
    cabal build

repl:
    cabal repl


profile:
    cabal clean
    cabal build --enable-profiling --disable-shared
    # run with +RTS -hc -p -RTS to get heap dump and prof file


vis:
    hp2ps -c -M filehub.hp
    ps2pdf filehub.ps


clean:
    cabal clean
    rm -f filehub.hp
    rm -f filehub.ps
    rm -f filehub.pdf
    rm -f filehub.prof
    rm -f filehub.aux


watch:
  ghcid -c "cabal repl filehub" -T "Filehub.mainDev \"--config-file _cache/config.toml\"" -W


storybook:
  ghcid -c "cabal repl filehub_storybook filehub"  -T "Main.mainDev 3333" -W


test_integration:
  ghcid -c "cabal repl filehub_test_integration filehub"  -T "Main.main" -W


test_unit:
  ghcid -c "cabal repl filehub_test_unit filehub"  -T "Main.main" -W


release:
  #!/usr/bin/env bash
  set -euo pipefail

  # Configuration
  APP_NAME="filehub"
  DIST_DIR="release"
  SOURCE_BIN="result/bin/$APP_NAME"

  # Dynamic Architecture Detection
  OS=$(uname -s | tr '[:upper:]' '[:lower:]')
  ARCH=$(uname -m)

  # Standardize arch names
  PLATFORM="${OS}-${ARCH}"

  # Get version from cabal file or fallback to 'latest'
  VERSION=$(grep -m 1 "^version:" *.cabal | awk '{print $2}' || echo "latest")
  ASSET_NAME="${APP_NAME}-${VERSION}-${PLATFORM}"

  echo "Blocking for platform: $PLATFORM (Version: $VERSION)"

  # Cleanup and Setup
  rm -rf "$DIST_DIR"
  mkdir -p "$DIST_DIR"

  if [ ! -f "$SOURCE_BIN" ]; then
      echo "Error: Binary not found at $SOURCE_BIN. Did you run 'nix build'?"
      exit 1
  fi

  # Stamp with git-rev
  git rev-parse --short HEAD > "$DIST_DIR/git-rev"

  # Prepare Binary
  cp "$SOURCE_BIN" "$DIST_DIR/$APP_NAME"
  chmod +x "$DIST_DIR/$APP_NAME"

  # Create Archive
  echo "Creating tarball: ${ASSET_NAME}.tar.gz"

  # Build list of extra files if they exist
  EXTRA_FILES=""
  [ -f "LICENSE" ] && EXTRA_FILES+=" LICENSE"
  [ -f "README.md" ] && EXTRA_FILES+=" README.md"
  EXTRA_FILES+=" git-rev"

  for f in $EXTRA_FILES; do
    if [ -f "$f" ]; then
      cp "$f" "$DIST_DIR/"
      FILES_TO_PACK+=("$f")
    fi
  done

  tar -czvf "$DIST_DIR/${ASSET_NAME}.tar.gz" \
      -C "$DIST_DIR" "$APP_NAME" \
      $EXTRA_FILES

  # Generate Checksum
  (cd "$DIST_DIR" && sha256sum "${ASSET_NAME}.tar.gz" > "${ASSET_NAME}.tar.gz.sha256")

  echo "---------------------------------------"
  echo "Release ready in ./$DIST_DIR:"
  ls -lh "$DIST_DIR/${ASSET_NAME}.tar.gz"*
