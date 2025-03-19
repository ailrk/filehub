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
