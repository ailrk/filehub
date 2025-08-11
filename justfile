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
