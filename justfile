build:
    cabal2nix . > default.nix
    nix build

repl:
    cabal repl
