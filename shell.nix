{ pkgs, hspkgs }:
hspkgs.shellFor {
  withHoogle = true;
  packages = p: [ p.filehub ];
  buildInputs = [
    hspkgs.cabal-install
    hspkgs.haskell-language-server
    hspkgs.hlint
    hspkgs.cabal2nix
    hspkgs.ghcprofview
    pkgs.ghciwatch
    pkgs.bashInteractive
  ];
}
