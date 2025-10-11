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
    hspkgs.eventlog2html
    pkgs.ghcid
    pkgs.ghciwatch
    pkgs.bashInteractive
    pkgs.upx
    pkgs.nodePackages.typescript

    # for tests
    pkgs.tree
  ];

  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.bzip2}/lib:$LD_LIBRARY_PATH"
  '';
}
