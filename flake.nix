{
  description = "filehub";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              filehub = hfinal.callPackage ./default.nix {};
            };
        };
        filehub = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.filehub;
      };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          nixosModules = rec {
            filehub = import ./nix/modules self;
            default = filehub;
          };

          devShells = rec {
            default = filehub-shell;
            filehub-shell = pkgs.callPackage ./shell.nix { hspkgs = hspkgs; };
          };

          packages = rec {
            default = filehub;
            filehub = pkgs.filehub;
          };
        };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
