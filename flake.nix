{
  description = "filehub";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/8f3cf34b8d2e2caf4ae5ee1d1fddc1baab4c5964";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              filehub = (hfinal.callPackage ./default.nix {});
            };
        };

        filehub =
          with final.haskell.lib.compose;
          overrideCabal (drv: {
            disallowGhcReference = false;
            enableSeparateDataOutput = false;
            configureFlags = drv.configureFlags or [] ++ [
              "--ghc-options=-O2"
              "--ghc-options=-optl=-s"  # Strip via linker
              "--ghc-options=-split-sections"
              "--ghc-options=-fomit-interface-pragmas"
            ];
          }) (final.haskellPackages.filehub);
        };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [ overlay ];
            config.stripDebugInfo = true; # strip all debug infos
          };
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
