{
  description = "Host DNS server for local dev VMs";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";

  outputs = { self, nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      nixosConfigurations.dns-host = nixpkgs.lib.nixosSystem {
        inherit system;

        modules = [
          ({ config, pkgs, lib, ... }: {

            services.dnsmasq = {
              enable = true;

              # settings attribute set (not string)
              settings = {
                listen-address = "127.0.0.1";
                port = 53;
                bind-interfaces = true;
                no-resolv = true;

                # your dev domains
                address = [
                  "/auth.local/127.0.0.1"
                  "/filehub.local/127.0.0.1"
                ];
              };
            };

            networking.firewall.allowedUDPPorts = [ 53 ];
            networking.firewall.allowedTCPPorts = [ 53 ];

          })
        ];
      };
    };
}
