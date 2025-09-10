{
  description = "Local NixOS VM for the testing environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/8f3cf34b8d2e2caf4ae5ee1d1fddc1baab4c5964";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
  flake-utils.lib.eachDefaultSystem (system:

  {
    nixosConfigurations.local = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        ({ config, pkgs, lib, modulesPath, ... }: {

          imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];

          # VM settings
          virtualisation = {
            graphics = false;
            host = { inherit pkgs; };
            memorySize = 2 * 1024;
            diskSize = 8 * 1024;
            forwardPorts = [
              { from = "host"; host.port = 53535; guest.port = 53; }  # DNS
              { from = "host"; host.port = 8443;  guest.port = 443; }
              { from = "host"; host.port = 8080;  guest.port = 80; }
              { from = "host"; host.port = 9520;  guest.port = 9520; }
              { from = "host"; host.port = 2223;  guest.port = 2223; }
            ];
          };

          services.getty.autologinUser = "root";

          services.openssh = lib.mkForce {
            enable = true;
            settings = {
              StrictModes = false;
              PermitRootLogin = "without-password";
              PasswordAuthentication = false;
            };
          };

          environment.systemPackages = with pkgs; [ lsof htop tmux ];

          environment.etc = {
            "authelia-main/.jwt-secret".source = ./authelia/.jwt-secret;
            "authelia-main/.session-secret".source = ./authelia/.session-secret;
            "authelia-main/.storage-encryption-key".source = ./authelia/.storage-encryption-key;
            "authelia-main/users_database.yml".source = ./authelia/users_database.yml;
            "authelia-main/config.yml".source = ./authelia/config.yml;
            "filehub/config.toml".source = ./filehub/config.toml;
            "ssl/localhost.crt".source = ./ssl/localhost.crt;
            "ssl/localhost.key".source = ./ssl/localhost.key;
          };

          users.users.root = {
            password = "root";
          };

          networking.hostName = "local";
          networking.useDHCP = true;

          # DNS resolver so VM can resolve hostnames
          networking.firewall.allowedTCPPorts = [ 53 80 443 9520 2223 ];
          networking.firewall.enable = true;

          # Enable Nginx reverse proxy
          services.nginx.enable = true;

          services.nginx.virtualHosts."auth.local" = {
            forceSSL = true;
            sslCertificate = "/etc/ssl/localhost.crt";
            sslCertificateKey = "/etc/ssl/localhost.key";
            locations."/" = {
              proxyPass = "http://localhost:9520";
              proxyWebsockets = true;
              extraConfig = ''
                proxy_set_header Host $host;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
              '';
            };
          };

          services.authelia = {
            instances.main = {
              # user of all config files has to be 'authelia'
              enable = true;
              secrets = {
                jwtSecretFile            = "/etc/authelia-main/.jwt-secret";
                storageEncryptionKeyFile = "/etc/authelia-main/.storage-encryption-key";
                sessionSecretFile        = "/etc/authelia-main/.session-secret";
              };
              settingsFiles = [
                "/etc/authelia-main/config.yml"
              ];
            };
          };

          # extend the environment variable
          systemd.services."authelia-main".serviceConfig.Environment = lib.mkForce [ "X_AUTHELIA_CONFIG_FILTERS=template" ];

          services.dnsmasq = {
            enable = true;
            settings = {
              address = "/local/127.0.0.1";
              "listen-address" = "0.0.0.0";
              port = 53;
              "bind-interfaces" = true;
              "no-resolv" = true;
            };
          };
        })
      ];
    };
  });
}
