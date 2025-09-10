{
  description = "Local NixOS VM for the testing environment";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/8f3cf34b8d2e2caf4ae5ee1d1fddc1baab4c5964";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.filehub.url = "path:..";

  outputs = { self, nixpkgs, flake-utils, filehub }:
  flake-utils.lib.eachDefaultSystem (system:
  {
    nixosConfigurations.localvm = nixpkgs.lib.nixosSystem {
      inherit system;
      modules = [
        filehub.nixosModules.${system}.default

        ({ config, pkgs, lib, modulesPath, ... }: {

          imports = [ "${modulesPath}/virtualisation/qemu-vm.nix" ];

          # VM settings
          virtualisation = {
            graphics = true;
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
            qemu.options = [
              "-serial" "mon:stdio"   # Serial console stays in terminal
            ];
          };

          environment.systemPackages = with pkgs; [
            lsof
            htop
            tmux
            xterm               # optional terminal
          ];

          environment.etc = {
            "authelia-main/.jwt-secret".source             = ./authelia/.jwt-secret;
            "authelia-main/.session-secret".source         = ./authelia/.session-secret;
            "authelia-main/.storage-encryption-key".source = ./authelia/.storage-encryption-key;
            "authelia-main/users_database.yml".source      = ./authelia/users_database.yml;
            "authelia-main/config.yml".source              = ./authelia/config.yml;
            "filehub/config.toml".source                   = ./filehub/config.toml;
            "ssl/localhost.crt".source                     = ./ssl/localhost.crt;
            "ssl/localhost.key".source                     = ./ssl/localhost.key;
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

          services.xserver.enable = true;
          services.xserver.displayManager.startx.enable = true;
          services.xserver.videoDrivers = [ "dummy" ];  # no GPU required

          users.users.root = {
            password = "root";
          };

          networking.hostName = "local-vm";
          networking.useDHCP = true;

          # DNS resolver so VM can resolve hostnames
          networking.firewall.allowedTCPPorts = [ 53 80 443 9520 2223 ];
          networking.firewall.enable = true;

          security.pki.certificateFiles = [
            ./ssl/localhost.crt
          ];

          programs.firefox = {
            enable = true;
            wrapperConfig = {
              MOZILLA_CERTIFICATE_TRUST = "1";
            };
          };

          # Enable Nginx reverse proxy
          services.nginx.enable = true;

          services.nginx.virtualHosts."auth.home.local-vm" = {
            forceSSL = true;
            sslCertificate = "/etc/ssl/localhost.crt";
            sslCertificateKey = "/etc/ssl/localhost.key";
            locations."/" = {
              proxyPass = "http://localhost:9520";
              extraConfig = ''
                proxy_set_header Host $host;
                proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
                proxy_set_header X-Forwarded-Proto $scheme;
              '';
            };
          };

          services.nginx.virtualHosts."filehub.home.local-vm" = {
            forceSSL = true;
            sslCertificate = "/etc/ssl/localhost.crt";
            sslCertificateKey = "/etc/ssl/localhost.key";
            locations."/" = {
              proxyPass = "http://localhost:2223";
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

          services.filehub.test = {
            enable = true;
            fs = [];
            s3 = [];
            theme = "dark";
            configFile = "/etc/filehub/config.toml";
            port = 2223;
          };

          # extend the environment variable
          systemd.services."authelia-main".serviceConfig.Environment = lib.mkForce [ "X_AUTHELIA_CONFIG_FILTERS=template" ];

          services.dnsmasq = {
            enable = true;
            settings = {
              address = "/local-vm/127.0.0.1";
              "listen-address" = "0.0.0.0";
              server = [
                "10.0.2.3" # QEMU builtin DNS
              ];
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
