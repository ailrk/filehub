self: { config, lib, pkgs, ...}:
let
  cfg = config.services.filehub;
in
{
  options.services.filehub = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Enable filehub
      '';
    };

    root = lib.mkOption {
      type = lib.types.path;
      default = "";
      description = ''
        Root folde to serve.
      '';
    };

    port = lib.mkOption {
      type = lib.types.int;
      default = "5000";
      description = ''
        Port to serve
      '';
    };

    environment = lib.mkOption {
      type = lib.types.attrsOf (lib.types.either lib.types.str lib.types.path);
      default = {};
      description = ''
        Environment variables for filehub.
      '';
    };

    package = lib.mkOption {
      type = lib.types.package;
      default = self.packages.${pkgs.system}.default;
      description = ''
        The filehub package to use.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.filehub = {
      wantedBy = [  "multi-user.target" ];
      after = [ "network.target" ];
      description = "Start filehub.";
      environment = cfg.environment;
      unitConfig.DefaultDependencies = "no";
      serviceConfig = {
        Type = "simple";
        EnvironmentFile = cfg.secretFile;
        ExecStart = [
          "${cfg.package}/bin/filehub"
        ];
      };
    };
  };
}
