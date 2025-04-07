self: { config, lib, pkgs, ...}:
let
  cfgs = config.services.filehub;
in
{
  options.services.filehub =
  let submodule = { config, ... }:
    {
      options = {
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
          default = 5000;
          description = ''
            Port to serve
          '';
        };

        theme = lib.mkOption {
          type = lib.types.str;
          default = "dark1";
          description = ''
            Filehub theme. Possible themes are [dark1, light1]
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
    };
  in
    lib.mkOption {
      description = "Configure filehub instances.";
      default = {};
      type = lib.types.attrsOf (lib.types.submodule submodule);
    };


  config = lib.mkIf (lib.any (cfg: cfg.enable) (lib.attrValues cfgs)) {
    systemd.services =
      let
        mkService = name: cfg:
        let
          serviceName = "filehub-" + name;
          port = builtins.toString cfg.port;
        in
          lib.nameValuePair
            serviceName
            {
              wantedBy = [ "multi-user.target" ];
              after = [ "network.target" ];
              description = "Start ${serviceName}";
              unitConfig.DefaultDependencies = "no";
              path = [ pkgs.getent ];
              serviceConfig = {
                Type = "simple";
                ExecStart = ''
                  "${cfg.package}/bin/filehub --port ${port} --root ${cfg.root} --theme ${cfg.theme}"
                '';
              };
            };
      in
      lib.mapAttrs' mkService cfgs ;
  };
}
