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

        fs = lib.mkOption {
          type = lib.types.listOf lib.types.path;
          default = [];
          description = ''
            List of local file system paths to serve.
          '';
        };

        s3 = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [];
          description = ''
            List of S3 buckets to serve.
          '';
        };

        port = lib.mkOption {
          type = lib.types.int;
          default = 5000;
          description = ''
            Port to serve
          '';
        };

        readonly = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = ''
            Enable read-only mode
          '';
        };


        theme = lib.mkOption {
          type = lib.types.str;
          default = "dark";
          description = ''
            Filehub theme. Possible themes: [dark, light]
          '';
        };

        environment = lib.mkOption {
          type = lib.types.either lib.types.str lib.types.path;
          description = ''
            Path to the environment variable file. This file will be used on the systemd service.

            S3 environment:
            Filehub accepts the following AWS credentials:
              AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_ENDPOINT_URL, AWS_DEFAULT_REGION.
          '';
        };

        package = lib.mkOption {
          type = lib.types.package;
          default = self.packages.${pkgs.system}.default;
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
              path = [ pkgs.getent ];
              serviceConfig = {
                Type = "simple";
                EnvironmentFile = cfg.environment;
                ExecStart = lib.escapeShellArgs ([
                  "${cfg.package}/bin/filehub"
                  "--port" port
                  "--theme" cfg.theme
                ]
                ++ (if cfg.readonly then [ "--readonly" ] else [])
                ++ (map (p: "--fs=${toString p}") cfg.fs)
                ++ (map (s: "--s3=${s}") cfg.s3)
                );
              };
            };
      in
      lib.mapAttrs' mkService cfgs ;
  };
}
