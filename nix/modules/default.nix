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
          type = lib.types.nullOr (lib.types.either lib.types.str lib.types.path);
          default = null;
          description = ''
            Path to the environment variable file. This file will be used on the systemd service.

            S3 environment:
            Filehub accepts the following AWS credentials:
              AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_ENDPOINT_URL, AWS_DEFAULT_REGION.
          '';
        };

        configFile = lib.mkOption {
          type = lib.types.nullOr lib.types.path;
          default = null;
          description = ''
            Path to the main toml config file.
          '';
        };

        extraConfigs = lib.mkOption {
          type = lib.types.listOf lib.types.path;
          default = [];
          description = ''
            Additional toml config files to load after configFile.
            Later files override scalar fields; list fields (targets, plugins, etc.) concatenate.
            Useful for loading plugin config fragments.
          '';
        };

        plugins = lib.mkOption {
          default = [];
          description = ''
            Declarative plugin definitions. Each plugin can add context menu items and
            viewer overrides for specific MIME types. Generates a toml config fragment
            passed to filehub automatically.
          '';
          type = lib.types.listOf (lib.types.submodule {
            options = {
              name = lib.mkOption {
                type = lib.types.str;
                description = "Plugin name.";
              };
              contextMenu = lib.mkOption {
                default = [];
                description = ''
                  Context menu entries added for files whose MIME type starts with `mime`.
                  The `url` field supports template variables:
                    {file_path}    — URL-encoded client path
                    {file_name}    — URL-encoded filename
                    {mime_type}    — URL-encoded MIME type
                    {download_url} — absolute authenticated download URL (local apps only)
                    {share_url}    — absolute public share link URL (prompts to create if needed)
                '';
                type = lib.types.listOf (lib.types.submodule {
                  options = {
                    mime  = lib.mkOption { type = lib.types.str; description = "MIME prefix to match (e.g. \"image\", \"application/pdf\")."; };
                    label = lib.mkOption { type = lib.types.str; description = "Menu item label shown to the user."; };
                    url   = lib.mkOption { type = lib.types.str; description = "URL template to open."; };
                  };
                });
              };
              viewer = lib.mkOption {
                default = [];
                description = ''
                  Viewer overrides: when the user clicks a matching file, open this URL
                  instead of the built-in viewer. Does not support {share_url} (use
                  contextMenu for that). Supports {file_path}, {file_name}, {mime_type}.
                '';
                type = lib.types.listOf (lib.types.submodule {
                  options = {
                    mime = lib.mkOption { type = lib.types.str; description = "MIME prefix to match."; };
                    url  = lib.mkOption { type = lib.types.str; description = "URL template to open."; };
                  };
                });
              };
            };
          });
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
        mkPluginsToml = plugins:
          let
            escape = s: lib.replaceStrings [ "\\" "\"" ] [ "\\\\" "\\\"" ] s;
            mkContextMenuEntry = e: ''

              [[plugin.context_menu]]
              mime  = "${escape e.mime}"
              label = "${escape e.label}"
              url   = "${escape e.url}"
            '';
            mkViewerEntry = e: ''

              [[plugin.viewer]]
              mime = "${escape e.mime}"
              url  = "${escape e.url}"
            '';
            mkPlugin = p: ''
              [[plugin]]
              name = "${escape p.name}"
            '' + lib.concatMapStrings mkContextMenuEntry p.contextMenu
               + lib.concatMapStrings mkViewerEntry p.viewer;
          in
            pkgs.writeText "filehub-plugins.toml"
              (lib.concatMapStrings mkPlugin plugins);

        mkService = name: cfg:
        let
          serviceName = "filehub-" + name;
          port = builtins.toString cfg.port;
          pluginsToml = mkPluginsToml cfg.plugins;
          configArgs =
               (if cfg.configFile != null then [ "--config=${cfg.configFile}" ] else [])
            ++ (map (f: "--config=${f}") cfg.extraConfigs)
            ++ (if cfg.plugins != [] then [ "--config=${pluginsToml}" ] else []);
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
                EnvironmentFile = if cfg.environment == null then "" else cfg.environment;
                ExecStart = lib.escapeShellArgs ([
                  "${cfg.package}/bin/filehub"
                  "--port" port
                  "--theme" cfg.theme
                ]
                ++ (if cfg.readonly then [ "--readonly" ] else [])
                ++ configArgs
                ++ (map (p: "--fs=${toString p}") cfg.fs)
                ++ (map (s: "--s3=${s}") cfg.s3)
                );
              };
            };
      in
      lib.mapAttrs' mkService cfgs ;
  };
}
