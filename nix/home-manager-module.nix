{ lib, pkgs, config, ... }:

with lib;

let
  cfg = config.programs.foo-bar;


in
{
  options =
    {
      programs.foo-bar =
        {
          enable = mkEnableOption "Foo/Bar cli and syncing";
          extraConfig =
            mkOption {
              type = types.str;
              description = "Extra contents for the config file";
              default = "";
            };
          sync =
            mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options =
                      {
                        enable = mkEnableOption "Foo/Bar syncing";
                        server-url =
                          mkOption {
                            type = types.str;
                            example = "api.foo-bar.cs-syd.eu";
                            description = "The url of the sync server";
                          };
                        username =
                          mkOption {
                            type = types.str;
                            example = "syd";
                            description =
                              "The username to use when logging into the sync server";
                          };
                        password =
                          mkOption {
                            type = types.str;
                            example = "hunter12";
                            description =
                              "The password to use when logging into the sync server";
                          };
                      };
                  }
                );
            };
        };
    };
  config =
    let
      fooBarPkgs = (import ./pkgs.nix).fooBarPackages;
      configContents = cfg: ''
        
${cfg.extraConfig}

      '';
      syncConfigContents = syncCfg:
        optionalString (syncCfg.enable or false) ''

server-url: "${cfg.sync.server-url}"
username: "${cfg.sync.username}"
password: "${cfg.sync.password}"

      '';


      syncFooBarName = "sync-foo-bar";
      syncFooBarService =
        {
          Unit =
            {
              Description = "Sync foo-bar";
              Wants = [ "network-online.target" ];
            };
          Service =
            {
              ExecStart =
                "${pkgs.writeShellScript "sync-foo-bar-service-ExecStart"
                  ''
                    exec ${fooBarPkgs.foo-bar-cli}/bin/foo-bar sync
                  ''}";
              Type = "oneshot";
            };
        };
      syncFooBarTimer =
        {
          Unit =
            {
              Description = "Sync foo-bar every day";
            };
          Install =
            {
              WantedBy = [ "timers.target" ];
            };
          Timer =
            {
              OnCalendar = "daily";
              Persistent = true;
              Unit = "${syncFooBarName}.service";
            };
        };

      fooBarConfigContents =
        concatStringsSep "\n" [
          (configContents cfg)
          (syncConfigContents cfg.sync)
        ];

      services =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncFooBarName}" = syncFooBarService;
          }
        );
      timers =
        (
          optionalAttrs (cfg.sync.enable or false) {
            "${syncFooBarName}" = syncFooBarTimer;
          }
        );
      packages =
        [
          fooBarPkgs.foo-bar-cli
        ];


    in
      mkIf cfg.enable {
        xdg = {
          configFile."foo-bar/config.yaml".text = fooBarConfigContents;
        };
        systemd.user =
          {
            startServices = true;
            services = services;
            timers = timers;
          };
        home.packages = packages;
      };
}
