{
  description = "A self-hostable pastebin for your tailnet";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";

    gomod2nix = {
      url = "github:tweag/gomod2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "utils";
    };
  };

  outputs = {
    self,
    nixpkgs,
    utils,
    gomod2nix,
  }:
    {
      overlays.default = final: prev: {
        tclip = self.packages."${prev.system}".tclip;
        tclipd = self.packages."${prev.system}".tclipd;
      };

      nixosModules.tclip = {
        config,
        lib,
        ...
      }:
        with lib; let
          cfg = config.services.tclip;
        in {
          options.services.tclip = {
            enable = mkEnableOption "Enable tclip service";

            package = mkOption {
              type = types.package;
              description = ''
                tclip package to use
              '';
              default = self.packages."${system}".tclipd;
            };

            dataDir = mkOption {
              type = types.path;
              default = "/var/lib/tclip";
              description = "Path to data dir";
            };

            hostname = mkOption {
              type = types.str;
              default = "paste";
              description = "Hostname to use on your tailnet";
            };

            funnel = mkOption {
              type = types.bool;
              default = false;
              description = "if set, expose individual pastes to the public internet with Funnel";
            };

            user = mkOption {
              type = types.str;
              default = "tclip";
              description = "User account under which tclip runs.";
            };

            group = mkOption {
              type = types.str;
              default = "tclip";
              description = "Group account under which tclip runs.";
            };

            tailscaleAuthKeyFile = mkOption {
              type = types.path;
              description = "Path to file containing the Tailscale Auth Key";
            };

            verbose = mkOption {
              type = types.bool;
              default = false;
            };
          };
          config = mkIf cfg.enable {
            environment.systemPackages = [
              self.packages."${system}".tclip
            ];

            users.users."${cfg.user}" = {
              home = cfg.dataDir;
              createHome = true;
              group = "${cfg.group}";
              isSystemUser = true;
              isNormalUser = false;
              description = "User for tclip service";
            };
            users.groups."${cfg.group}" = {};

            systemd.services.tclip = {
              enable = true;
              script = let
                args =
                  [
                    "--data-dir"
                    cfg.dataDir
                    "--hostname"
                    cfg.hostname
                  ]
                  ++ lib.optionals cfg.verbose ["--tsnet-verbose"]
                  ++ lib.optionals cfg.funnel ["--use-funnel"];
              in ''
                ${lib.optionalString (cfg.tailscaleAuthKeyFile != null) ''
                  export TS_AUTHKEY="$(head -n1 ${lib.escapeShellArg cfg.tailscaleAuthKeyFile})"
                ''}
                ${cfg.package}/bin/tclipd ${builtins.concatStringsSep " " args};
              '';
              wantedBy = ["multi-user.target"];
              serviceConfig = {
                User = cfg.user;
                Group = cfg.group;
                Restart = "always";
                RestartSec = "15";
                WorkingDirectory = "${cfg.dataDir}";
              };
            };
          };
        };

      nixosModules.default = self.nixosModules.tclip;
    }
    // utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ] (system: let
      graft = pkgs: pkg:
        pkg.override {
          buildGoModule = pkgs.buildGo122Module;
        };
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          gomod2nix.overlays.default
          (final: prev: {
            go = prev.go;
            go-tools = graft prev prev.go-tools;
            gotools = graft prev prev.gotools;
            gopls = graft prev prev.gopls;
          })
        ];
      };
      version = builtins.substring 0 8 self.lastModifiedDate;
    in {
      packages = rec {
        tclipd = pkgs.buildGoApplication {
          pname = "tclipd";
          version = "0.1.0-${version}";
          go = pkgs.go;
          src = ./.;
          subPackages = "cmd/tclipd";
          modules = ./gomod2nix.toml;
        };

        tclip = pkgs.buildGoApplication {
          pname = "tclip";
          inherit (tclipd) src version modules;
          subPackages = "cmd/tclip";
          go = pkgs.go;

          CGO_ENABLED = "0";
        };

        docker = pkgs.dockerTools.buildLayeredImage {
          name = "ghcr.io/tailscale-dev/tclip";
          tag = "latest";
          config.Cmd = ["${tclipd}/bin/tclipd"];
          contents = [pkgs.cacert];
        };

        portable-service = let
          web-service = pkgs.substituteAll {
            name = "tclip.service";
            src = ./run/portable-service/tclip.service.in;
            inherit tclipd;
          };
        in
          pkgs.portableService {
            inherit (tclipd) version;
            pname = "tclip";
            description = "The tclip service";
            homepage = "https://github.com/tailscale-dev/tclip";
            units = [web-service];
            symlinks = [
              {
                object = "${pkgs.cacert}/etc/ssl";
                symlink = "/etc/ssl";
              }
            ];
          };

        default = docker;
      };

      apps.default =
        utils.lib.mkApp {drv = self.packages.${system}.default;};

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          go
          gopls
          gotools
          go-tools
          gomod2nix.packages.${system}.default
          sqlite-interactive

          yarn
          nodejs
        ];

        TSNET_HOSTNAME = "paste-devel";
      };
    })
    // {};
}
