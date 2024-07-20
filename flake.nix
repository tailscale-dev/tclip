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

  outputs = { self, nixpkgs, utils, gomod2nix }:
    utils.lib.eachDefaultSystem
      (system:
        let
          graft = pkgs: pkg: pkg.override {
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
        in
        {
          packages = rec {
            tclipd = pkgs.buildGoApplication {
              pname = "tclipd";
              version = "0.1.0-${version}";
              go = pkgs.go;
              src = ./.;
              subPackages = "cmd/tclipd";
              modules = ./gomod2nix.toml;
            };

            tclipd_arm64 = tclipd.overrideAttrs ({ GOOS = "linux"; GOARCH = "arm64"; CGO_ENABLED = 0; });
            tclipd_amd64 = tclipd.overrideAttrs ({ GOOS = "linux"; GOARCH = "amd64"; CGO_ENABLED = 0; });

            tclip = pkgs.buildGoApplication {
              pname = "tclip";
              inherit (tclipd) src version modules;
              subPackages = "cmd/tclip";
              go = pkgs.go;
              CGO_ENABLED = "0";
            };

            # Builds natively for your current OS/arch
            docker = pkgs.dockerTools.buildLayeredImage {
              name = "ghcr.io/tailscale-dev/tclip";
              tag = "latest";
              config.Cmd = [ "${tclipd}/bin/tclipd" ];
              contents = [ pkgs.cacert ];
            };

            # Always builds for amd64, used when building multi-arch :latest manifest
            docker_amd64 = docker.override {
              config.Cmd = [ "${tclipd_amd64}/bin/tclipd" ];
              tag = "amd64";
              architecture = "amd64";
            };

            # Always builds for arm64, used when building multi-arch :latest manifest
            docker_arm64 = docker.override {
              config.Cmd = [ "${tclipd_arm64}/bin/linux_arm64/tclipd" ];
              tag = "arm64";
              architecture = "arm64";
            };

            portable-service =
              let
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
                units = [ web-service ];
                symlinks = [{
                  object = "${pkgs.cacert}/etc/ssl";
                  symlink = "/etc/ssl";
                }];
              };

            default = docker;
          };

          apps.default =
            utils.lib.mkApp { drv = self.packages.${system}.default; };

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
        }) // { };
}
