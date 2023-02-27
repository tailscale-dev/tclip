{
  description = "A self-hostable pastebin for your tailnet";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";

    gomod2nix = {
      url = "github:tweag/gomod2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "utils";
    };
  };

  outputs = { self, nixpkgs, utils, gomod2nix }:
    utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ] (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ gomod2nix.overlays.default (final: prev: {
            go = prev.go_1_20;
          }) ];
        };
        version = builtins.substring 0 8 self.lastModifiedDate;
      in {
        packages = rec {
          web = pkgs.buildGoApplication {
            pname = "tclip-web";
            version = "0.1.0-${version}";
            src = ./.;
            subPackages = "cmd/web";
            modules = ./gomod2nix.toml;
          };

          tclip = pkgs.buildGoApplication {
            pname = "tclip";
            inherit (web) src version modules;
            subPackages = "cmd/tclip";

            CGO_ENABLED = "0";
          };

          docker = pkgs.dockerTools.buildLayeredImage {
            name = "ghcr.io/tailscale-dev/tclip";
            tag = "latest";
            config.Cmd = [ "${web}/bin/web" ];
            contents = [ pkgs.cacert ];
          };

          portable-service = let
            web-service = pkgs.substituteAll {
              name = "tclip.service";
              src = ./run/portable-service/tclip.service.in;
              inherit web;
            };
          in pkgs.portableService {
            inherit (web) version;
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
          ];

          TSNET_HOSTNAME = "paste-devel";
        };
      }) // {};
}
