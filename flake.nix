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
          overlays = [ gomod2nix.overlays.default ];
        };
        version = builtins.substring 0 8 self.lastModifiedDate;
      in {
        packages = rec {
          bin = pkgs.buildGoApplication {
            pname = "tailpaste";
            version = "0.1.0-dev";
            src = ./.;
            modules = ./gomod2nix.toml;
          };

          tailpaste = pkgs.runCommand "tailpaste" {} ''
            mkdir -p $out/bin
            ln -s ${bin}/bin/tailpaste $out/bin/tailpaste
          '';
          
          web = pkgs.runCommand "web" {} ''
            mkdir -p $out/bin
            ln -s ${bin}/bin/web $out/bin/web
          '';

          docker = pkgs.dockerTools.buildLayeredImage {
            name = "tailpaste";
            tag = "latest";
            config.Cmd = [ "${web}/bin/web" ];
            contents = [ pkgs.cacert bin ];

            copyToRoot = pkgs.buildEnv {
              name = "image-root";
              paths = [ pkgs.cacert bin ];
              pathsToLink = [ "/bin" ];
            };
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

            jo
            jq
          ];

          TSNET_HOSTNAME = "paste-devel";
        };
      });
}
