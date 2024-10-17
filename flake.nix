{
  description = "A self-hostable pastebin for your tailnet";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
  }:
    utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ] (system: let
      # graft = pkgs: pkg: pkg.override {
      #   buildGoModule = pkgs.buildGo123Module;
      # };
      #   pkgs = import nixpkgs {
      #     inherit system;
      # overlays = [ gomod2nix.overlays.default (final: prev: {
      #   go = prev.go_1_23;
      #   go-tools = graft prev prev.go-tools;
      #   gotools = graft prev prev.gotools;
      #   gopls = graft prev prev.gopls;
      # }) ];
      # };
      version = builtins.substring 0 8 self.lastModifiedDate;
      pkgs = import nixpkgs {inherit system;};
    in {
      packages = rec {
        tclipd = pkgs.buildGo123Module {
          pname = "tclipd";
          version = "0.1.0-${version}";
          inherit (pkgs) go;
          src = ./.;
          subPackages = "cmd/tclipd";
          vendorHash = "sha256-x7dgVkvhfOIdsjtJzMYcD2RmMO1FSpRDW+Tx7DPXyrI=";
        };

        tclip = pkgs.buildGo123Module {
          pname = "tclip";
          inherit (tclipd) src version vendorHash;
          subPackages = "cmd/tclip";
          go = pkgs.go;
          CGO_ENABLED = "0";
        };

        # docker = pkgs.dockerTools.buildLayeredImage {
        #   name = "ghcr.io/tailscale-dev/tclip";
        #   tag = "latest";
        #   config.Cmd = [ "${tclipd}/bin/tclipd" ];
        #   contents = [ pkgs.cacert ];
        # };

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

        # default = docker;
        default = tclipd;
      };

      apps.default =
        utils.lib.mkApp {drv = self.packages.${system}.default;};

      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          go_1_23
          gopls
          gotools
          go-tools
          sqlite-interactive

          yarn
          nodejs
        ];

        TSNET_HOSTNAME = "paste-devel";
      };
    })
    // {};
}
