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
      version = builtins.substring 0 8 self.lastModifiedDate;
      pkgs = import nixpkgs {inherit system;};
    in {
      packages = rec {
        tclipd = pkgs.buildGo124Module {
          pname = "tclipd";
          version = "0.1.0-${version}";
          inherit (pkgs) go;
          src = ./.;
          subPackages = "cmd/tclipd";
          vendorHash = "sha256-4QWTlzsGoKDA02+IC6fTX/zQOG84Wb5l3+xWsVgNY2k=";
        };

        tclip = pkgs.buildGo124Module {
          pname = "tclip";
          inherit (tclipd) src version vendorHash;
          subPackages = "cmd/tclip";
          inherit (pkgs) go;
          CGO_ENABLED = "0";
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

          (pkgs.buildGo124Module rec {
            name = "mkctr";
            src = pkgs.fetchFromGitHub {
              owner = "tailscale";
              repo = "mkctr";
              rev = "42e5cb39d30bc804bd9a0071095cbd5de78e54f8";
              sha256 = "sha256-MN47+aiJXqzAir3hhCKgY7OAys/ZLFi3OKkwH/wgFco=";
            };

            vendorHash = "sha256-nIoe79dZwrFqrYLVfqASQDDjG1x0GmZpxDpnEdfny8k=";
          })
        ];

        TSNET_HOSTNAME = "paste-devel";
      };
    })
    // {};
}
