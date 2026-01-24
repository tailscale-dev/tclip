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
        tclipd = pkgs.buildGo125Module {
          pname = "tclipd";
          version = "0.1.0-${version}";
          inherit (pkgs) go;
          src = ./.;
          subPackages = "cmd/tclipd";
          vendorHash = "sha256-HY9yvmkQnk7GSlP42vgQmhm7rTxB4XE7r9MBvgr3AVI=";
        };

        tclip = pkgs.buildGo125Module {
          pname = "tclip";
          inherit (tclipd) src version vendorHash;
          subPackages = "cmd/tclip";
          inherit (pkgs) go;
          env.CGO_ENABLED = "0";
        };

        portable-service = let
          web-service = pkgs.replaceVars ./run/portable-service/tclip.service.in {
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
          go_1_25
          gopls
          gotools
          go-tools
          sqlite-interactive
          yarn
          nodejs

          (pkgs.buildGo125Module rec {
            name = "mkctr";
            src = pkgs.fetchFromGitHub {
              owner = "tailscale";
              repo = "mkctr";
              rev = "ea857e3e500ba9eba656ade7b351fbf8cb4b7587";
              sha256 = "sha256-j1Ru+5PZGnZa70ussNUcJNfmKDpBXvMuHg4iTjsLCwk=";
            };

            vendorHash = "sha256-RTw80aWylnl3d9IbUFdewFoW7OheecFwGBsPBeWROkE=";
          })
        ];

        TSNET_HOSTNAME = "paste-devel";
      };
    })
    // {};
}
