# Specify the version of nixpkgs we want
let
  inherit (import <nixpkgs> { }) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    name = "nixos-unstable-2020-07-13";
    owner = "nixos";
    repo = "nixpkgs";
    rev = "c87c474b17af792e7984ef4f058291f7ce06f594";
    sha256 = "1171bwg07dcaqgayacaqwk3gyq97hi261gr7a4pgbrkafqb5r3ds";
  };
  pkgs = import nixpkgs { inherit config; };
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compilerVersion}" =
            pkgs.haskell.packages."${compilerVersion}".override {
              overrides = self: super: {
                ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
                ghcWithPackages = self.ghc.withPackages;
                # shakespeare =
                #   super.callCabal2nix "shakespeare" ../yesodweb/shakespeare { };
                # yesod = super.callCabal2nix "yesod" ../yesodweb/yesod/yesod { };
                yesod-auth-oauth2 = super.callHackage "yesod-auth-oauth2" "0.6.1.2" { };
                hoauth2 = super.callHackage "hoauth2" "1.8.9" { };
                # depends on cryptonite instead of deprecated cipher-aes
                clientsession = super.callCabal2nix "clientsession"
                  (fetchFromGitHub {
                    name = "clientsession-cryptonite";
                    owner = "nwf";
                    repo = "clientsession";
                    rev = "58e3605875d2a033fb753bcc57f2d6936069d34b";
                    sha256 =
                      "01qx822fa1xl6q8c8grw5qqjcs21kl5ycxn2xd2fdnys3mqsqlpd";
                  }) { };
                sendgrid-v3 = pkgs.haskell.lib.dontCheck
                  (super.callCabal2nix "sendgrid-v3" (fetchFromGitHub {
                    name = "sendgrid-v3";
                    owner = "marcelbuesing";
                    repo = "sendgrid-v3";
                    rev = "22f55f95f1f6660cdb328e1bcde78691b045295b";
                    sha256 =
                      "0cg8zlhkk5xq3s8dwiqhjij82nsml0wwr1n1wavid3ld0j8q0x3x";
                  }) { });

              };
            };
        };
      };
    };
    allowBroken = true;
    allowUnfree = true;
  };
  compilerVersion = "ghc883";

  name = "fingerdb";
in {
  compiler = pkgs.haskell.packages."${compilerVersion}";
  inherit pkgs compilerVersion config nixpkgs name;
}
