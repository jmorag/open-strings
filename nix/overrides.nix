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
                # yesod-auth-oauth2 =
                #   pkgs.haskell.lib.doJailbreak super.yesod-auth-oauth2;
                hoauth2 = super.callHackage "hoauth2" "1.8.9" { };
                keter = super.callCabal2nix "keter" (fetchFromGitHub {
                  name = "keter-modernize";
                  owner = "tolysz";
                  repo = "keter";
                  rev = "db6a104aa252f49f98ee79a1d88b3fb343f4f635";
                  sha256 =
                    "003ypqmmdbxz9k0ygmm4v0ij8mj8x93a6cjsc054rz6hrrvm488q";
                }) { };
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
