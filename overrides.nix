# Specify the version of nixpkgs we want
let
  inherit (import <nixpkgs> { }) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    name = "nixos-unstable-2020-07-09";
    owner = "nixos";
    repo = "nixpkgs";
    rev = "8d05772134f17180fb2711d0660702dae2a67313";
    sha256 = "0pnyg26c1yhnp3ymzglc71pd9j0567ymqy6il5ywc82bbm1zy25a";
  };
  pkgs = import nixpkgs { inherit config; };
  compilerVersion = "ghc883";
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
                # keter = super.callCabal2nix "keter" (fetchFromGitHub {
                #   name = "keter-modernize";
                #   owner = "tolysz";
                #   repo = "keter";
                #   rev = "db6a104aa252f49f98ee79a1d88b3fb343f4f635";
                #   sha256 = "003ypqmmdbxz9k0ygmm4v0ij8mj8x93a6cjsc054rz6hrrvm488q";
                # }) { };
              };
            };
        };
      };
    };
    allowBroken = true;
    allowUnfree = true;
  };

in {
  compiler = pkgs.haskell.packages."${compilerVersion}";
  pkgs = pkgs;
}
