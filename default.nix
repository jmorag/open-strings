let
  inherit (import <nixpkgs> { }) fetchFromGitHub;
  nixpkgs = fetchFromGitHub {
    name = "nixos-unstable-2020-05-29";
    owner = "nixos";
    repo = "nixpkgs";
    rev = "19aac2413ae2810a47850f165a592cbe0d06f744";
    sha256 = "0m0h02avy888zmkrys9azs6sqx10my3gxi3f0m70fhsyc6shpc78";
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

              };
            };
        };
      };
    };
  };
  compiler = pkgs.haskell.packages."${compilerVersion}";
  pkg = compiler.developPackage {
    root = ./.;
    source-overrides = { };
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv
      (with pkgs.haskellPackages; [ cabal-install hoogle ghcid yesod-bin ]);
  };
  buildInputs = [ pkgs.libpqxx ];
in pkg.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ buildInputs; })
