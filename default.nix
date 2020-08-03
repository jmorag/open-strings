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
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
        cabal-install
        hoogle
        ghcid
        yesod-bin
        ghcide
        implicit-hie
        hlint
        ormolu
        dhall
        dhall-lsp-server
      ]);
  };
  buildInputs = [ pkgs.libpqxx ];
in pkg.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ buildInputs; })
