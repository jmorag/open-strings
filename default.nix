# Adapted from
# https://github.com/PostgREST/postgrest/blob/bd2160db26210d41821825925b7149bf30566069/nix/static-haskell-package.nix
with (import ./nix/overrides.nix);
let
  applyPatches = src: patches:
    pkgs.runCommand "apply patches" { inherit src patches; } ''
      cp -r $src $out
      chmod -R +w $out
      for p in $patches; do
        echo "Applying patch $p";
        patch -d $out -p1 < "$p";
      done
    '';

  # The nh2/static-haskell-nix project does all the hard work for us.
  static-haskell-nix = ./nix/static-haskell.nix;

  patches = [ ./patches/nixpkgs-revert-ghc-bootstrap.patch ];

  normalPkgs = import (applyPatches nixpkgs patches) { inherit config; };

  # The static-haskell-nix 'survey' derives a full static set of Haskell
  # packages, applying fixes where necessary.
  survey = import static-haskell-nix {
    compiler = compilerVersion;
    inherit normalPkgs;
  };
  lib = pkgs.haskell.lib;
in {
  fingerdb =
    lib.justStaticExecutables (lib.dontCheck survey.haskellPackages."${name}");
}
