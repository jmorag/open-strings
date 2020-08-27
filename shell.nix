with (import ./nix/overrides.nix);
let
  pkg = compiler.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
        cabal2nix
        cabal-install
        dhall
        dhall-lsp-server
        ghcid
        haskell-language-server
        hlint
        hoogle
        implicit-hie
        ormolu
        stack
        yesod-bin
      ]);
  };
  buildInputs = with pkgs; [ ngrok ];
in pkg.overrideAttrs
(attrs: { buildInputs = attrs.buildInputs ++ buildInputs; })
