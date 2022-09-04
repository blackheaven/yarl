{
  description = "yarl";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        github = owner: repo: rev: sha256:
          builtins.fetchTarball { inherit sha256; url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz"; };

        sources = { };

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (pkgs.haskell.lib.unmarkBroken pkg));

        haskellPackages = pkgs.haskell.packages.ghc924.override {
          overrides = hself: hsuper: { };
        };
      in
      rec
      {
        packages.yarl = # (ref:haskell-package-def)
          haskellPackages.callCabal2nixWithOptions "yarl" ./. "-fbenchmark" rec {
            # Dependency overrides go here
          };

        defaultPackage = packages.yarl;

        devShell =
          let
            scripts = pkgs.symlinkJoin {
              name = "scripts";
              paths = pkgs.lib.mapAttrsToList pkgs.writeShellScriptBin {
                ormolu = ''
                  ${pkgs.ormolu}/bin/ormolu -o -XDataKinds -o -XDefaultSignatures -o -XDeriveAnyClass -o -XDeriveGeneric -o -XDerivingStrategies -o -XDerivingVia -o -XDuplicateRecordFields -o -XFlexibleContexts -o -XGADTs -o -XGeneralizedNewtypeDeriving -o -XKindSignatures -o -XLambdaCase -o -XNoImplicitPrelude -o -XOverloadedLists -o -XOverloadedStrings -o -XRankNTypes -o -XRecordWildCards -o -XScopedTypeVariables -o -XTypeApplications -o -XTypeFamilies -o -XTypeOperators -o -XNoImportQualifiedPost -o -XOverloadedRecordDot $@
                '';
              };
            };
          in
          pkgs.mkShell {
            buildInputs = with haskellPackages; [
              haskell-language-server
              ghcid
              cabal-install
              haskell-ci
              scripts
            ];
            inputsFrom = [
              self.defaultPackage.${system}.env
            ];
          };
      });
}
