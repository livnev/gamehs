{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:
let
  inherit (nixpkgs) pkgs;
  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages (ps: with ps; [
          monad-par mtl parsec cabal-install hoogle list-extras pretty-terminal
        ]);
in
pkgs.stdenv.mkDerivation {
  name = "game";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
