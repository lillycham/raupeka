let
  pkgs = import <nixpkgs> { };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell-language-server
    zlib
    ghc
    cabal-install
  ];
}
