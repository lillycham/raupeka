let
  pkgs = import <nixpkgs> { };
  # Workaround for https://github.com/NixOS/nixpkgs/issues/140774
  fixCyclicReference = drv:
    pkgs.haskell.lib.overrideCabal drv (_: {
      enableSeparateBinOutput = false;
    });
  hls = fixCyclicReference pkgs.haskellPackages.haskell-language-server;
in
pkgs.mkShell {
  buildInputs = [
    hls
  ];
}
