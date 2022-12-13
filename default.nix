{ ghc }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs)
    mkShell;
    
  inherit (pkgs.haskell.packages."${ghc}") 
    haskell-language-server
    hlint
    prim-swar
    swar; 
}