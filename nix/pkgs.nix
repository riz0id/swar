args:

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions (map (f: f args) [  
      (import exts/prim-swar.nix)
      (import exts/swar.nix)
    ]) pkgs pkgs;
}