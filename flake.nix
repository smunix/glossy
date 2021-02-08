{
  description = "Glossy Graphical Projects for Kayla";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/master";
    flake-utils.url = "github:numtide/flake-utils/master";
  };
  
  outputs = { self, nixpkgs, flake-utils, ... }:
    with flake-utils.lib;
    eachSystem [ "x86_64-linux" ] (system:
      with (import nixpkgs { inherit system; });
      with haskell.lib;
      with haskellPackages;
      let withGloss = drv: overrideCabal drv (_: {
            librarySystemDepends = [ libGL libGLU ];
            doCheck = false;
            doHaddock = false;
          });
      in rec {
        packages = flattenTree rec {
          kaze = withGloss (callCabal2nix "kaze" ./kaze {});
          geometry = withGloss (callCabal2nix "geometry" ./geometry {});
        };
      });
}
