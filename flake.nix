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
          withVersion = drv : overrideCabal drv (o: {
            version = o.version + "-${nixpkgs.lib.substring 0 8 self.lastModifiedDate}.${self.shortRev or "dirty"}";
          });
      in rec {
        packages = flattenTree rec {
          kaze = withVersion (withGloss (callCabal2nix "kaze" ./kaze {}));
          geometry = withVersion (withGloss (callCabal2nix "geometry" ./geometry {}));
          ezyang-com = withVersion (callCabal2nix "ezyang-com" ./ezyang.com {});
          doisinkidney-com = withVersion (callCabal2nix "doisinkidney-com" ./doisinkidney.com {});
        };
      });
}
