{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    reanimate = {
      url = "github:reanimate/reanimate";
      flake = false;
    };
    reanimate-svg = {
      url = "github:reanimate/reanimate-svg";
      flake = false;
    };
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];
      perSystem = { self', pkgs, ... }: {
        haskellProjects.default = {
          packages.animate.root = ./.;  # This value is detected based on .cabal files
          overrides = self: super:  with pkgs.haskell.lib; {
            reanimate =  dontCheck (self.callCabal2nix "reanimate" inputs.reanimate {}); 
            reanimate-svg = appendBuildFlag  (dontCheck (self.callCabal2nix  "reanimate-svg" inputs.reanimate-svg  {})) "--ghc-option=-fsimpl-tick-factor=200"; 
          };
          # devShell = {
          #  enable = true;  # Enabled by default
          #  tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };
          #  hlsCheck.enable = true;
          # };
        };
        # haskell-flake doesn't set the default package, but you can do it here.
        packages.default = self'.packages.animate;
      };
    };
}
