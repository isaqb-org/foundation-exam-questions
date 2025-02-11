{
  description = "Active Group Kontakte";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
      devShells = {
        default = pkgs.mkShell {
          packages = [
            pkgs.gradle
            pkgs.racket
          ];
        };
      };
    });
}
