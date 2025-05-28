{
  description = "iSAQB Foundation Exam Code";

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
            pkgs.racket-minimal
            pkgs.libxml2
            self.packages.${system}.make-exam
          ];
        };
      };
      packages = {
        default = self.packages.${system}.make-exam;
        make-exam-rkt = pkgs.stdenv.mkDerivation {
          name = "make-exam-rkt";
          src = ./code/foundation-exam;
          installPhase = "mkdir -p $out/code/foundation-exam && cp *.rkt $out/code/foundation-exam";
        };
        make-exam = pkgs.stdenv.mkDerivation {
          name = "make-exam";
          src = ./code/foundation-exam;
          buildInputs = [ pkgs.racket-minimal ];
          buildPhase = "mkdir $out && raco exe make-exam.rkt";
          installPhase = "mkdir -p $out/bin && cp make-exam $out/bin";
        };
      };
    });
}

