{
  description = "iSAQB Foundation Exam Code";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      ...
    }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            packages = [
              pkgs.gradle
              pkgs.racket
              pkgs.libxml2
              pkgs.texlive.combined.scheme-medium
              self.packages.${system}.make-exam
            ];
          };
        };
        packages = {
          default = self.packages.${system}.make-exam;
          make-exam = pkgs.stdenv.mkDerivation {
            name = "make-exam";
            src = ./code/foundation-exam;
            buildInputs = [ pkgs.racket ];
            buildPhase = "raco exe --launcher make-exam.rkt";
            installPhase = ''
              mkdir -p $out/bin
              cp make-exam $out/bin
              mkdir -p $out/code/foundation-exam
              cp *.rkt $out/code/foundation-exam
            '';
            fixupPhase = ''
              substituteInPlace $out/bin/make-exam --replace-fail $PWD $out/code/foundation-exam
            '';
          };
          mock-exam = pkgs.stdenv.mkDerivation {
            name = "mock-exam";
            src = ./mock;
            buildInputs = [
              self.packages.${system}.make-exam
              pkgs.texlive.combined.scheme-medium
            ];
            buildPhase = ''
              make-exam --template template-en.tex --out mock-en.tex --language en questions/*.xml
              pdflatex mock-en.tex ; pdflatex mock-en.tex ; pdflatex mock-en.tex
              make-exam --template template-de.tex --out mock-de.tex --language de questions/*.xml
              pdflatex mock-de.tex ; pdflatex mock-de.tex ; pdflatex mock-de.tex
            '';
            installPhase = ''
              mkdir -p $out/pdf
              cp mock-en.pdf $out/pdf
              cp mock-de.pdf $out/pdf
            '';
          };
        };
      }
    );
}
