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
        tex = pkgs.texlive.combine {
          inherit (pkgs.texlive)
            scheme-medium
            environ
            roboto
            fontaxes # dependency of roboto
            ;
        };
      in
      {
        devShells = {
          default = pkgs.mkShell {
            packages = [
              pkgs.gradle
              pkgs.racket
              pkgs.libxml2
              tex
              self.packages.${system}.make-exam
              pkgs.python3Packages.chevron
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
          pool = pkgs.stdenv.mkDerivation {
            name = "pool";
            src = ./.;
            buildInputs = [ ];
            installPhase = ''
              mkdir -p $out/xml
              cp -R pool/* $out/xml/
            '';
          };
          mock-exam =
            let
              mkBuild =
                lang: data: suffix:
                let
                  outfile = "mock-${lang}${suffix}.tex";
                in
                ''
                  make-exam --template template-${lang}.tex --out mock-temp.tex --language ${lang} questions/*.xml
                  chevron -d ${data} mock-temp.tex > ${outfile}
                  pdflatex ${outfile} && pdflatex ${outfile} && pdflatex ${outfile} 
                '';
            in
            pkgs.stdenv.mkDerivation {
              name = "mock-exam";
              src = ./mock;
              buildInputs = [
                self.packages.${system}.make-exam
                tex
                pkgs.python3Packages.chevron
              ];
              buildPhase =
                (mkBuild "de" "template-data-nosolutions.json" "")
                + (mkBuild "de" "template-data-solutions.json" "-solutions")
                + (mkBuild "en" "template-data-nosolutions.json" "")
                + (mkBuild "en" "template-data-solutions.json" "-solutions");
              installPhase = ''
                mkdir -p $out/pdf
                cp mock-en.pdf mock-en-solutions.pdf mock-de.pdf mock-de-solutions.pdf $out/pdf
              '';
            };
        };
      }
    );
}
