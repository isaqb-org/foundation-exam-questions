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
              self.packages.${system}.tag-release
              pkgs.mustache-go
              pkgs.jing-trang # for schema conversion
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
          tag-release = pkgs.stdenv.mkDerivation {
            name = "tag-release";
            src = ./code/foundation-exam;
            buildInputs = [ pkgs.racket ];
            buildPhase = "raco exe --launcher tag-release.rkt";
            installPhase = ''
              mkdir -p $out/bin
              cp tag-release $out/bin
              mkdir -p $out/code/foundation-exam
              cp *.rkt $out/code/foundation-exam
            '';
            fixupPhase = ''
              substituteInPlace $out/bin/tag-release --replace-fail $PWD $out/code/foundation-exam
            '';
          };
          pool = pkgs.stdenv.mkDerivation {
            name = "pool";
            src = ./.;
            buildInputs = [ ];
            installPhase = ''
              mkdir -p $out/xml
              cp -R pool/* $out/xml/
              cp -R mock/questions $out/xml/mock
            '';
          };
          mock-exam =
            let
              mkBuild =
                lang: solutions:
                let
                  suffix = if solutions then "-solutions" else "";
                  solutionsReplacement = if solutions then "true" else "false";
                  outfile = "mock-${lang}${suffix}.tex";
                in
                ''
                  make-exam --template template-${lang}.tex --out mock-temp.tex --language ${lang} questions/*.xml
                  (cat release.yml; echo solutions: ${solutionsReplacement}) | mustache mock-temp.tex > ${outfile}
                  pdflatex ${outfile} && pdflatex ${outfile} && pdflatex ${outfile} 
                '';
            in
            pkgs.stdenv.mkDerivation {
              name = "mock-exam";
              src = ./mock;
              buildInputs = [
                self.packages.${system}.make-exam
                tex
                pkgs.mustache-go
              ];
              buildPhase =
                (mkBuild "de" false) + (mkBuild "de" true) + (mkBuild "en" false) + (mkBuild "en" true);
              installPhase = ''
                mkdir -p $out/pdf
                cp mock-en.pdf mock-en-solutions.pdf mock-de.pdf mock-de-solutions.pdf $out/pdf
              '';
            };
        };
        apps = {
          xmllint = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "lint-xml" ''
                set -e

                FAILED=0

                for file in mock/questions/mock*.xml pool/*/*.xml; do
                  echo "Validating: $file"
                  if ${pkgs.libxml2}/bin/xmllint --relaxng exam.rng --noout "$file" 2>&1; then
                    echo "  ✓ Valid"
                  else
                    echo "  ✗ Invalid"
                    FAILED=1
                  fi
                done

                if [ $FAILED -eq 1 ]; then
                  echo "XML validation failed!"
                  exit 1
                fi

                echo "All XML files are valid!"
              ''
            );
          };
        };
      }
    );
}
