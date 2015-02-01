with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, containers, diagrams, diagrams-contrib
             , diagrams-lib, diagrams-svg, parsec, parsers, readline, stdenv
             , SVGFonts, text
             }:
             mkDerivation {
               pname = "truths";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [
                 base containers diagrams diagrams-contrib diagrams-lib diagrams-svg
                 parsec parsers readline SVGFonts text
               ];
               description = "Automatic generation of truth tables for propositional logic";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
