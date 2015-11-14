{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, cassava, directory
      , filepath, process, stdenv, vector
      }:
      mkDerivation {
        pname = "mastery";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base ];
        executableHaskellDepends = [
          aeson base bytestring cassava directory filepath process vector
        ];
        testHaskellDepends = [ base ];
        homepage = "http://github.com/davidrusu/mastery";
        description = "Track your pursuit of mastery";
        license = stdenv.lib.licenses.bsd3;
        buildTools = [ pkgs.cabal-install ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};
in

  if pkgs.lib.inNixShell then drv.env else drv
