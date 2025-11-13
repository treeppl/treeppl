{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    relocatable.url = "github:elegios/relocatable.nix";
    miking.url = "github:miking-lang/miking?dir=misc/packaging";
    miking.inputs.nixpkgs.follows = "nixpkgs";
    miking-dppl.url = "github:miking-lang/miking-dppl?dir=misc/packaging";
    miking-dppl.inputs.nixpkgs.follows = "nixpkgs";
    miking-dppl.inputs.miking.follows = "miking";
  };

  outputs = { self, nixpkgs, flake-utils, relocatable, miking, miking-dppl }:
    let
      mkPkg = system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.pkgs;
          mpkgs = miking.packages.${system};
          mdpkgs = miking-dppl.packages.${system};
          treeppl-unwrapped = pkgs.callPackage ./treeppl-unwrapped.nix {
            inherit (mpkgs) miking-lib miking-unwrapped;
            inherit (mdpkgs) miking-dppl-lib;
          };
          treeppl-tmp-tar-gz = relocatable.bundlers.${system}.fixedLocationTarGz {
            drv = treeppl-unwrapped;
            tarName = "${treeppl-unwrapped.name}-${system}";
            extraSetup = storePath: ''
              for site in ${storePath}*/lib/ocaml/*/site-lib; do
                OCAMLPATH="$site''${OCAMLPATH:+:''${OCAMLPATH}}"
              done
              export OCAMLPATH
              for mcore in ${storePath}*/lib/mcore/*; do
                MCORE_LIBS="$(basename "$mcore")=$mcore''${MCORE_LIBS:+:''${MCORE_LIBS}}"
              done
              export MCORE_LIBS
            '';
            runtimeInputs = [pkgs.ocamlPackages.findlib pkgs.ocamlPackages.owl pkgs.stdenv.cc];
          };
        in
          rec {
            packages.treeppl-unwrapped = treeppl-unwrapped;
            packages.tpplc-tmp-tar-gz = treeppl-tmp-tar-gz;
            devShells.default = pkgs.mkShell {
              name = "TreePPL dev shell";
              inputsFrom = [ packages.treeppl-unwrapped ];
              buildInputs = [ pkgs.ocamlPackages.owl ];
            };
          };
    in flake-utils.lib.eachDefaultSystem mkPkg // rec {
      overlays.treeppl = final: prev: {
        treeppl-unwrapped = final.callPackage ./treeppl-unwrapped.nix {};
      };
      overlays.default = overlays.treeppl;
    };
}
