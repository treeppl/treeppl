{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    relocatable.url = "github:elegios/relocatable.nix";
    miking.url = "github:elegios/miking/update-nix-packaging-stuff?dir=misc/packaging";
    miking.inputs.nixpkgs.follows = "nixpkgs";
    miking-dppl.url = "github:elegios/miking-dppl/nix-flake?dir=misc/packaging";
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
          treeppl-wrapped = pkgs.writeShellApplication {
            name = "tpplc";
            runtimeInputs = [pkgs.ocamlPackages.findlib pkgs.ocamlPackages.owl pkgs.stdenv.cc];
            # NOTE(vipa, 2025-06-03): These commands will add *all*
            # things in the current nix store to environment
            # variables. When bundling this script the created nix
            # store will only contain what's needed. The explicit
            # references to `treeppl-unwrapped` (except the
            # `bin/tpplc` reference at the end) are just to get to a
            # directory inside the nix store, to reach the store
            # itself.
            text = ''
              for bin in ${treeppl-unwrapped}/../*/bin; do
                PATH="$bin"''${PATH:+:''${PATH}}
              done
              export PATH
              for lib in ${treeppl-unwrapped}/../*/lib; do
                LIBRARY_PATH="$lib"''${LIBRARY_PATH:+:''${LIBRARY_PATH}}
              done
              export LIBRARY_PATH
              for site in ${treeppl-unwrapped}/../*/lib/ocaml/*/site-lib; do
                OCAMLPATH="$site''${OCAMLPATH:+:''${OCAMLPATH}}"
              done
              export OCAMLPATH
              for mcore in ${treeppl-unwrapped}/../*/lib/mcore/*; do
                MCORE_LIBS="$(basename "$mcore")=$mcore''${MCORE_LIBS:+:''${MCORE_LIBS}}"
              done
              export MCORE_LIBS
              exec -a "$0" "${treeppl-unwrapped}/bin/tpplc" "$@"
            '';
          };
        in
          rec {
            packages.treeppl-unwrapped = treeppl-unwrapped;
            packages.tpplc-deploy = relocatable.bundlers.${system}.default treeppl-wrapped;
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
