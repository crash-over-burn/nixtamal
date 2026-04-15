#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
  description = "Nixtamal - A Nix version pinning tool with first-class support for Darcs, Pijul, and other VCS systems";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/16c7794d0a28b5a37904d55bcca36003b9109aaa";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import ./nix/overlay/default.nix)
            (import ./nix/overlay/development.nix)
            (import ./nix/overlay/check.nix)
          ];
        };
        
        # Build nixtamal package using callPackage
        nixtamalPkg = pkgs.callPackage ./nix/package/nixtamal.nix {
          nixtamal = null; # Prevent infinite recursion
        };
        
        # Development shell using the same approach as shell.nix
        devShell = pkgs.mkShell {
          buildInputs = (nixtamalPkg.buildInputs or []) ++ (nixtamalPkg.checkInputs or []);
          nativeBuildInputs = nixtamalPkg.nativeBuildInputs or [];
        };
      in
      {
        # Package outputs
        packages = {
          nixtamal = nixtamalPkg;
          default = nixtamalPkg;
        };

        # Development shell
        devShells.default = devShell;

        # Test suite integration
        checks = {
          # Basic check that the package builds
          nixtamal-build = nixtamalPkg;

          # Explicit test check
          nixtamal-test = nixtamalPkg;

          # Coverage instrumentation sanity check
          nixtamal-coverage = pkgs.runCommand "nixtamal-coverage-check"
            {
              nativeBuildInputs = (nixtamalPkg.nativeBuildInputs or [ ])
                ++ (nixtamalPkg.buildInputs or [ ])
                ++ (with pkgs.ocamlPackages; [
                  bisect_ppx
                  dune_3
                ]);
            }
            ''
              export HOME="$TMPDIR"
              cp -r ${nixtamalPkg.src} source
              chmod -R u+w source
              cd source

              BISECT_ENABLE=YES dune runtest --instrument-with bisect_ppx --force
              bisect-ppx-report summary --coverage-path _build/default/test > coverage-summary.txt

              mkdir -p "$out"
              cp coverage-summary.txt "$out"/
            '';
        };

        # Library outputs for ecosystem integration
        lib = {
          # Expose the nixtamal package for flake consumption
          nixtamal = nixtamalPkg;
          
          # Helper functions for hybrid workflows
          makeHybridInputs = { 
            extraInputs ? {}
          }: {
            inherit nixtamalPkg;
          } // extraInputs;

          # Utility to create flake-compatible inputs from nixtamal projects
          fromNixtamalProject = projectPath: 
            import (projectPath + "/nix/tamal") { 
              inherit system; 
              nixpkgs = pkgs;
            };
        };

        # App output for running nixtamal directly
        apps = {
          nixtamal = {
            type = "app";
            program = "${nixtamalPkg}/bin/nixtamal";
          };
          default = self.apps.${system}.nixtamal;
        };
      });
}
