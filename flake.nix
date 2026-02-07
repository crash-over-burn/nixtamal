#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
  description = "Nixtamal - A Nix version pinning tool with first-class support for Darcs, Pijul, and other VCS systems";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        
        # Build nixtamal package using the traditional nix-build approach
        # This preserves the existing build infrastructure
        nixtamalPkg = pkgs.callPackage ./nix/package/nixtamal.nix {
          # Override dependencies as needed
          nixtamal = null; # Prevent infinite recursion
        };
        
        # Development shell using the same approach as shell.nix
        devShell = pkgs.mkShell {
          buildInputs = nixtamalPkg.buildInputs or [];
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