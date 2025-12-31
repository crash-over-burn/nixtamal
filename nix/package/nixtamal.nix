#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
   lib,
   coreutils,
   python3Packages,
   nix-prefetch-darcs,
   nix-prefetch-git,
   nix-prefetch-pijul,
   ocamlPackages,
   testers,
   nixtamal,
}:

ocamlPackages.buildDunePackage {
   pname = "nixtamal";
   version = "0.0.8-alpha";
   release_year = 2025;

   src =
      let
         fs = lib.fileset;

         ocaml_project =
            file:
            lib.lists.elem file.name [
               "dune"
               "dune-project"
               "dune-workspace"
            ]
            || file.hasExt "opam";

         ocaml_src =
            file:
            lib.lists.any file.hasExt [
               "ml"
               "mld"
               "mli"
               "mly"
            ];
      in
      fs.toSource {
         root = ../..;
         fileset = fs.difference (fs.unions [
            ../../LICENSE.txt
            (fs.fileFilter (file: file.hasExt "txt") ../../license)
            (fs.fileFilter ocaml_project ../..)
            (fs.fileFilter ocaml_src ../../bin)
            (fs.fileFilter ocaml_src ../../lib)
            (fs.fileFilter ocaml_src ../../test)
            ../../doc/manifest.rst
         ]) (fs.maybeMissing ../../_build);
      };

   env = {
      DUNE_PROFILE = "release";
   };

   nativeBuildInputs = [
      python3Packages.docutils
      # NOTE: no KDL support
      python3Packages.pygments
   ];

   buildInputs = [
      # required since the prefetcher scripts presently don’t specify all of
      # their inputs
      coreutils
      nix-prefetch-darcs
      nix-prefetch-git
      nix-prefetch-pijul
   ]
   ++ (with ocamlPackages; [
      camomile
      cmdliner
      eio
      eio_main
      fmt
      jingoo
      (jsont.override {
         withBrr = false;
         withBytesrw = true;
      })
      kdl
      logs
      ppx_deriving
      ppx_deriving_qcheck
      saturn
      uri
   ]);

   postPatch = ''
      substituteInPlace bin/main.ml \
         --subst-var version
      substituteInPlace lib/lock_loader.ml \
         --subst-var release_year
   '';

   doCheck = true;

   checkInputs = with ocamlPackages; [
      alcotest
      qcheck
      qcheck-alcotest
   ];

   passthru.tests.version = testers.testVersion {
      package = nixtamal;
      command = "${nixtamal.meta.mainProgram} --version";
   };

   meta = {
      license = with lib.licenses; [ gpl3Plus ];
      platforms = lib.platforms.unix;
      mainProgram = "nixtamal";
   };
}
