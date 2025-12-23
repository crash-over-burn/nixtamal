#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
   mkShell,
   kdlfmt,
   topiary,
   ocamlPackages,
   mark-darcs-weak-hash,
   nixtamal,
   nixfmt,
   ocamlformat-rpc-bin,
}:

mkShell {
   name = "nixtamal";
   inputsFrom = [
      nixtamal
   ];
   packages = [
      kdlfmt
      mark-darcs-weak-hash
      nixfmt
      topiary

      ocamlPackages.alcotest
      ocamlPackages.qcheck
      ocamlPackages.qcheck-alcotest
      ocamlPackages.ocaml-lsp
      ocamlformat-rpc-bin # 💢 why does the LSP depend on ocamlformat‽
   ];
   env = {
      TOPIARY_CONFIG_FILE = "${../../.topiary.ncl}";
   };
}
