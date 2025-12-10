#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
{
   mkShell,
   kdlfmt,
   topiary,
   ocamlPackages,
   nixtamal,
   nixfmt-3-space,
   ocamlformat-rpc-bin,
}:

mkShell {
   name = "nixtamal";
   inputsFrom = [
      nixtamal
   ];
   packages = [
      kdlfmt
      nixfmt-3-space
      topiary

      ocamlPackages.alcotest
      ocamlPackages.ocaml-lsp
      ocamlformat-rpc-bin # 💢 why does the LSP depend on ocamlformat‽
   ];
   env = {
      TOPIARY_CONFIG_FILE = "${../../.topiary.ncl}";
   };
}
