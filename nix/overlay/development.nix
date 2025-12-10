#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
final: prev: {
   nixtamal = prev.nixtamal.overrideScope (
      final': prev': {
         dev-shell = final'.callPackage ../package/dev-shell.nix { };

         nixfmt-3-space = final'.callPackage ../package/nixfmt-3-space.nix { };

         ocamlformat-rpc-bin = final'.callPackage ../package/ocamlformat-rpc-bin.nix { };
      }
   );
}
