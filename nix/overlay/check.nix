#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
final: prev: {
   nixtamal = prev.nixtamal.overrideScope (
      final': prev': {
         check = {
            format = {
               EditorConfig = final'.callPackage ../check/editorconfig.nix { };

               nixfmt = final'.callPackage ../check/nixfmt.nix { };
            };
         };
      }
   );
}
