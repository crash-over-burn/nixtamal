#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
let
   inputs = import ./nix/tamal { };

   pkgs = import inputs.nixpkgs {
      overlays = [
         (import ./nix/overlay/default.nix)
         (import ./nix/overlay/development.nix)
      ];
   };
in
{
   inherit (pkgs) nixtamal;

   inherit (pkgs.nixtamal) default;

   shell = pkgs.nixtamal.dev-shell;

   test = {
      inherit (pkgs.nixtamal) tests;
   };

}
