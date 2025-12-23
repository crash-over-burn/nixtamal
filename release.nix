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
         (import ./nix/overlay/check.nix)
      ];
   };

   inherit (pkgs) lib;
in
{
   inherit (pkgs) nixtamal;

   inherit (pkgs.nixtamal) default;

   shell = pkgs.nixtamal.dev-shell;

   check = lib.concatMapAttrs (
      parent: children:
      if lib.isDerivation children then
         { ${parent} = children; }
      else
         lib.mapAttrs' (child: value: {
            name = "${parent}_${child}";
            inherit value;
         }) children
   ) pkgs.nixtamal.check;
}
