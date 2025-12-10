#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
let
   pkgs_src = builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/bd88d6c13ab85cc842b93d53f68d6d40412e5a18.tar.gz";
      sha256 = "0nxk7d12y1zq7rbfjkspprbh2wac61cjsjaidc94gpv9bd0pbsja";
   };

   pkgs = import pkgs_src {
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
