#──────────────────────────────────────────────────────────────────────────────┐
# SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
# SPDX-License-Identifier: LGPL-2.1-or-later                                   │
#──────────────────────────────────────────────────────────────────────────────┘
final: prev: {
   nixtamal = prev.nixtamal.overrideScope (
      final': prev': {
         check = (prev'.check or { }) // {
            EditorConfig =
               final.runCommand "check-EditorConfig"
                  {
                     src =
                        let
                           fs = final.lib.fileset;
                        in
                        (fs.toSource {
                           root = ../..;
                           fileset = fs.unions [
                              (fs.fileFilter (file: file.name == ".editorconfig") ../..)
                              (fs.fromSource prev'.nixtamal.src)
                              ../../nix
                           ];
                        });
                  }
                  ''
                     cd $src
                     ${final.lib.getExe final.editorconfig-checker} | tee $out
                  '';
         };
      }
   );
}
