(*─────────────────────────────────────────────────────────────────────────────┐
│ SPDX-FileCopyrightText: 2025 toastal <https://toast.al/contact/>             │
│ SPDX-License-Identifier: LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception │
└─────────────────────────────────────────────────────────────────────────────*)
let pp ppf = Fmt.pf ppf {|┏┓╻+╻ ╱┏┳┓┏┓┏┳┓┏┓╻@.┃┃┃┃┗━┓╹┃╹┣┫┃┃┃┣┫┃@.╹┗┛╹╱ ╹ ╹ ╹╹╹ ╹╹╹┗┛|}

let show = Fmt.str "%t" pp
