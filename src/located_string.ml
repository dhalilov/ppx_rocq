(** Representation of strings with a source location. *)

open Ppxlib

(** {1 Positions} *)

type position =
  { pos: Lexing.position; (** Position inside the source code. *)
    index: int;           (** Index inside the string. *)
  }

let start_position loc =
  { pos = loc.loc_start; index = 0 }

let advance_char { pos; index } char =
  let next_cnum = pos.pos_cnum + 1 in
  let next_pos =
    match char with
    | '\n' ->
       { pos with pos_lnum = pos.pos_lnum + 1;
                  pos_cnum = next_cnum;
                  pos_bol  = next_cnum }
    | _ ->
       { pos with pos_cnum = next_cnum }
  in
  { pos = next_pos; index = index + 1 }

let advance substring pos =
  String.fold_left advance_char pos substring

let previous { pos; index } =
  if pos.pos_cnum > pos.pos_bol && index > 0 then
    Some { pos = { pos with pos_cnum = pos.pos_cnum - 1 };
           index = index - 1 }
  else None

(** {1 Located strings} *)

type t = string loc

let lookup pos string =
  String.get string.txt pos.index

let rec find_f ~from f string =
  match lookup from string with
  | c when f c ->
     Some from
  | c ->
     find_f ~from:(advance_char from c) f string
  | exception Invalid_argument _ ->
     None

let find ~from c string = find_f ~from (Char.equal c) string

let substring ?from ?until string =
  let loc = string.loc in
  let from =
    match from with
    | Some from -> from
    | None -> { pos = loc.loc_start; index = 0 }
  in
  let until =
    match until with
    | Some until -> until
    | None -> { pos = loc.loc_end; index = String.length string.txt }
  in
  let substring_loc = { loc_start = from.pos; loc_end = until.pos; loc_ghost = false } in
  let substring = String.sub string.txt from.index (until.index - from.index) in
  { txt = substring; loc = substring_loc }
