(** Methods for detecting and parsing pattern variables. *)

open Ppxlib

type kind =
  | First_order
  | Second_order

type t =
  { name: Located_string.t;
    kind: kind }

let parse_name ~question_mark string =
  let ident_start = Located_string.advance "?" question_mark in
  (* TODO: For now, we only support a very basic set of Latin letters.
     We might want to extend it to the intersection of valid OCaml identifiers
     with Rocq identifiers (and warn when only one of them supports the character). *)
  let is_ident_char c = Char.Ascii.is_lower c || c == '_' || c == '\'' in
  let ident_end = Located_string.find_f ~from:ident_start (Fun.negate is_ident_char) string in
  let name = Located_string.substring ~from:ident_start ?until:ident_end string in
  match name.txt with
  | "" -> None
  | _ -> Some (name, ident_end)

let find ~from string =
  let ( let* ) = Option.bind in
  let* question_mark = Located_string.find ~from '?' string in
  (* Check for second-order patterns of the form [@?ident]. *)
  let kind, start_pos =
    match Located_string.previous question_mark with
    | Some pos when Located_string.lookup pos string == '@' ->
       Second_order, pos
    | _ -> First_order, question_mark
  in
  match parse_name ~question_mark string with
  | Some (name, ident_end) ->
     let loc = { loc_start = start_pos.pos; loc_end = name.loc.loc_end; loc_ghost = false } in
     Some ({ name = { txt = name.txt; loc }; kind }, ident_end)
  | None -> None

let rec find_all_from ~from string =
  match find ~from string with
  | None -> []
  | Some (pattern, None) -> [pattern]
  | Some (pattern, Some pattern_end) ->
     pattern :: find_all_from ~from:pattern_end string

let find_all ~loc string =
  let string = { txt = string; loc } in
  let start = Located_string.start_position loc in
  find_all_from ~from:start string
