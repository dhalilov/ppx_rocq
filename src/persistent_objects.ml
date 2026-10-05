(** Support for persistent objects for Camltac. *)

open Ppxlib

(** Generates a unique ID based on the string location. *)
let generate_id pos =
  pos.pos_fname ^ ":" ^ string_of_int pos.pos_lnum ^ ":" ^ string_of_int pos.pos_cnum

let persist ~loc ~string_loc e =
  let persist e =
    let id = Ast_builder.Default.estring ~loc (generate_id string_loc.loc_start) in
    [%expr Runtime.Environment.persist
        ~id:[%e id]
        (fun () -> [%e e])]
  in
  Camltac_mode.if_enabled persist e
