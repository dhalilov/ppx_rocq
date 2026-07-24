(** Support for persistent objects for Camltac. *)

open Ppxlib

(** Generates a unique ID based on the string location. *)
let generate_id pos =
  pos.pos_fname ^ ":" ^ string_of_int pos.pos_lnum ^ ":" ^ string_of_int pos.pos_cnum

let camltac_mode = ref false

let () =
  Driver.Cookies.add_simple_handler
    "ppx_rocq.camltac_mode"
    Ast_pattern.(ebool __)
    ~f:(fun value -> camltac_mode := Option.value ~default:false value)

let persist ~loc ~string_loc e =
  if not !camltac_mode then e
  else
    let id = Ast_builder.Default.estring ~loc (generate_id string_loc.loc_start) in
    [%expr Runtime.Environment.persist
        ~id:[%e id]
        (fun () -> [%e e])]
