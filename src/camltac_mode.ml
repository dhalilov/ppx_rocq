(** Camltac-specific extensions. *)

open Ppxlib

let camltac_mode = ref false

let () =
  Driver.Cookies.add_simple_handler
    "ppx_rocq.camltac_mode"
    Ast_pattern.(ebool __)
    ~f:(fun value -> camltac_mode := Option.value ~default:false value)

let if_enabled f x =
  if !camltac_mode then f x else x
