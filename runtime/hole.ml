(** Placeholder values for parsing with incomplete information. *)

open Names
open Tactics

(** {1 Definitions} *)

type hole = Hole of int

(* We capture the globalization environment, so that constrexprs substituted
   later can be internalized correctly. *)
type glob_hole = hole * Genintern.glob_sign

(** Pretty-printing representation of holes. *)
let hole_name (Hole n) = "__ppx_hole__" ^ string_of_int n

(** {1 Generic argument} *)

(** We treat holes as a generic term with a delayed interpretation. *)

let wit_hole : (hole, glob_hole) GenConstr.tag = GenConstr.create "ppx_rocq:hole"

(** {2 Internalization} *)

let () =
  let intern_hole ?loc glob_sign hole = hole, glob_sign in
  Genintern.register_intern_constr wit_hole intern_hole

(** {2 Interpretation} *)

let () =
  let interp_hole ?loc ~poly glob_env sigma tycon (Hole n, _) =
    failwith "Cannot interpret hole"
  in
  GlobEnv.register_constr_interp0 wit_hole interp_hole

(** {2 Module substitution} *)

let () = Gensubst.register_constr_subst wit_hole (fun _ v -> v)

(** {2 Printing} *)

let () =
  let print_hole hole = Genprint.PrinterBasic (fun env sigma -> Pp.str (hole_name hole)) in
  let print_glob_hole (hole, _) = print_hole hole in
  Genprint.register_constr_print wit_hole print_hole print_glob_hole

(** {2 Constructors, destructors} *)

let make ?loc n =
  CAst.make ?loc (Constrexpr.CGenarg (Raw (wit_hole, Hole n)))

let find_hole t =
  let get_raw : type raw glob. (raw, glob) GenConstr.tag -> GenConstr.raw -> raw option = fun t (Raw (tag, value)) ->
    match GenConstr.eq t tag with
    | Some Refl -> Some value
    | None -> None
  in
  match t with
  | Constrexpr.CGenarg raw -> get_raw wit_hole raw
  | _ -> None

let rec fill_holes f t =
  match find_hole t.CAst.v with
  | Some hole -> f ?loc:t.loc hole
  | None -> Terms.Expr.map (fill_holes f) t

let find_glob_hole t =
  let get_glob : type raw glob. (raw, glob) GenConstr.tag -> GenConstr.glb -> glob option = fun t (Glb (tag, value)) ->
    match GenConstr.eq t tag with
    | Some Refl -> Some value
    | None -> None
  in
  match t with
  | Glob_term.GGenarg raw -> get_glob wit_hole raw
  | _ -> None

let rec fill_glob_holes f t =
  match find_glob_hole (DAst.get t) with
  | Some (Hole n, glob_sign) -> f ?loc:t.loc (Hole n) glob_sign
  | None -> Terms.Glob_constr.map (fill_glob_holes f) t
