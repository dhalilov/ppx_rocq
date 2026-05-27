(** Placeholder values for parsing with incomplete information. *)

open Names
open Tactics

(** {1 Definitions} *)

type hole = Hole of int

(* We capture the globalization environment, so that constrexprs substituted
   later can be internalized correctly. *)
type glob_hole = hole * Genintern.glob_sign

(* Inside constrs, we represent holes as specially-named evars. *)
type constr_hole = EConstr.t

let circled_numbers =
  [| "⓪" ; "①" ; "②" ; "③" ; "④" ; "⑤" ; "⑥" ; "⑦" ; "⑧" ; "⑨" ;
     "⑩" ; "⑪" ; "⑫" ; "⑬" ; "⑭" ; "⑮" ; "⑯" ; "⑰" ; "⑱" ; "⑲" ;
     "⑳" |]

(** Pretty-printing representation of holes. *)
let hole_name (Hole n) =
  if n < Array.length circled_numbers then circled_numbers.(n)
  else "□[" ^ string_of_int n ^ "]"

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
    let env = GlobEnv.renamed_env glob_env in
    let hole_name = hole_name (Hole n) in
    let sigma, typ, relevance =
      match tycon with
      | Some typ -> sigma, typ, None
      | None ->
         let sigma, (typ, sort) =
           Evarutil.new_type_evar
             ~src:(loc, Evar_kinds.InternalHole)
             ~naming:(Namegen.IntroIdentifier (Id.of_string_soft ("type of " ^ hole_name)))
             env
             sigma
             Evd.univ_flexible in
         let relevance = EConstr.ESorts.relevance_of_sort sort in
         sigma, typ, Some relevance
    in
    let sigma, evar = Evarutil.new_evar
                        ~src:(loc, Evar_kinds.InternalHole)
                        ~naming:(Namegen.IntroIdentifier (Id.of_string_soft hole_name))
                        ?relevance
                        env
                        sigma
                        typ
    in
    Environ.make_judge evar typ, sigma
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

let rec find_glob_holes t =
  match find_glob_hole (DAst.get t) with
  | Some (Hole n, glob_sign) -> [n, glob_sign]
  | None ->
     let f acc subterm = find_glob_holes subterm @ acc in
     Terms.Glob_constr.fold f [] t

let rec fill_glob_holes f t =
  match find_glob_hole (DAst.get t) with
  | Some (Hole n, glob_sign) -> f ?loc:t.loc (Hole n) glob_sign
  | None -> Terms.Glob_constr.map (fill_glob_holes f) t

let parse_hole_name str =
  match Array.find_index (String.equal str) circled_numbers with
  | Some i -> Some i
  | None ->
     let prefix = "□[" in
     if String.starts_with ~prefix str then
       let prefix_length = String.length prefix in
       int_of_string_opt (String.sub str prefix_length (String.length str - prefix_length - 1))
     else None

let find_constr_hole sigma t =
  match EConstr.kind sigma t with
  | Evar (e, _) ->
      begin match Evd.evar_ident e sigma with
      | Some fullpath ->
         let name = Libnames.basename fullpath in
         parse_hole_name (Id.to_string name)
      | None -> None
      end
  | _ -> None

let fill_constr_holes f t =
  let rec fill_constr_holes sigma f t =
    match find_constr_hole sigma t with
    | Some n -> f (Hole n)
    | None -> EConstr.map sigma (fill_constr_holes sigma f) t
  in
  let* sigma = evar_map in
  Proofview.Monad.return (fill_constr_holes sigma f t)
