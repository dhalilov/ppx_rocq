(** API for parsing terms. *)

open Names

let parse ?loc entry s = Procq.parse_string ?loc entry s

(** Execute function [f] in synterp phase. This function is a hack that tricks
    Rocq by temporarily setting the [Flags.in_synterp] flag. *)
let with_synterp f =
  let old = !Flags.in_synterp_phase in
  Flags.in_synterp_phase := Some true;
  Fun.protect
    ~finally:(fun () -> Flags.in_synterp_phase := old)
    f

(* Entries registered at synterp time. *)
let constrexpr     = with_synterp (fun () -> Procq.eoi_entry Procq.Constr.term)
let ident          = with_synterp (fun () -> Procq.eoi_entry Procq.Constr.ident)
let qualid         = with_synterp (fun () -> Procq.eoi_entry Procq.Prim.qualid)
let match_pattern  = with_synterp (fun () -> Procq.eoi_entry Procq.Constr.cpattern)
let vernac_control = with_synterp (fun () -> Procq.eoi_entry Pvernac.Vernac_.vernac_control)
let ltac           = with_synterp (fun () -> Procq.eoi_entry Ltac_plugin.Pltac.tactic)
let ltac2          = with_synterp (fun () -> Procq.eoi_entry Ltac2_plugin.G_ltac2.ltac2_expr)

let parse_constrexpr ?loc s    = parse ?loc constrexpr s
let parse_ident ?loc s         = parse ?loc ident s
let parse_qualid ?loc s        = parse ?loc qualid s
let parse_vernac ?loc s        = parse ?loc vernac_control s
let parse_ltac ?loc s          = parse ?loc ltac s
let parse_ltac2 ?loc s         = parse ?loc ltac2 s
let parse_match_pattern ?loc s = parse ?loc match_pattern s

let glob_constr_of_string ?loc s =
  let parsed_term = parse_constrexpr ?loc s in
  Terms.Glob_constr.of_constrexpr parsed_term

let constr_of_string ?loc s =
  let parsed_term = parse_constrexpr ?loc s in
  Terms.Constr.of_constrexpr parsed_term

let open_constr_of_string ?loc s =
  let parsed_term = parse_constrexpr ?loc s in
  Terms.Open_constr.of_constrexpr parsed_term

let match_pattern_of_string ?loc s =
  let parsed_pattern = parse_match_pattern ?loc s in
  Terms.Pattern.of_constrexpr parsed_pattern

(** {1 Parsing with antiquotations} *)

(** Representation of holes inside terms. *)
type hole = Hole of int

let circled_numbers =
  [| "⓪" ; "①" ; "②" ; "③" ; "④" ; "⑤" ; "⑥" ; "⑦" ; "⑧" ; "⑨" ;
     "⑩" ; "⑪" ; "⑫" ; "⑬" ; "⑭" ; "⑮" ; "⑯" ; "⑰" ; "⑱" ; "⑲" ;
     "⑳" |]

(** Pretty-printing representation of holes. *)
let hole_name (Hole n) =
  if n < Array.length circled_numbers then circled_numbers.(n) else "□[" ^ string_of_int n ^ "]"

let parse_hole_name str =
  match Array.find_index (String.equal str) circled_numbers with
  | Some i -> Some (Hole i)
  | None ->
     let prefix = "□[" in
     if String.starts_with ~prefix str then
       let prefix_length = String.length prefix in
       match int_of_string_opt (String.sub str prefix_length (String.length str - prefix_length - 1)) with
       | Some i -> Some (Hole i)
       | None -> None
     else None

(** Representation of globalized holes. We capture the globalization
    environment, so that constrexprs substituted later can be reinterpreted
    correctly. *)
type globalized_hole = hole * Genintern.glob_sign

(** Generic term for holes. *)
let wit_hole : (hole, globalized_hole) GenConstr.tag =
  GenConstr.create "ppx_rocq:hole"

(** Internalize the given hole by capturing the globalization environment. *)
let intern_hole ?loc glob_sign hole =
  hole, glob_sign

(** Convert the given globalized hole to a [constr] by interpreting it as an
    evar. *)
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


let () =
  Genintern.register_intern_constr wit_hole intern_hole;
  GlobEnv.register_constr_interp0 wit_hole interp_hole;
  Gensubst.register_constr_subst wit_hole (fun _ v -> v)

let () =
  let print_hole hole = Genprint.PrinterBasic (fun env sigma -> Pp.str (hole_name hole)) in
  let print_glob_hole (hole, _) = print_hole hole in
  Genprint.register_constr_print wit_hole print_hole print_glob_hole

(** {2 Generic arguments} *)

type genarg_antiquotation =
  [ `Constr of Terms.constr       (** {v %{…} v} or {v %constr:{…} v} *)
  | `Preterm of Terms.glob_constr (** {v %preterm:{…} v} *)
  ]

type antiquotation =
  [ genarg_antiquotation
  | `Expr of Terms.constrexpr (** {v %expr:{…} v} *)
  ]

(** As a performance optimization, we interpret {v %preterm:{…} v} and
    {v %constr:{…} v} as generic arguments, so that we don't have to
    re-globalize/re-typecheck the given terms. *)

let wit_antiquotation : (genarg_antiquotation, genarg_antiquotation) GenConstr.tag =
  GenConstr.create "ppx_rocq:antiquotation"

let intern_antiquotation ?loc glob_sign (antiquotation: genarg_antiquotation) =
  (* constr and preterm antiquotations are already internalized. *)
  antiquotation

let () =
  Genintern.register_intern_constr wit_antiquotation intern_antiquotation

let interp_constr_antiquotation ?loc env sigma tycon c =
  let judgment = Retyping.get_judgment_of env sigma c in
  match tycon with
  | None -> judgment, sigma
  | Some ty ->
     (* Recheck judgement against the typing condition. *)
     let sigma =
       try Evarconv.unify_leq_delay env sigma judgment.uj_type ty
       with Evarconv.UnableToUnify (sigma, e) ->
         Pretype_errors.error_actual_type ?loc env sigma judgment ty e
     in
     { judgment with uj_type = ty }, sigma

let interp_preterm_antiquotation env sigma tycon t =
  let open Pretyping in
  let tycon =
    match tycon with
    | Some ty -> OfType ty
    | None -> WithoutTypeConstraint
  in
  let sigma, t, ty =
    Pretyping.understand_tcc_ty
      ~flags:(Ltac2_plugin.Tac2core.preterm_flags)
      ~expected_type:tycon
      env sigma t
  in
  Environ.make_judge t ty, sigma

let interp ?loc ~poly env sigma tycon =
  let env = GlobEnv.renamed_env env in
  function
  | `Constr c -> interp_constr_antiquotation ?loc env sigma tycon c
  | `Preterm t -> interp_preterm_antiquotation env sigma tycon t

let () =
  GlobEnv.register_constr_interp0 wit_antiquotation interp

(* Module substitution does not affect our antiquotations. *)
let () =
  Gensubst.register_constr_subst wit_antiquotation (fun _ v -> v)

let () =
  let print_antiquotation (antiquotation: genarg_antiquotation) =
    let open Pp in
    Genprint.PrinterBasic (fun env sigma ->
      match antiquotation with
      | `Constr c -> str "%{" ++ Printer.pr_econstr_env env sigma c ++ str "}"
      | `Preterm t -> str "%preterm:{" ++ Printer.pr_glob_constr_env env sigma t ++ str "}"
    )
  in
  Genprint.register_constr_print wit_antiquotation print_antiquotation print_antiquotation

(** {2 Camlp5 grammar tricks} *)

(** Generic production rule for antiquotations:
    [[ [ "%{"; n = natural; "}" -> { Hole n } ] ]]
 *)
let antiquotation_production =
  let open Procq in
  Production.make
    (Rule.next
       (Rule.next
          (Rule.next (Procq.Rule.stop)
             ((Symbol.token (Tok.PKEYWORD ("%{")))))
          ((Symbol.nterm Prim.natural)))
       ((Symbol.token (Tok.PKEYWORD ("}")))))
    (fun _ n _ loc -> CAst.make ~loc (Constrexpr.CGenarg (Raw (wit_hole, Hole n))))

(** Execute function [f] where [entry] allows anti-quotations in the map
    [context]. *)
let with_antiquotations entry f =
  with_synterp (fun () ->
    let grammar_state = Procq.freeze () in
    let () =
      Egramml.grammar_extend ~ignore_kw:false
        entry
        (Reuse (Some "0", [antiquotation_production]))
    in
    Fun.protect
      ~finally:(fun () -> Procq.unfreeze grammar_state)
      f
  )

(** {2 Quasiparsing methods} *)

let quasiparse ?loc s =
  with_antiquotations Procq.Constr.term (fun () -> parse_constrexpr ?loc s)

open Proofview.Monad
open Tactics

let generic_quasiparse ~lower ~plug_holes ?loc s =
  let parsed_term = quasiparse ?loc s in
  lower parsed_term >>= fun lowered_term ->
  return (fun values -> plug_holes lowered_term values)

let get_raw : type raw glob. (raw, glob) GenConstr.tag -> GenConstr.raw -> raw option = fun t (Raw (tag, value)) ->
  match GenConstr.eq t tag with
  | Some Refl -> Some value
  | None -> None

let get_glob : type raw glob. (raw, glob) GenConstr.tag -> GenConstr.glb -> glob option = fun t (Glb (tag, value)) ->
  match GenConstr.eq t tag with
  | Some Refl -> Some value
  | None -> None

let quasiparse_constrexpr ?loc s =
  let parsed_term = quasiparse ?loc s in
  let open Constrexpr in
  let plug_antiquotation ?loc : antiquotation -> Terms.constrexpr = function
    | `Expr e -> e
    | #genarg_antiquotation as antiquotation ->
       let genarg = CGenarg (GenConstr.Raw (wit_antiquotation, antiquotation)) in
       CAst.make ?loc genarg
  in
  let plug_holes constrexpr substitution =
    let rec f constrexpr =
      match constrexpr.CAst.v with
      | CGenarg raw ->
         begin match get_raw wit_hole raw with
         | Some (Hole n) -> plug_antiquotation ?loc:constrexpr.loc (substitution.(n))
         | None -> constrexpr
         end
      | _ -> Terms.Expr.map f constrexpr
    in
    f constrexpr
  in
  plug_holes parsed_term

let glob_constr_of_quasistring =
  let open Tactics in
  let lower = Terms.Glob_constr.of_constrexpr in
  let plug_hole ?loc glob_sign : antiquotation -> Terms.glob_constr = function
    | `Expr e -> Constrintern.intern_core WithoutTypeConstraint glob_sign e
    | `Preterm e -> e
    | `Constr c as antiquotation ->
       let open Glob_term in
       let genarg = GGenarg (GenConstr.Glb (wit_antiquotation, antiquotation)) in
       DAst.make ?loc genarg
  in
  let plug_holes glob_constr substitution =
    let rec f glob_constr =
      match DAst.get glob_constr with
      | Glob_term.GGenarg glb ->
         begin match get_glob wit_hole glb with
         | Some (Hole n, glob_sign) ->
            plug_hole ?loc:glob_constr.loc glob_sign (substitution.(n))
         | None -> glob_constr
         end
      | _ -> Terms.Glob_constr.map f glob_constr
    in
    return (f glob_constr)
  in
  generic_quasiparse ~lower ~plug_holes

let generic_constr_of_quasistring lower ?loc s =
  let open Tactics in
  let plug_hole : antiquotation -> Terms.constr Proofview.tactic = function
    | `Expr e -> Terms.Constr.of_constrexpr e
    | `Preterm e -> Terms.Constr.of_glob_constr e
    | `Constr c -> return c
  in
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let plug_holes constr substitution =
    let* substitutions = Tactics.of_array (Array.map plug_hole substitution) in
    let detect_hole evar =
      match Evd.evar_ident evar sigma with
      | Some fullpath ->
         let name = Libnames.basename fullpath in
         parse_hole_name (Id.to_string name)
      | None -> None
    in
    let map = EConstr.map sigma in
    let rec f constr =
      match EConstr.kind sigma constr with
      | Evar (e, _) ->
         begin match detect_hole e with
         | Some (Hole n) -> substitutions.(n)
         | None -> constr
         end
      | _ -> map f constr
    in
    return (f constr)
  in
  generic_quasiparse ~lower ~plug_holes ?loc s

let constr_of_quasistring =
  generic_constr_of_quasistring Terms.Constr.of_constrexpr

let open_constr_of_quasistring =
  generic_constr_of_quasistring Terms.Open_constr.of_constrexpr
