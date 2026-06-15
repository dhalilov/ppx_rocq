(** Extensions using OCaml's [match] construct. *)

open Ppxlib

(** {1 Definitions} *)

(** Type of patterns in a term-matching case. *)
type term_pattern =
  | Pattern of string loc       (** e.g. ["?x + ?y"] *)
  | Wildcard of location option (** [_] *)

(** Type of patterns for matching hypotheses. *)
type hypothesis_pattern =
  { name: label loc;              (** Name of the hypothesis. *)
    binder_pattern: term_pattern; (** Pattern to apply to the binder. *)
    type_pattern: term_pattern    (** Pattern to apply to the type. *)
  }

(** Type of patterns in a goal-matching case. *)
type goal_pattern =
  { hypotheses: hypothesis_pattern list;
    conclusion: term_pattern }

(** Type of pattern-matching cases. *)
type 'pattern match_case =
  { pattern: 'pattern;  (** Pattern condition of the case. *)
    rhs: expression loc (** Expression to execute when the pattern matches. *)
  }

(** Type of scrutinees of the [match%constr] construct. *)
type match_term_scrutinee =
  | Literal of string loc    (** ["…"] *)
  | Expression of expression (** Any expression of type [constr]. *)

(** Type of term-matching expressions. *)
type match_term_expression =
  { scrutinee: match_term_scrutinee;     (** Scrutinee of the [match] expression. *)
    cases: term_pattern match_case list; (** List of cases of the [match]. *)
  }

(** Type of goal-matching expressions. *)
type match_goal_expression =
  { reverse: bool;                       (** Whether the match should be reversed or not. *)
    cases: goal_pattern match_case list; (** List of cases of the [match]. *)
  }

(** {2 AST patterns} *)

(** Term-matching patterns.

    Example: ["?x + ?y"], [_]. *)
let term_pattern (): (pattern, term_pattern -> 'a, 'a) Ast_pattern.t =
  let regular_pattern = Ast_pattern.(
      map ~f:(fun f s loc -> f (Pattern { txt = s; loc })) @@
        ppat_constant (pconst_string __ __ drop))
  in
  let wildcard_pattern = Ast_pattern.(
      map ~f:(fun f loc -> f (Wildcard (Some loc))) @@
        ppat_loc __ ppat_any)
  in
  Ast_pattern.alt regular_pattern wildcard_pattern

(** Term pattern-matching cases.

    Example: ["?x + ?y" -> …]. *)
let match_term_case =
  let match_term_case = Ast_pattern.(
      case
        ~lhs:(term_pattern ())
        ~guard:none
        ~rhs:__')
  in
  Ast_pattern.(map ~f:(fun f pattern rhs -> f { pattern; rhs }) match_term_case)

(** Term pattern-matching expressions.

    Example: [match%constr c with "?x + ?y" -> …]. *)
let match_term: (expression, match_term_expression -> expression, expression) Ast_pattern.t =
  let match_term = Ast_pattern.(pexp_match __' (many match_term_case)) in
  Ast_pattern.(map ~f:(fun f { txt = scrutinee; loc } cases ->
                   (* Check whether the scrutinee is a constant string. *)
                   let x = Ast_pattern.parse_res (estring __') loc scrutinee (fun x -> x) in
                   match x with
                   | Ok string -> f { scrutinee = Literal string; cases }
                   | Error _ -> f { scrutinee = Expression scrutinee; cases }) match_term)

(** Hypothesis pattern.

    Example: [h = "?x + ?y"], [h = _ :: "nat"]. *)
let hypothesis_pattern =
  let name = Ast_pattern.(loc (lident __')) in
  let binder = term_pattern () in
  let binder_with_type =
    Ast_pattern.(ppat_construct (lident (string "::"))
                   (some (pair nil (ppat_tuple (term_pattern () ^:: term_pattern () ^:: nil))))) in
  let rhs =
    Ast_pattern.(
      alt
        (map ~f:(fun f pattern -> f (~binder_pattern:pattern, ~type_pattern:(Wildcard None))) binder)
        (map ~f:(fun f binder typ -> f (~binder_pattern:binder, ~type_pattern:typ)) binder_with_type)
    )
  in
  let hypothesis_pattern = Ast_pattern.pair name rhs in
  Ast_pattern.(map ~f:(fun f name (~binder_pattern, ~type_pattern) -> f { name; binder_pattern; type_pattern }) hypothesis_pattern)

(** Pattern for a list of hypotheses.

    Example: [{ h1 = "?x + ?y"; h2 = "?z" }], [_]. *)
let hypotheses_pattern =
  let hypotheses_pattern = Ast_pattern.(ppat_record (many hypothesis_pattern) closed) in
  Ast_pattern.(alt hypotheses_pattern (map ~f:(fun f -> f []) ppat_any))

(** Goal pattern.

    Example: [{ h1 = "?x + ?y"; h2 = "?z" }, "?x = ?z"]. *)
let goal_pattern =
  let conclusion_pattern = term_pattern () in
  let goal_pattern = Ast_pattern.(ppat_tuple (hypotheses_pattern ^:: conclusion_pattern ^:: nil)) in
  Ast_pattern.(map ~f:(fun f hypotheses conclusion -> f { hypotheses; conclusion }) goal_pattern)

(** Case of a [match%goal] construct.

    Example: [{ h = "?x" :: "nat" }, "?x = ?x" -> …]. *)
let match_goal_case =
  let match_goal_case = Ast_pattern.(
      case
        ~lhs:goal_pattern
        ~guard:none
        ~rhs:__')
  in
  Ast_pattern.(map ~f:(fun f pattern rhs -> f { pattern; rhs }) match_goal_case)

(** [match%goal] construct.

    Example: [match%goal __ with { H = "?x" :: "nat" }, "?x = ?x" -> …]. *)
let match_goal : (expression, match_goal_expression -> expression, expression) Ast_pattern.t =
  let reverse = Ast_pattern.(
      alt
        (map ~f:(fun f -> f true) @@ pexp_ident (lident (string "reverse")))
        (map ~f:(fun f -> f false) @@ pexp_ident (lident (string "__"))))
  in
  let match_goal = Ast_pattern.(pexp_match reverse (many match_goal_case)) in
  Ast_pattern.(map ~f:(fun f reverse cases -> f { reverse; cases }) match_goal)

(** {1 Expansions} *)

(** {2 Term matching} *)

module Term = struct
  let expand_pattern ~loc pattern =
    match pattern with
    | Pattern { txt = pattern; loc = pattern_loc } ->
       let pattern_variables = Pattern_variable.find_all ~loc:pattern_loc pattern in
       let pattern_expr = Ast_builder.Default.estring ~loc:pattern_loc pattern in
       let rocq_loc = Ppx_utils.rocq_loc_of_loc pattern_loc in
       Hoister.hoist
         ~loc
         ~name:"pattern"
         [%expr
             Ppx_rocq_runtime.Tactics.memoize begin
               Ppx_rocq_runtime.Parsing.match_pattern_of_string
                 ~loc:[%e rocq_loc] [%e pattern_expr]
             end], pattern_variables
    | Wildcard wildcard_loc ->
       let loc = match wildcard_loc with Some loc -> loc | None -> loc in
       [%expr Proofview.tclUNIT Ppx_rocq_runtime.Terms.Pattern.wildcard], Pattern_variable.Set.empty

  let expand_rhs ~loc ~pattern_variables rhs =
    if Pattern_variable.Set.is_empty pattern_variables then
      [%expr fun _ -> [%e rhs.txt]]
    else
       let to_binding Pattern_variable.{ name } =
         let loc = name.loc in
         let name_expr = Ast_builder.Default.estring ~loc name.txt in
         name, [%expr Names.(Id.Map.find (Id.of_string [%e name_expr]) __subst)]
       in
       let bindings = List.map to_binding (Pattern_variable.Set.to_list pattern_variables) in
       [%expr fun __subst -> [%e Ppx_utils.with_let_bindings ~loc bindings rhs.txt]]

  let expand_case ~loc { pattern; rhs } =
    let pattern, pattern_variables = expand_pattern ~loc pattern in
    let rhs = expand_rhs ~loc ~pattern_variables rhs in
    [%expr ([%e pattern], [%e rhs])]

  let expand_match ~ctxt { scrutinee; cases } =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    (* TODO: Warn on any case after a wildcard, as they're unreachable. *)
    let cases = List.map (expand_case ~loc) cases in
    let cases = Ast_builder.Default.elist ~loc cases in
    match scrutinee with
    | Expression scrutinee ->
       [%expr Ppx_rocq_runtime.Pattern_matching.match_term [%e scrutinee] ~cases:[%e cases]]
    | Literal scrutinee ->
       let scrutinee = Ast_builder.Default.estring ~loc:scrutinee.loc scrutinee.txt in
       [%expr Ppx_rocq_runtime.Pattern_matching.match_term' [%constr [%e scrutinee]] ~cases:[%e cases]]
end

(** {2 Goal matching} *)

module Goal = struct
  let merge_pattern_variables l1 l2 =
    Pattern_variable.Set.union l1 l2

  let expand_hypothesis ~loc { name; binder_pattern; type_pattern } =
    let binder_pattern, binder_pattern_variables = Term.expand_pattern ~loc binder_pattern in
    let type_pattern, type_pattern_variables = Term.expand_pattern ~loc type_pattern in
    let expr = [%expr
                let* binder = [%e binder_pattern] in
                let* typ = [%e type_pattern] in
                Proofview.tclUNIT (binder, typ)] in
    expr, merge_pattern_variables binder_pattern_variables type_pattern_variables

  let expand_goal_pattern ~loc { hypotheses; conclusion } =
    let hypotheses, pattern_variables = List.split (List.map (expand_hypothesis ~loc) hypotheses) in
    let conclusion, conclusion_variables = Term.expand_pattern ~loc conclusion in
    let pattern_variables = List.fold_left merge_pattern_variables Pattern_variable.Set.empty pattern_variables in
    let pattern_variables = merge_pattern_variables pattern_variables conclusion_variables in
    [%expr
       let* hypotheses = Ppx_rocq_runtime.Tactics.of_list [%e Ast_builder.Default.elist ~loc hypotheses] in
       let* conclusion = [%e conclusion] in
       Proofview.tclUNIT (Ppx_rocq_runtime.Pattern_matching.{ hypotheses; conclusion })
    ], pattern_variables

  let expand_rhs ~loc ~pattern_variables ~hyps rhs =
    let rhs = Term.expand_rhs ~loc ~pattern_variables rhs in
    (* Bind hypothese names. *)
    let to_binding i hyp = hyp, [%expr __hyps.([%e Ast_builder.Default.eint ~loc i])] in
    let bindings = List.mapi to_binding hyps in
    [%expr fun __hyps -> [%e Ppx_utils.with_let_bindings ~loc bindings rhs]]

  let expand_case ~loc { pattern; rhs } =
    let hyps = List.map (fun { name } -> name) pattern.hypotheses in
    let pattern, pattern_variables = expand_goal_pattern ~loc pattern in
    let rhs = expand_rhs ~loc ~pattern_variables ~hyps rhs in
    [%expr ([%e pattern], [%e rhs])]

  let expand_match ~ctxt { reverse; cases } =
    let loc = Expansion_context.Extension.extension_point_loc ctxt in
    (* TODO: Warn on any case after a wildcard, as they're unreachable. *)
    let cases = List.map (expand_case ~loc) cases in
    let cases = Ast_builder.Default.elist ~loc cases in
    let reverse = Ast_builder.Default.ebool ~loc reverse in
    [%expr
        Proofview.Goal.enter_one (fun __goal -> Ppx_rocq_runtime.Pattern_matching.match_goal ~reverse:[%e reverse] (Proofview.Goal.concl __goal) ~cases:[%e cases])]

  let extension =
    Extension.V3.declare
      "goal"
      Extension.Context.expression
      Ast_pattern.(single_expr_payload match_goal)
      expand_match

  let rule = Context_free.Rule.extension extension
end
