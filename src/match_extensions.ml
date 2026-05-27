(** Extensions using OCaml's [match] construct. *)

open Ppxlib

(** {1 Definitions} *)

(** Type of patterns in a term-matching case. *)
type term_pattern =
  | Pattern of string loc (** e.g. ["?x + ?y"] *)
  | Wildcard of location  (** [_] *)

(** Type of patterns for matching hypotheses. *)
type hypothesis_pattern =
  { name: label loc;
    pattern: term_pattern }

(** Type of patterns in a goal-matching case. *)
type goal_pattern =
  { hypotheses: hypothesis_pattern list;
    conclusion: term_pattern }

(** Type of pattern-matching cases. *)
type 'pattern match_case =
  { pattern: 'pattern;  (** Pattern condition of the case. *)
    rhs: expression loc (** Expression to execute when the pattern matches. *)
  }

(** Type of term-matching expressions. *)
type match_term_expression =
  { scrutinee: expression;               (** Scrutinee of the [match] expression. *)
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
      map ~f:(fun f loc -> f (Wildcard loc)) @@
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
  let match_term = Ast_pattern.(pexp_match __ (many match_term_case)) in
  Ast_pattern.(map ~f:(fun f scrutinee cases -> f { scrutinee; cases }) match_term)

(** Hypothesis pattern.

    Example: [H = "?x + ?y"]. *)
let hypothesis_pattern =
  let name = Ast_pattern.(loc (lident __')) in
  let pattern = term_pattern () in
  let hypothesis_pattern = Ast_pattern.pair name pattern in
  Ast_pattern.(map ~f:(fun f name pattern -> f { name; pattern }) hypothesis_pattern)

(** Pattern for a list of hypotheses.

    Example: [{ H1 = "?x + ?y"; H2 = "?z" }]. *)
let hypotheses_pattern =
  Ast_pattern.(ppat_record (many hypothesis_pattern) closed)

(** Goal pattern.

    Example: [{ H1 = "?x + ?y"; H2 = "?z" }, "?x = ?z"]. *)
let goal_pattern =
  let conclusion_pattern = term_pattern () in
  let goal_pattern = Ast_pattern.(ppat_tuple (hypotheses_pattern ^:: conclusion_pattern ^:: nil)) in
  Ast_pattern.(map ~f:(fun f hypotheses conclusion -> f { hypotheses; conclusion }) goal_pattern)

let match_goal_case =
  let match_goal_case = Ast_pattern.(
      case
        ~lhs:goal_pattern
        ~guard:none
        ~rhs:__')
  in
  Ast_pattern.(map ~f:(fun f pattern rhs -> f { pattern; rhs }) match_goal_case)

let match_goal : (expression, match_goal_expression -> expression, expression) Ast_pattern.t =
  let reverse = Ast_pattern.(
      alt
        (map ~f:(fun f -> f true) @@ pexp_ident (lident (string "reverse")))
        (map ~f:(fun f -> f false) pexp_unreachable))
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
    | Wildcard loc -> [%expr Proofview.tclUNIT Ppx_rocq_runtime.Terms.Pattern.wildcard], []

  let expand_rhs ~loc ~pattern_variables rhs =
    match pattern_variables with
    | [] -> [%expr fun _ -> [%e rhs.txt]]
    | _ ->
       let to_binding Pattern_variable.{ name } =
         let loc = name.loc in
         let name_expr = Ast_builder.Default.estring ~loc name.txt in
         name, [%expr Names.(Id.Map.find (Id.of_string [%e name_expr]) __subst)]
       in
       let bindings = List.map to_binding pattern_variables in
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
    [%expr Ppx_rocq_runtime.Pattern_matching.match_term [%e scrutinee] ~cases:[%e cases]]
end

(** {2 Goal matching} *)

module Goal = struct

end
