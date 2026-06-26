(** Runtime support for term and goal pattern matching. *)

open Names
open Tactics
open Ltac2_plugin

(** {1 General matching algorithms} *)

let no_match_error = (CErrors.UserError Pp.(str "No matching clauses for match."), Exninfo.null)

(** {2 Backtracking match} *)

let rec one_match ~error = function
  | [] ->
     let (e, info) = error in
     Proofview.tclZERO ~info e
  | tac :: rest ->
     try
       Proofview.tclOR tac (fun error -> one_match ~error rest)
     with Constr_matching.PatternMatchingFailure ->
       one_match ~error rest

let one_match cases = one_match ~error:no_match_error cases

(** {1 Matching over terms} *)

type 'a case = pattern Proofview.tactic * 'a continuation
and pattern = Pattern.constr_pattern
and 'a continuation = substitution -> 'a Proofview.tactic
and substitution = Ltac_pretype.patvar_map

let match_term t ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let case_tactic (pattern, k) =
    let* pattern in
    let subst = Constr_matching.matches env sigma pattern t in
    k subst
  in
  one_match (List.map case_tactic cases)

let match_term' t ~cases =
  let* t in match_term t ~cases

(** {1 Matching over goals} *)

type 'a goal_case = goal_pattern Proofview.tactic * (Id.t array -> 'a continuation)
and goal_pattern =
  { hypotheses: (pattern option * pattern) list;
    conclusion: pattern }

let compile_case case =
  let* case in
  let open Tac2match in
  let binder_to_pattern = function
    | Some pattern -> Some (MatchPattern pattern)
    | None -> None
  in
  let to_patterns (binder, typ) = binder_to_pattern binder, MatchPattern typ in
  let hypotheses = List.map to_patterns case.hypotheses in
  let conclusion = MatchPattern case.conclusion in
  Proofview.tclUNIT (hypotheses, conclusion)

let match_goal ?(reverse = false) goal ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let case_tactic (case, k) =
    let* rule = compile_case case in
    let* (hypotheses, context, subst) = Tac2match.match_goal env sigma goal ~rev:reverse rule in
    let hyp_names = Array.of_list @@ List.map (fun (name, _, _) -> name) hypotheses in
    k hyp_names subst
  in
  one_match (List.map case_tactic cases)
