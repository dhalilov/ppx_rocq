(** Runtime support for term and goal pattern matching. *)

open Names
open Tactics
open Ltac2_plugin

(** {1 General matching algorithms} *)

let match_failure = (CErrors.UserError Pp.(str "No matching clauses for match."), Exninfo.null)
let match_failed () =
  let (e, info) = match_failure in
  Proofview.tclZERO ~info e

(** {2 Backtracking matches} *)

let multi_match cases =
  let rec multi_match error = function
  | [] ->
     let (e, info) = error in
     Proofview.tclZERO ~info e
  | case :: rest ->
     Proofview.tclOR case (fun error -> multi_match error rest)
  in multi_match match_failure cases

let one_match cases = Proofview.tclONCE (multi_match cases)

(** {2 Non-backtracking match} *)

let lazy_match cases =
  let rec lazy_match = function
    | [] -> match_failed ()
    | case :: rest ->
       let thunk = Proofview.tclUNIT (fun () -> case) in
       Proofview.tclOR thunk (fun _ -> lazy_match rest)
  in
  let* thunk = Proofview.tclONCE (lazy_match cases) in
  thunk ()

(** {1 Matching over terms} *)

type 'a case = pattern Proofview.tactic * 'a continuation
and pattern = Pattern.constr_pattern
and 'a continuation = substitution -> 'a Proofview.tactic
and substitution = Ltac_pretype.patvar_map

let match_term_case env sigma t (pattern, k) =
  let* pattern in
  try
    let subst = Constr_matching.matches env sigma pattern t in
    k subst
  with Constr_matching.PatternMatchingFailure ->
    match_failed ()

let match_term t ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let cases = List.map (fun case -> match_term_case env sigma t case) cases in
  one_match cases

let match_term' t ~cases =
  let* t in match_term t ~cases

let lazy_match_term t ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let cases = List.map (fun case -> match_term_case env sigma t case) cases in
  lazy_match cases

let lazy_match_term' t ~cases =
  let* t in lazy_match_term t ~cases

let multi_match_term t ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let cases = List.map (fun case -> match_term_case env sigma t case) cases in
  multi_match cases

let multi_match_term' t ~cases =
  let* t in multi_match_term t ~cases

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

let match_goal_case ~reverse env sigma goal (case, k) =
  let* rule = compile_case case in
  try
    let* (hypotheses, context, subst) = Tac2match.match_goal env sigma goal ~rev:reverse rule in
    let hyp_names = Array.of_list @@ List.map (fun (name, _, _) -> name) hypotheses in
    k hyp_names subst
  with Constr_matching.PatternMatchingFailure ->
    match_failed ()

let match_goal ?(reverse = false) goal ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let cases = List.map (fun case -> match_goal_case ~reverse env sigma goal case) cases in
  one_match cases

let lazy_match_goal ?(reverse = false) goal ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let cases = List.map (fun case -> match_goal_case ~reverse env sigma goal case) cases in
  lazy_match cases

let multi_match_goal ?(reverse = false) goal ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let cases = List.map (fun case -> match_goal_case ~reverse env sigma goal case) cases in
  multi_match cases
