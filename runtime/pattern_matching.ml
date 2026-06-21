(** Runtime support for term and goal pattern matching. *)

open Tactics

type 'a case = pattern Proofview.tactic * 'a continuation
and pattern = Pattern.constr_pattern
and 'a continuation = substitution -> 'a Proofview.tactic
and substitution = Ltac_pretype.patvar_map

let matching_error = (CErrors.UserError Pp.(str "No matching clauses for match."), Exninfo.null)

let match_term t ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let rec test_cases (e, info) = function
    | [] -> Proofview.tclZERO ~info e
    | (pattern, k) :: rest ->
       let* pattern in
       try
         let subst = Constr_matching.matches env sigma pattern t in
         let tac = k subst in
         Proofview.tclOR tac (fun e -> test_cases e rest)
       with Constr_matching.PatternMatchingFailure ->
         test_cases (e, info) rest
  in
  test_cases matching_error cases

let match_term' t ~cases =
  let* t in match_term t ~cases

type 'a goal_case = goal_pattern Proofview.tactic * (Names.Id.t array -> 'a continuation)
and goal_pattern =
  { hypotheses: (pattern option * pattern) list;
    conclusion: pattern }

open Ltac2_plugin

let compile_case case =
  let* case in
  let open Tac2match in
  let hyp_to_ltac2 (binder, typ) =
    let binder =
      match binder with
      | Some pattern -> Some (MatchPattern pattern)
      | None -> None
    in binder, MatchPattern typ
  in
  let hypotheses = List.map hyp_to_ltac2 case.hypotheses in
  let conclusion = MatchPattern case.conclusion in
  Proofview.tclUNIT (hypotheses, conclusion)

let match_goal ?(reverse = false) t ~cases =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let cases = List.map (fun (case, k) -> compile_case case, k) cases in
  let rec test_cases (e, info) = function
    | [] -> Proofview.tclZERO ~info e
    | (rule, k) :: rest ->
       let* rule in
       try
         let* (hypotheses, context, subst) = Ltac2_plugin.Tac2match.match_goal env sigma t ~rev:reverse rule in
         let hyp_names = Array.of_list @@ List.map (fun (name, _, _) -> name) hypotheses in
         let tac = k hyp_names subst in
         Proofview.tclOR tac (fun e -> test_cases e rest)
       with Constr_matching.PatternMatchingFailure ->
         test_cases (e, info) rest
  in
  test_cases matching_error cases
