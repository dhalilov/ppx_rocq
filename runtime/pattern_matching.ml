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
