(** Runtime support for term and goal pattern matching. *)

open Names
open Tactics
open Ltac2_plugin
open Tac2match

let return = Proofview.tclUNIT

(** {1 General matching algorithms} *)

let match_failure = (CErrors.UserError Pp.(str "No matching clauses for match."), Exninfo.null)
let match_failed () =
  let (e, info) = match_failure in
  Proofview.tclZERO ~info e

(** {1 Matching over terms} *)

type 'a case = pattern Proofview.tactic * 'a continuation
and pattern = Tac2match.match_pattern
and 'a continuation = Constr_matching.context -> substitution -> 'a Proofview.tactic
and substitution = Ltac_pretype.patvar_map

let match_pattern t pattern =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  try
    let subst = Constr_matching.matches env sigma pattern t in
    return subst
  with Constr_matching.PatternMatchingFailure ->
    match_failed ()

let match_context t pattern =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let rec values_of_stream s =
    match IStream.peek s with
    | Nil -> match_failed ()
    | Cons (Constr_matching.{ m_sub = (_, subst); m_ctx }, s) ->
       Proofview.tclOR
         (return (m_ctx, subst))
         (fun _ -> values_of_stream s)
  in
  let matches = Constr_matching.match_subterm env sigma (Id.Set.empty, pattern) t in
  values_of_stream matches

let match_term t pattern k =
  match pattern with
  | MatchPattern pattern ->
     let context = Constr_matching.empty_context in
     let* subst = match_pattern t pattern in
     return (fun () -> k context subst)
  | MatchContext pattern ->
     let* (context, subst) = match_context t pattern in
     return (fun () -> k context subst)

let multi_match_term t cases =
  let rec interp error cases =
    match cases with
    | [] -> let (e, info) = error in Proofview.tclZERO ~info e
    | (pattern, k) :: cases ->
       let* pattern in
       Proofview.tclOR
         (let* f = match_term t pattern k in f ())
         (fun e -> interp e cases)
  in
  interp match_failure cases

let lazy_match_term t cases =
  let rec interp error cases =
    match cases with
    | [] -> let (e, info) = error in Proofview.tclZERO ~info e
    | (pattern, k) :: cases ->
       let* pattern in
       Proofview.tclOR
         (match_term t pattern k)
         (fun e -> interp e cases)
  in
  let* f = Proofview.tclONCE (interp match_failure cases) in
  f ()

let match_term t cases = Proofview.tclONCE (multi_match_term t cases)

(** {1 Matching over goals} *)

type 'a goal_case = match_rule Proofview.tactic * ((Id.t * context * context) array -> 'a continuation)

let context_or_empty context =
  match context with
  | Some context -> context
  | None -> Constr_matching.empty_context

let format_hyp (name, binder_context, typ_context) =
  let binder_context = context_or_empty (Option.flatten binder_context) in
  let typ_context = context_or_empty typ_context in
  name, binder_context, typ_context

let match_goal_pattern ~reverse concl goal_pattern =
  let* env = Tactics.env in
  let* sigma = Tactics.evar_map in
  let* (hypotheses, concl_context, subst) = Tac2match.match_goal env sigma concl ~rev:reverse goal_pattern in
  let hyps = Array.of_list (List.map format_hyp hypotheses) in
  let concl_context = context_or_empty concl_context in
  return (hyps, concl_context, subst)

let match_goal ~reverse concl goal_pattern k =
  let* (hyps, concl_context, subst) = match_goal_pattern ~reverse concl goal_pattern in
  return (fun () -> k hyps concl_context subst)

let multi_match_goal ?(reverse = false) concl goal_patterns =
  let rec interp error cases =
    match cases with
    | [] -> let (e, info) = error in Proofview.tclZERO ~info e
    | (goal_pattern, k) :: goal_patterns ->
       let* goal_pattern in
       Proofview.tclOR
         (let* f = match_goal ~reverse concl goal_pattern k in f ())
         (fun e -> interp e goal_patterns)
  in
  interp match_failure goal_patterns

let lazy_match_goal ?(reverse = false) concl goal_patterns =
  let rec interp error cases =
    match cases with
    | [] -> let (e, info) = error in Proofview.tclZERO ~info e
    | (goal_pattern, k) :: goal_patterns ->
       let* goal_pattern in
       Proofview.tclOR
         (match_goal ~reverse concl goal_pattern k)
         (fun e -> interp e goal_patterns)
  in
  let* f = Proofview.tclONCE (interp match_failure goal_patterns) in
  f ()

let match_goal ?(reverse = false) concl goal_patterns =
  Proofview.tclONCE (multi_match_goal ~reverse concl goal_patterns)

