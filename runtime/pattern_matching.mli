(** Support for pattern matching on terms and goals. *)

(** {1 Term matching} *)

type 'a case = pattern Proofview.tactic * 'a continuation
(** Type of cases in term matching. *)

and pattern = Pattern.constr_pattern
(** Type of patterns in term pattern matching. *)

and 'a continuation = substitution -> 'a Proofview.tactic
(** Type of continuations in a pattern matching branch. *)

and substitution = Ltac_pretype.patvar_map
(** Type of substitutions. *)

val match_term : EConstr.constr -> cases:'a case list -> 'a Proofview.tactic
(** [match_term t ~cases] performs pattern matching on term [t] with
    backtracking. *)

val match_term' : EConstr.constr Proofview.tactic -> cases:'a case list -> 'a Proofview.tactic
(** [match_term' t ~cases] is a shorthand for [let* t in match_term t ~cases]. *)

type 'a goal_case = goal_pattern Proofview.tactic * 'a continuation

and goal_pattern = {
  hypotheses : (Names.variable * pattern * pattern) list;
  conclusion : pattern;
}

val match_goal : ?reverse:bool -> Evd.econstr -> cases: 'a goal_case list -> 'a Proofview.tactic
(** [match_goal ?reverse t ~cases] performs goal matching on [t] with backtracking. *)
