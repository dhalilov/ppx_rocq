(** Support for pattern matching on terms and goals. *)

(** {1 Matching over terms} *)

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

val lazy_match_term : EConstr.constr -> cases:'a case list -> 'a Proofview.tactic
(** [lazy_match_term t ~cases] performs pattern matching on term [t], committing
    to the first branch that matches. *)

val lazy_match_term' : EConstr.constr Proofview.tactic -> cases:'a case list -> 'a Proofview.tactic
(** [lazy_match_term' t ~cases] is a shorthand for [let* t in lazy_match_term t ~cases]. *)

val multi_match_term : EConstr.constr -> cases:'a case list -> 'a Proofview.tactic
(** [multi_match_term t ~cases] performs pattern matching on term [t] with backtracking.
    If a tactic fails after the [match], the next branch is tried. *)

val multi_match_term' : EConstr.constr Proofview.tactic -> cases:'a case list -> 'a Proofview.tactic
(** [multi_match_term' t ~cases] is a shorthand for [let* t in multi_match_term t ~cases]. *)

(** {1 Matching over goals} *)

type 'a goal_case = goal_pattern Proofview.tactic * (Names.Id.t array -> 'a continuation)

and goal_pattern = {
  hypotheses : (pattern option * pattern) list;
  conclusion : pattern;
}

val match_goal : ?reverse:bool -> Evd.econstr -> cases: 'a goal_case list -> 'a Proofview.tactic
(** [match_goal ?reverse t ~cases] performs goal matching on [t] with backtracking. *)

val lazy_match_goal : ?reverse:bool -> Evd.econstr -> cases: 'a goal_case list -> 'a Proofview.tactic
(** [lazy_match_goal ?reverse t ~cases] performs goal matching on [t],
    committing to the first branch that succeeds. *)

val multi_match_goal : ?reverse:bool -> Evd.econstr -> cases: 'a goal_case list -> 'a Proofview.tactic
(** [multi_match_goal ?reverse t ~cases] performs goal matching on [t]. If an
    expression fails after the [match], the next branch is tried. *)
