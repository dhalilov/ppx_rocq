(** Placeholder values for parsing partial terms. *)

open Terms

type hole = private Hole of int (** [Hole n] represents [□ₙ] *)
(** Type of holes in the syntax AST ({!Terms.constrexpr}).

    A hole is a placeholder that is used instead of a concrete value in a term. Its main
    purpose is to enable parsing, globalization, and interpretation with partial information. *)

type glob_hole
(** Type of holes in globalized untyped terms ({!Terms.glob_constr}). *)

val make : ?loc:Loc.t -> int -> constrexpr
(** [make ?loc n] creates a term with a hole named [n]. *)

val fill_holes : (?loc:Loc.t -> hole -> constrexpr) -> constrexpr -> constrexpr
(** [fill_holes f c] replaces every hole [Hole n] in [c] by [f ?loc n], where
    [loc] is the location of the hole. *)

val fill_glob_holes : (?loc:Loc.t -> hole -> Genintern.glob_sign -> glob_constr) -> glob_constr -> glob_constr
(** [fill_glob_holes f c] replaces every hole [Hole n] in [c] by [f ?loc n glob_sign],
    where [loc] is the location of the hole and [glob_sign] is the captured
    globalization signature. *)
