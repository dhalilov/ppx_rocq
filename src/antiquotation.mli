(** Methods for parsing antiquotations. *)

open Ppxlib

(** The kind of an antiquotation is an optional string specifying the type of the
    antiquoted expression. *)
type kind =
  | Default                      (** [%{…}] *)
  | Explicit of Located_string.t (** [%kind:{…}] *)

(** Type of parsed antiquotations. *)
type t = private
  { percent : Located_string.position;
    (** Position of the percent character. *)

    kind : kind;
    (** Kind of the antiquotation. *)

    opening_brace : Located_string.position;
    (** Position of the opening brace [{]. *)

    expression : Located_string.t;
    (** Antiquoted expression. *)

    closing_brace : Located_string.position option;
    (** Position of the closing brace [}], or [None] if the antiquotation is unclosed. *)
  }

val find : from:Located_string.position -> Located_string.t -> t option
(** [find ~from string] finds and parses the first antiquotation that occurs
    after [from] in [string]. *)

val interpret_expression : default:(loc:location -> expression -> expression) ->
                           explicit:((string * (loc:location -> expression -> expression)) list) ->
                           t ->
                           (expression, string loc) result
(** [interpret_expression ~default ~explicit antiquotation] parses and interprets the expression
    of the antiquotation according to its kind:
    - If the kind is [Default], the [default] function is applied to the parsed expression;
    - If the kind is [Explicit], the corresponding interpretation function is searched in [explicit].
      If there is such a function, it is applied to the parsed expression; otherwise an error message is returned.
 *)

val to_string : t -> string
(** [to_string antiquotation] returns the textual representation of the antiquotation.
    Used for printing purposes. *)
