(** Methods for parsing template strings with antiquotations. *)

open Ppxlib

(** Type of template fragments. *)
type 'a fragment =
  | Literal of string   (** A literal string inside the template. *)
  | Antiquotation of 'a (** An antiquotation, e.g., [%{…}]. *)

val parse : loc:location -> string -> Antiquotation.t fragment list
(** [parse ~loc s] parses the template string [s] by finding all antiquotations
    (of the form [%{…}] or [%kind:{…}]) in [s].

    The contents of each antiquotation is left uninterpreted, as each quotation may
    allow a different set of antiquotations. Validation is performed by the [interpret]
    function.

    Currently, the implementation does not allow the [}] character to appear inside
    an antiquotation. *)

val interpolate : loc:location -> Antiquotation.t fragment list -> expression
(** [interpolate ~loc template] interprets the list of template fragments as string interpolation,
    i.e., every antiquotation is a string.

    For example, [interpolate ~loc [Literal "foo_"; Antiquotation { kind = Default; expression = "x" }]]
    returns [[%expr String.concat "" ["foo_"; x]]].
 *)

val interpret :
  loc:location ->
  default:(loc:location -> expression -> expression) ->
  explicit:((string * (loc:location -> expression -> expression)) list) ->
  Antiquotation.t fragment list ->
  expression * expression list
(** [interpret ~loc ~default ~explicit fragments] interprets the list of template fragments to a
    runtime expression and a list of antiquoted values, where antiquotations are interpreted according
    to their kind.

    For example, [interpret [Literal "1 + "; Antiquotation { kind = "constr"; expression = "x" }]] returns
    the expression [[%expr "1 + %{0}"]] along with the list [[[%expr x : constr]]]. Note that the runtime template
    uses integers starting from 0, as they are easily recognized by Rocq's parser.

    @see {!Antiquotation.interpret_expression}
 *)
