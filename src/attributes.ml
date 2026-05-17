(** Handling of top-level attributes for compiler options. *)

open Ppxlib
open Sexplib.Std

(** [[@@@compiler]] floating attribute, used to adds flags to the compiler. *)
module Compiler_options = struct
  (** Property for compiler options, set through the [[@@@compiler]] floating
      attribute. *)
  module Property =
    Driver.Create_file_property
      (struct let name = "compiler_options" end)
      (struct type t = string list [@@deriving sexp] end)

  let expand ~ctxt options =
    Property.set options;
    []

  let pattern =
    let option = Ast_pattern.(estring __) in
    let options = Ast_utils.comma_separated option in
    Ast_pattern.single_expr_payload options

  let attribute =
    Attribute.Floating.(declare "mltac.compiler" Context.structure_item pattern Fun.id)

  let rule =
    Context_free.Rule.attr_str_floating_expect_and_expand attribute expand
end

module Library_options = struct
  module Property =
    Driver.Create_file_property
      (struct let name = "libraries" end)
      (struct type t = string list [@@deriving sexp] end)

  let () = Findlib.init ()

  (** Check that the library exists using [ocamlfind], otherwise embed an error node. *)
  let check_lib { txt = lib; loc } =
    let packages = Findlib.list_packages' () in
    if List.mem lib packages then lib
    else
      (* TODO: Embed the error instead of raising. It currently does not work
         because PPX complains about a missing [@@@ppxlib.inline.end] *)
      match Spellcheck.spellcheck packages lib with
      | Some suggestion ->
         Location.raise_errorf ~loc "Could not find package %s.\n%s" lib suggestion
      | None ->
         Location.raise_errorf ~loc "Could not find package %s." lib

  let expand ~ctxt libs =
    let libs = List.map check_lib libs in
    Property.set libs;
    []

  let pattern =
    let library = Ast_pattern.(estring __') in
    let libraries = Ast_utils.comma_separated library in
    Ast_pattern.single_expr_payload libraries

  let attribute =
    Attribute.Floating.(declare "mltac.using" Context.structure_item pattern Fun.id)

  let rule =
    Context_free.Rule.attr_str_floating_expect_and_expand attribute expand
end

(**/**)

let () =
  Ppxlib.Driver.register_transformation
    ~rules:[
      Compiler_options.rule;
      Library_options.rule
    ]
    "mltac.attributes"
