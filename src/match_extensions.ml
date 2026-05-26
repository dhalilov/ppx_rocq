(** Extensions using OCaml's [match] construct. *)

open Ppxlib

type payload =
  | String of string loc
  | Match of
      { scrutinee: expression;
        cases: match_case list
      }
and match_case =
  { lhs: string option loc; (** Pattern string, or [None] if the case is catch-all. *)
    rhs: expression loc     (** Expression to execute when the pattern matches. *)
  }

let expand_pattern_var { txt = id; loc } =
  let id_expr = Ast_builder.Default.estring ~loc id in
  { txt = id; loc }, [%expr Names.Id.Map.find (Names.Id.of_string [%e id_expr]) subst]

let find_all_pattern_vars ~loc pattern =
  let rec find_all_in_stream stream =
    let _, stream = CharStream.span ~pattern:{|@?\?|} stream in
    if CharStream.is_empty stream then []
    else
      let prefix = Str.matched_string stream.contents in
      let loc_start = stream.pos in
      let stream = CharStream.advance ~n:(String.length prefix) stream in
      (* Attempt to parse an identifier from the rest of the stream *)
      try
        let rest = CharStream.take_all stream in
        let id = Names.Id.to_string (Ppx_rocq_runtime.Parsing.parse Procq.Prim.ident rest) in
        let stream = CharStream.advance ~n:(String.length id) stream in
        let loc_end = stream.pos in
        let loc = { loc_start; loc_end; loc_ghost = false } in
        { txt = id; loc } :: find_all_in_stream stream
      with Gramlib.Grammar.ParseError _ ->
        (* Failed to parse an identifier: most likely a false positive. *)
        find_all_in_stream stream
  in
  let stream = CharStream.of_string ~loc pattern in
  find_all_in_stream stream

let expand_case ~loc { lhs = { txt = lhs; loc = lhs_loc }; rhs = { txt = rhs; loc = rhs_loc } } =
  let rocq_loc = Ppx_utils.rocq_loc_of_loc lhs_loc in
  let lhs, bindings =
    match lhs with
    | Some pattern ->
       (** Approximate the set of metavariables used by [pattern] at
           compilation-time by finding all occurrences of {v ?x v} or {v @?x v}.
           Note that parsing is not available since PPX runs as a process separated from Rocq,
           and therefore patterns such as {v ?x + ?y v} would fail to parse correctly. *)
       let bindings = find_all_pattern_vars ~loc:lhs_loc pattern in
       let bindings = List.map expand_pattern_var bindings in
       let pattern_expr = Ast_builder.Default.estring ~loc:lhs_loc pattern in
       Hoister.hoist ~loc ~name:"pattern" [%expr Ppx_rocq_runtime.Tactics.memoize (Ppx_rocq_runtime.Parsing.match_pattern_of_string ~loc:[%e rocq_loc] [%e pattern_expr])], bindings
    | None -> Hoister.hoist ~loc ~name:"wildcard" [%expr Ppx_rocq_runtime.Tactics.memoize (Ppx_rocq_runtime.Parsing.match_pattern_of_string ~loc:[%e rocq_loc] "_")], []
  in
  let rhs =
    match bindings with
    | [] -> [%expr fun _ -> [%e rhs]]
    | _ -> [%expr fun subst -> [%e Ppx_utils.with_let_bindings ~loc bindings rhs]]
  in
  [%expr ([%e lhs], [%e rhs])]

let expand_match ~ctxt ~scrutinee ~cases =
  let loc = Expansion_context.Extension.extension_point_loc ctxt in
  let cases = List.map (expand_case ~loc) cases in
  let cases = Ast_builder.Default.elist ~loc cases in
  [%expr Ppx_rocq_runtime.Pattern_matching.match_term [%e scrutinee] ~cases:[%e cases]]

let match_pattern: (expression, payload -> expression, expression) Ast_pattern.t =
  let regular_pattern = Ast_pattern.(
      map ~f:(fun f s loc -> f { txt = Some s; loc }) @@
        ppat_constant (pconst_string __ __ drop))
  in
  let any_pattern = Ast_pattern.(
      map ~f:(fun f loc -> f { txt = None; loc }) @@
        ppat_loc __ ppat_any)
  in
  let case_pattern = Ast_pattern.(
      case
        ~lhs:(alt regular_pattern any_pattern)
        ~guard:(none)
        ~rhs:(__'))
  in
  let case_pattern = Ast_pattern.(map ~f:(fun f lhs rhs -> f { lhs; rhs }) case_pattern) in
  let match_pattern = Ast_pattern.(pexp_match __ (many case_pattern)) in
  Ast_pattern.(map ~f:(fun f scrutinee cases -> f (Match { scrutinee; cases })) match_pattern)
