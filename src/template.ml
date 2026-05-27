(** Methods for parsing template strings with antiquotations. *)

open Ppxlib

type 'a fragment =
  | Literal of string
  | Antiquotation of 'a

let rec parse_from ~from string =
  match Antiquotation.find ~from string with
  | Some ({ percent; closing_brace } as antiquotation) ->
     let { txt = literal } = Located_string.substring ~from ~until:percent string in
     let rest =
       match closing_brace with
       | Some closing_brace -> parse_from ~from:(Located_string.advance "}" closing_brace) string
       | None -> []
     in
     let result = Antiquotation antiquotation :: rest in
     if literal <> "" then Literal literal :: result else result
  | None ->
     let { txt = literal } = Located_string.substring ~from string in
     if literal <> "" then [Literal literal] else []

let parse ~loc string =
  let start = Located_string.start_position loc in
  parse_from ~from:start { txt = string; loc }

type interpretation_result =
  | Constant of string
  | Antiquoted_value of expression
  | Unknown_antiquotation of { warning: string loc; antiquotation: string }

let interpret_fragment ~default ~explicit fragment =
  match fragment with
  | Literal lit -> Constant lit
  | Antiquotation antiquotation ->
     match Antiquotation.interpret_expression ~default ~explicit antiquotation with
     | Ok value -> Antiquoted_value value
     | Error warning -> Unknown_antiquotation { warning; antiquotation = Antiquotation.to_string antiquotation }

let interpret ~loc ~default ~explicit fragments =
  let rec interpret fragments next_id =
    match fragments with
    | [] ->
       ~template:"", ~antiquotations:[], ~warnings:[]
    | fragment :: fragments' ->
       match interpret_fragment ~default ~explicit fragment with
       | Constant lit ->
          let ~template, ~antiquotations, ~warnings = interpret fragments' next_id in
          ~template:(lit ^ template), ~antiquotations, ~warnings
       | Antiquoted_value expr ->
          let ~template, ~antiquotations, ~warnings = interpret fragments' (next_id + 1) in
          ~template:("%{" ^ string_of_int next_id ^ "}" ^ template), ~antiquotations:(expr :: antiquotations), ~warnings
       | Unknown_antiquotation { warning; antiquotation } ->
          let ~template, ~antiquotations, ~warnings = interpret fragments' next_id in
          ~template:(antiquotation ^ template), ~antiquotations, ~warnings:(warning :: warnings)
  in
  let ~template, ~antiquotations, ~warnings = interpret fragments 0 in
  let runtime_template = Ast_builder.Default.estring ~loc template in
  let runtime_template = Ast_diagnostics.warn' warnings runtime_template in
  runtime_template, antiquotations
