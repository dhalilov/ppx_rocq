(** Utilities for manipulating tactics. *)

open Proofview.Monad
let (let*) = Proofview.Monad.(>>=)

let env =
  let* goals = Proofview.Goal.goals in
  match goals with
  | [goal] -> let* goal in return (Proofview.Goal.env goal)
  | _ -> Proofview.tclENV

let evar_map =
  let* goals = Proofview.Goal.goals in
  match goals with
  | [goal] -> let* goal in return (Proofview.Goal.sigma goal)
  | _ -> Proofview.tclEVARMAP

let memoize t =
  let res = ref None in
  (* Enter tactic mode *)
  let* () = return () in
  match !res with
  | None ->
     let* v = t in
     res := Some v;
     return v
  | Some v ->
     return v

let of_array tacs =
  let* list = CArray.fold_right (fun t acc ->
    let* v = t in
    let* acc in
    return (v :: acc)
  ) tacs (return [])
  in return (Array.of_list list)
