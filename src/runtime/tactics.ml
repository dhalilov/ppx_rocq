(** Utilities for manipulating tactics. *)

let (let*) = Proofview.Monad.(>>=)

let with_env t =
  let* goals = Proofview.Goal.goals in
  match goals with
  | [goal] ->
     let* goal in
     let env = Proofview.Goal.env goal in
     let sigma = Proofview.Goal.sigma goal in
     t env sigma
  | _ ->
     let* env = Proofview.tclENV in
     let* sigma = Proofview.tclEVARMAP in
     t env sigma
