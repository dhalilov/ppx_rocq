# `ppx_rocq`: PPX syntax extensions for quoting Rocq terms in OCaml

`ppx_rocq` is a PPX rewriter that enables plugin writers to write Rocq terms using a simple quotation system, like so:

```ocaml
let nat_plus_assoc = [%constr "forall x y z : nat, (x + y) + z = x + (y + z)"] ;;
- : EConstr.t Proofview.tactic
```

`ppx_rocq` supports all quotations from Ltac2 (`%constr`, `%preterm`), an extra quotation for concrete syntax terms (`%expr`), as well as additional quotations for identifiers (`%ident`), qualifiers (`%qualid`), etc. Moreover, `ppx_rocq` also supports anti-quotations using the `%{…}` notation:

```ocaml
let lhs = [%expr "(x + y) + z"] in
let rhs = [%expr "x + (y + z)"] in
let nat_plus_assoc = [%constr "forall x y z : nat, %expr:{lhs} = %expr:{rhs}"] ;;
- : EConstr.t Proofview.tactic
```

## Setup

To use `ppx_rocq`, run `dune install` on this repository:
```bash
git clone https://github.com/epfl-systemf/ppx_rocq.git
cd ppx_rocq
dune build
dune install
```

Then add `ppx_rocq` to the `preprocessing` stanza of your Dune file:

```dune
(library
 (name my_library)
 ; …
 (preprocessing (pps ppx_rocq)))
```



