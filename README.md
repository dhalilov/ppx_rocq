# `ppx_rocq`: Syntax extensions for quoting Rocq terms in OCaml

`ppx_rocq` is a PPX rewriter that enables plugin writers to write Rocq terms using a simple quotation system, like so:

```ocaml
let nat_plus_assoc = [%constr "forall x y z : nat, (x + y) + z = x + (y + z)"] ;;
- : EConstr.t Proofview.tactic
```

`ppx_rocq` supports most quotations from Ltac2 (`%constr`, `%open_constr`, `%preterm`), an extra quotation for concrete syntax terms (`%expr`), and quotations for identifiers (`%ident`) and qualifiers (`%qualid`). Moreover, `ppx_rocq` also supports anti-quotations using the `%{…}` notation:

```ocaml
let lhs = [%expr "(x + y) + z"] in
let rhs = [%expr "x + (y + z)"] in
let nat_plus_assoc = [%constr "forall x y z : nat, %expr:{lhs} = %expr:{rhs}"] ;;
- : EConstr.t Proofview.tactic
```

`ppx_rocq` also includes pattern-matching extensions for matching over terms and goals:
```ocaml
match%rocq "1 + 1" with
| "?x + _" -> Proofview.tclUNIT x
| _ -> assert false ;;
- : EConstr.t Proofview.tactic
```

Check out [Camltac](https://github.com/epfl-systemf/camltac) for examples of `ppx_rocq` in the wild.

## Setup

Install `ppx_rocq` through `opam` using the following commands:
```bash
opam update
opam repo add rocq-released https://rocq-prover.github.io/opam/released/
opam pin add https://github.com/epfl-systemf/ppx_rocq.git
```

Then add `ppx_rocq` to the `preprocessing` field of your `library` or `executable` stanza:

```dune
(library
 (name my_library)
 ; …
 (preprocessing (pps ppx_rocq)))
```



