(lang dune 3.22)

(generate_opam_files true)

(name ppx_rocq)
(authors "Dario Halilovic")
(maintainers "Dario Halilovic")
(homepage "https://github.com/epfl-systemf/ppx_rocq")
(bug_reports "https://github.com/epfl-systemf/ppx_rocq/issues")

(package
 (name ppx_rocq)
 (synopsis "PPX syntax extensions for quoting Rocq terms in OCaml")
 (depends
   rocq-runtime
   ppxlib))
