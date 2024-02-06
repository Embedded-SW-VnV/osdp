(lang dune 3.11)

(name osdp)

(generate_opam_files false)

(source
 (github Embedded-SW-VnV/osdp))

(authors
 "Pierre Roux <pierre.roux@onera.fr>"
 "Pierre-Loïc Garoche <pierre-loic.garoche@ensac.fr>")

(maintainers "Pierre Roux")

(license LGPL-3.0-or-later)

(documentation https://github.com/Embedded-SW-VnV/osdp)

(package
 (name osdp)
 (synopsis "OCaml Interface to SDP solvers")
 (description "OSDP is an OCaml frontend library to semi-definite programming (SDP)
numerical optimization solvers. This package will be installed with
the solver CSDP. It will also be compiled with SDPA and Mosek support
if they can be found in the PATH.")
 (depends ocaml dune conf-autoconf zarith ocplib-simplex conf-csdp)
 (tags
  ("SDP" "verified SDP")))
