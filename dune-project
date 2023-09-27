(lang dune 3.10)

(name reasonEulerProject)

(generate_opam_files true)

(source
 (github Leatherlord/reasonEulerProject))

(authors "Boyko Vladislav")

(maintainers "Boyko Vladislav")

(package
 (name reasonEulerProject)
 (synopsis "Functional Programming first assignment.")
 (description
  "Set of different solutions for 13th and 18th of Euler Problems.")
 (depends ocaml dune reason zarith ounit2))
