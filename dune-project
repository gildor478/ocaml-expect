(lang dune 2.9)
(name expect)

(explicit_js_mode)

(generate_opam_files)

(source (github gildor478/ocaml-expect))
(license "LGPL-2.1-or-later WITH OCaml-LGPL-linking-exception")
(authors "Sylvain Le Gall")
(maintainers "Sylvain Le Gall <sylvain+ocaml@le-gall.net>")
(documentation "https://gildor478.github.io/ocaml-expect/")

(package
  (name expect)
  (synopsis "Simple implementation of 'expect' to help building unitary testing of interactive program")
  (description
"\| You can match the question using a regular expression or a timeout.
"\|
"\| See the Expect manual for more information:
"\| http://expect.nist.gov/
 )
  (depends
    base-unix
    (re (>= 1.12.0))
    (batteries (>= 3.8.0))
    (ounit2 (and (>= 2.0.0) :with-test))
    (ocaml (>= 4.14))))
