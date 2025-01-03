Expect - expect-like framework
==============================

[![OCaml-CI Build Status](https://img.shields.io/endpoint?url=https://ci.ocamllabs.io/badge/gildor478/ocaml-expect/master&logo=ocaml)](https://ci.ocamllabs.io/github/gildor478/ocaml-expect)

This is a simple implementation of `expect` to help building unitary testing
of interactive program.

It helps to receive question and send answers from an interactive process.
You can match the question using a regular expression (Str). You can also use
a timeout to ensure that the process answer in time.

See the [Expect manual](http://expect.nist.gov/) for more information and
example.

Installation
------------

The recommended way to install expect is via the [opam package manager][opam]:

```sh
$ opam install expect
```

Documentation
-------------

API documentation is
[available online](https://gildor478.github.io/ocaml-expect).
