## v0.1.0 - 2025-01-03

  * Replace ocaml-pcre by ocaml-re. The reason is that pcre-ocaml depends on an
    [obsolete library](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1000004) on
    Debian. (thhanks to glondu@)
  * Migrate build system to dune.

## v0.0.6 - 2017-12-31

  * Version 0.0.6
  * Fix compatibility issues with OCaml 4.06.0.

## v0.0.5 - 2014-02-10

  * Version 0.0.5
  * Be more verbose about end of file
  * Improve handling of "Broken pipe"

## v0.0.4 - 2013-10-26

  * Version 0.0.4
  * Adapt function to allow verbose output (OUnit2 improvement)
  * Cleanup

## v0.0.3 - 2012-06-14

  * Version 0.0.3
  * Regenerate setup.ml with oasis v0.3.0~rc6
  * Fix test to use qa.byte

## v0.0.2 - 2010-09-30

  * Version 0.0.2
  * Add a ~fmatch to expect, to dynamically define expect results
  * Allow to pass environment and redirect stderr to spawn
  * Add `Suffix/`Prefix/`Contains tests
  * Separate Str, add Pcre, create findlib packages expect.str and
    expect.pcre

## v0.0.1 - 2010-09-07

  * Initial version 0.0.1
