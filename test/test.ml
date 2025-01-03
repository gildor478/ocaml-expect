(******************************************************************************)
(* ocaml-expect: Expect-like framework                                        *)
(*                                                                            *)
(* Copyright (C) 2013, Sylvain Le Gall                                        *)
(* Copyright (C) 2010, OCamlCore SARL                                         *)
(*                                                                            *)
(* This library is free software; you can redistribute it and/or modify it    *)
(* under the terms of the GNU Lesser General Public License as published by   *)
(* the Free Software Foundation; either version 2.1 of the License, or (at    *)
(* your option) any later version, with the OCaml static compilation          *)
(* exception.                                                                 *)
(*                                                                            *)
(* This library is distributed in the hope that it will be useful, but        *)
(* WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY *)
(* or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more         *)
(* details.                                                                   *)
(*                                                                            *)
(* You should have received a copy of the GNU Lesser General Public License   *)
(* along with this library; if not, write to the Free Software Foundation,    *)
(* Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA              *)
(******************************************************************************)

(** Run tests for Expect
    @author Sylvain Le Gall
  *)

open OUnit2
open Expect
open TestCommon

let timeout = 0.1

let qa = Conf.make_exec "qa"


let with_qa test_ctxt ?use_stderr ?(exit_code=Unix.WEXITED 0) suite f =
  let _, real_exit_code =
    with_spawn
      ~verbose:true
      ~verbose_output:(logf test_ctxt `Info "%s")
      ?use_stderr
      ~timeout:(Some timeout) (qa test_ctxt) [|suite|]
      (fun t () -> f t) ()
  in
    assert_equal
      ~msg:"Exit code"
      ~printer:printer_exit_code
      exit_code
      real_exit_code

let tests =
  "Expect" >:::
  [
    "std-str" >::
    (fun test_ctxt ->
       with_qa test_ctxt "std"
         (fun t ->
            assert_bool
              "Ask name"
              (ExpectStr.expect t
                 [`Regexp
                    (Str.regexp (Str.quote "What's your name? ")),
                  true]
                 false);

            send t "Sylvain\n";

            assert_bool
              "Answer hello"
              (ExpectStr.expect t
                 [`Regexp
                    (Str.regexp (Str.quote "Hello Sylvain")),
                  true]
                 false)));

    "std-pcre" >::
    (fun test_ctxt ->
       with_qa test_ctxt "std"
         (fun t ->
            assert_bool
              "Ask name"
              (ExpectPcre.expect t
                 [`Pat "What's your name\\? ", true]
                 false);

            send t "Sylvain\n";

            assert_bool
              "Answer hello"
              (ExpectPcre.expect t
                 [`Pat "Hello Sylvain", true]
                 false)));

    "oasis1" >::
    (fun test_ctxt ->
       with_qa test_ctxt "oasis1"
         (fun t ->
            assert_bool
              "???name "
              (expect t
                 [`Exact "???name ", true]
                 false);
            send t "test\n"));

    "oasis1-withfmatch" >::
    (fun test_ctxt ->
       with_qa test_ctxt "oasis1"
         (fun t ->
            assert_bool
              "???name "
              (expect t
                 ~fmatches:[(fun str ->
                               if str = "???name " then
                                 Some true
                               else
                                 None)]
                 []
                 false);
            send t "test\n"));

    "oasis1-timeout" >::
    (fun test_ctxt ->
       with_qa test_ctxt ~exit_code:(Unix.WEXITED 2) "oasis1"
         (fun t ->
            let mtx =
              Mutex.create ()
            in
            let finished =
              ref false
            in
            let th =
              Thread.create
                (fun () ->
                   let _b : bool =
                     expect t [`Exact "toto", true] false
                   in
                     Mutex.lock mtx;
                     finished := true;
                     Mutex.unlock mtx)
                ()
            in
            let is_finished =
              Thread.delay (5. *. timeout);
              Mutex.lock mtx;
              !finished
            in
              Mutex.unlock mtx;
              if not is_finished then
                assert_failure "Process didn't timeout";
              Thread.join th));

    "stderr" >::
    (fun test_ctxt ->
      skip_if (Sys.os_type = "Win32") "Don't work on Windows";
      with_qa test_ctxt ~use_stderr:true "stderr"
      (fun t ->
        assert_bool
        "error"
        (expect t
        [`Exact "error", true]
        false)));

    "exit" >::
    (fun test_ctxt ->
       with_qa test_ctxt "exit"
         (fun t ->
            assert_bool
              "eof"
              (expect t
                 [`Eof, true]
                 true)));

    "non-existing" >::
    (fun test_ctxt ->
      try
        let _, real_exit_code =
          with_spawn
            ~verbose:true
            ~verbose_output:(logf test_ctxt `Info "%s")
            ("blahblah") [||]
            (fun _ () -> ())
            ()
        in
          assert_equal
            ~msg:"Exit code"
            ~printer:printer_exit_code
            (Unix.WEXITED 127)
            real_exit_code
      with Unix.Unix_error(Unix.ENOENT, _, _) ->
        (* OCaml >5.0 is return ENOENT rather than WEXITED 127 *)
        ())
  ]

let () = run_test_tt_main tests
