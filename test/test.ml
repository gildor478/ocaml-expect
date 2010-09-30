(********************************************************************************)
(*  ocaml-expect: Expect-like framework for OCaml                               *)
(*                                                                              *)
(*  Copyright (C) 2010, OCamlCore SARL                                          *)
(*                                                                              *)
(*  This library is free software; you can redistribute it and/or modify it     *)
(*  under the terms of the GNU Lesser General Public License as published by    *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at     *)
(*  your option) any later version, with the OCaml static compilation           *)
(*  exception.                                                                  *)
(*                                                                              *)
(*  This library is distributed in the hope that it will be useful, but         *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY  *)
(*  or FITNESS FOR A PARTICULAR PURPOSE. See the file COPYING for more          *)
(*  details.                                                                    *)
(*                                                                              *)
(*  You should have received a copy of the GNU Lesser General Public License    *)
(*  along with this library; if not, write to the Free Software Foundation,     *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA               *)
(********************************************************************************)


(** Run tests for Expect 
    @author Sylvain Le Gall
  *)

open OUnit
open Expect
open TestCommon

let () = 
  let timeout = 
    0.1
  in
  let with_qa ?use_stderr ?(exit_code=Unix.WEXITED 0) suite f = 
    let _, real_exit_code = 
      with_spawn  
(*         ~verbose:true  *)
        ?use_stderr
        ~timeout:(Some timeout) "_build/test/qa" [|suite|]
        (fun t () -> f t) ()
    in
      assert_equal 
        ~msg:"Exit code"
        ~printer:printer_exit_code
        exit_code
        real_exit_code
  in

  let tests = 
    "Expect" >:::
    [
      "std-str" >::
      (fun () -> 
         with_qa "std"
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
      (fun () -> 
         with_qa "std"
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
      (fun () ->
         with_qa "oasis1"
           (fun t ->
              assert_bool
                "???name "
                (expect t 
                   [`Exact "???name ", true]
                   false);
              send t "test\n"));

      "oasis1-withfmatch" >::
      (fun () ->
         with_qa "oasis1"
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
      (fun () ->
         with_qa ~exit_code:(Unix.WEXITED 2) "oasis1"
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
      (fun () ->
         with_qa ~use_stderr:true "stderr"
           (fun t ->
              assert_bool
                "error"
                (expect t 
                   [`Exact "error", true]
                   false)));
    ]
  in

  let _lst =
    run_test_tt_main tests
  in

    ()

