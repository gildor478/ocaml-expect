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

let () = 
  let with_qa suite f = 
    let _, exit_code = 
      with_spawn  
        ~timeout:(Some 0.1) "_build/test/qa" [|suite|]
        (fun t () -> f t) ()
    in
      assert_equal 
        ~msg:"Exit code"
        (Unix.WEXITED 0)
        exit_code
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
    ]
  in

  let _lst =
    run_test_tt_main tests
  in

    ()

