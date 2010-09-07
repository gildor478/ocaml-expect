

(** Run tests for Expect 
    @author Sylvain Le Gall
  *)

open OUnit
open Expect

let () = 
  let with_qa suite f = 
    let _, exit_code = 
      with_spawn  
        ~verbose:true
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
      "std" >::
      (fun () -> 
         with_qa "std"
           (fun t ->
              assert_bool
                "Ask name"
                (expect t 
                   [ExpectRegexp 
                      (Str.regexp (Str.quote "What's your name? ")), 
                    true]
                   false);

              send t "Sylvain\n";

              assert_bool
                "Answer hello"
                (expect t
                   [ExpectRegexp 
                      (Str.regexp (Str.quote "Hello Sylvain")),
                    true]
                   false)));

      "oasis1" >::
      (fun () ->
         with_qa "oasis1"
           (fun t ->
              assert_bool
                "???name "
                (expect t 
                   [ExpectExact "???name ", true]
                   false);
              send t "test\n"));
    ]
  in

  let _lst =
    run_test_tt_main tests
  in

    ()
