

(** Extend Expect module with Pcre matches.
  
    This module extends {!Expect.expect} to also handle [Pcre] regular expression.
    You can use either `Pat or `Rex, which will be passed to [Pcre.pmatch].

{[
open Expect
open ExpectPcre

let (), _ = 
  with_spawn "ls" [| "-alh" |]
  (fun t () ->
    if expect t [`Pat "\\.", true] false then
      prerr_endline "'.' found")
  ()
]}

    @author Sylvain Le Gall
  *)

(** See {!Expect.expect}. *)
val expect :
  Expect.t ->
  ?fmatches:(string -> 'a option) list ->
  ([<Expect.expect_match 
    | `Pat of string 
    | `Rex of Pcre.regexp] * 'a) list -> 'a -> 'a
