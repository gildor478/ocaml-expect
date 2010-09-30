(** Extend Expect module with Str matches.
  
    This module extends {!Expect.expect} to also handle [Str] regular expression.
    You can use `Regexp to provide the [Str.regexp].

{[
open Expect
open ExpectStr

let (), _ = 
  with_spawn "ls" [| "-alh" |]
  (fun t () ->
    if expect t [`Regexp (Str.regexp "\\."), true] false then
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
    | `Regexp of Str.regexp] * 'a) list -> 'a -> 'a
