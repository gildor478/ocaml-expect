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

(** Extend Expect module with Str matches.

    This module extends {!Expect.expect} to also handle [Str] regular
    expression. You can use `Regexp to provide the [Str.regexp].

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
