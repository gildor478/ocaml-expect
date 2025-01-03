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

open Expect

let expect t ?fmatches actions action_default =
  let actions =
    List.map
      (fun (exp, a) ->
         let exp =
           match exp with
             | #expect_match as x ->
                 x
             | `Rex rex ->
                 `Fun (fun s -> Re.Pcre.pmatch ~rex s)
             | `Pat pat ->
                 let rex = Re.Pcre.regexp pat in
                 `Fun (fun s -> Re.Pcre.pmatch ~rex s)
         in
           exp, a)
      actions
  in
    expect t ?fmatches actions action_default


