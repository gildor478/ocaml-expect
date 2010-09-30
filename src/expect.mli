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


(** Expect module for testing interactive program.

    This is a simple implementation of expect to help building unitary testing
    of interactive program. Since this is an OCaml library, only specific part
    of expect has been implemented. Other function can be replaced by standard
    OCaml functions (exit...).
   
    The use of this library is built around 4 functions:
    - spawn: to create a process
    - send: to send a string to the process
    - expect: match output of the process
    - close: end the process
   
    Output of the program is processed line by line.
   
    Regular expression is implemented through the library Str. You will need to
    build a regexp using this module. The regexp should only match a substring
    of the line. If you need to match something at the beginning or at the end, 
    use "^" and "$". To use a regexp
   
    Additional match functions can be build using a standard function. This
    function is passed the entire line and should return if it match or not.
   
    There is two additional event to match:
    - eof: process close its output
    - timeout: too much time has been spent waiting to match something
   
    Both of this action, if not matched will use the default_action provided.
   
    Here is an example program, that look for string "." in the output:
    
{[
open Expect

let (), exit_code = 
  with_spawn "ls" [|"-alh"|]
  (fun t () ->
    if expect t [`Exact ".", true] false then
      prerr_endline "'.' found"
    else
      prerr_endline "'.' not found")
  ()
in
  match exit_code with
  | Unix.WEXITED 0 ->
      print_endline "Exit normal"
  | _ ->
      print_endline "Problem when exiting"
]}
   
    See {{:http://directory.fsf.org/project/expect/}Expect manual}

    @author Sylvain Le Gall
  *)

(** A process under the monitoring of Expect.
  *)
type t

(** Describe expectation about the output of the process. Lines includes the EOL
    (i.e. \n).
  *)
type expect_match =
    [
        `Eof                     (** Look for EOF *)
      | `Fun of (string -> bool) (** Look for a line matching the string *)
      | `Exact of string         (** Look for a line matching exactly this 
                                     string *)
      | `Suffix of string        (** Look for a line ending with this 
                                     string *)
      | `Prefix of string        (** Look for a line starting with this 
                                     string *)
      | `Contains of string      (** Look for a line containing this
                                     string *)
      | `Timeout                 (* Wait timeout *)
    ]

(** [spawn prg args] Start a process and monitor its output. Contrary to
    [Unix.create_process], you don't need to repeat the program name at the 
    beginning of args.
  *)
val spawn: ?verbose:bool -> ?timeout:float option -> string -> string array -> t

(** Define the timeout for a process.
  *)
val set_timeout: t -> float option -> t

(** Send a string to a process.
  *)
val send: t -> string -> unit

(** Wait for output of the process and match it against expectation.
  *)
val expect: t -> (expect_match * 'a) list -> 'a -> 'a

(** Close the process.
  *)
val close: t -> Unix.process_status

(** Take care of opening and closing the process.
  *)
val with_spawn: 
  ?verbose:bool -> 
  ?timeout:float option -> 
  string -> string array -> 
  (t -> 'a -> 'a) -> 'a -> 'a * Unix.process_status
