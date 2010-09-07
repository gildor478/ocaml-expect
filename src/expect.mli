

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

let () = 
  let ls_prg =
    spawn "ls" [| "-alh" |]
  in
  let find_dot =
    expect 
      ls_prg
      [ ExpectRegexp(Str.regexp "\\."), true ]
      false
  in
  let () = 
    if find_dot then
      print_endline "'.' found"
    else
      print_endline "'.' not found"
  in
    match close ls_prg with
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
    ExpectEof                     (* Look for EOF *)
  | ExpectRegexp of Str.regexp    (* Look for a line matching the regexp *)
  | ExpectFun of (string -> bool) (* Look for a line matching the string *)
  | ExpectExact of string         (* Look for a line matching exactly this 
                                     string *)
  | ExpectTimeout                 (* Wait timeout *)

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
