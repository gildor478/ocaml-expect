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


open Unix;;

type t =
    {
      pid:            int;
      expect_stdin:   file_descr;
      expect_stdout:  file_descr;
      timeout:        float option;
      verbose:        string -> unit;
      mutable prev:   string;
    }
;;

type expect_match =
    [
        `Eof
      | `Fun of (string -> bool)
      | `Exact of string
      | `Prefix of string
      | `Suffix of string
      | `Contains of string
      | `Timeout
    ]
;;

let default_verbose_output =
  print_endline

let spawn ?(verbose=false)
          ?(verbose_output=default_verbose_output)
          ?(timeout=Some 10.0)
          ?env
          ?(use_stderr=false) prg args =
  let command_line =
    String.concat " " (prg :: (Array.to_list args))
  in
  let cmd =
    Array.init
      ((Array.length args) + 1)
      (function
         | 0 -> prg
         | n -> args.(n - 1))
  in
  let proc_stdin, self_stdin =
    pipe ()
  in
  let self_stdout, proc_stdout =
    pipe ()
  in
  let safe_set_close_on_exec fd =
    try
      set_close_on_exec fd
    with _ ->
      ()
  in
  let proc_stderr =
    if use_stderr then
      begin
        let fd =
          dup proc_stdout
        in
          safe_set_close_on_exec fd;
          fd
      end

    else
      begin
        stderr
      end
  in
  let pid =
    safe_set_close_on_exec self_stdin;
    safe_set_close_on_exec self_stdout;
    match env with
      | Some a ->
          create_process_env prg cmd a proc_stdin proc_stdout proc_stderr
      | None ->
          create_process prg cmd proc_stdin proc_stdout proc_stderr
  in
  let verbose str =
    if verbose then
      verbose_output str
  in
    close proc_stdout;
    close proc_stdin;
    verbose ("Command line: "^command_line);

    {
      pid           = pid;
      expect_stdin  = self_stdin;
      expect_stdout = self_stdout;
      timeout       = timeout;
      verbose       = verbose;
      prev          = "";
    }
;;

let set_timeout t timeout =
  {t with timeout = timeout}
;;

let send t str =
  let _i : int =
    t.verbose (Printf.sprintf "Send: %S" str);
    write t.expect_stdin str 0 (String.length str)
  in
    ()
;;

type expect_event =
  | Eof
  | Timeout
  | Line of string
;;

let expect t ?(fmatches=[]) actions action_default =
  let buff =
    String.make 4096 'x'
  in

  (* Tets if an event can be associated with a fmatch action or continue *)
  let action_fmatch event cont =
    match event with
      | Eof | Timeout ->
          cont ()
      | Line str ->
          begin
            let res =
              List.fold_left
                (fun res f ->
                   if res = None then
                     f str
                   else
                     res)
                None
                fmatches
            in
              match res with
                | Some e ->
                    e
                | None ->
                    cont ()
          end
  in

  (* Test if an event can be associated with an action or continue *)
  let action_match event cont =
    try
      begin
        let _, res =
          List.find
            (fun (action_condition, _) ->
                match event, action_condition with
                  | Eof, `Eof
                  | Timeout, `Timeout ->
                      true
                  | Line str, `Fun f ->
                      f str
                  | Line str, `Exact s ->
                      str = s
                  | Line str, `Suffix suff ->
                      BatString.ends_with str suff
                  | Line str, `Prefix pre ->
                      BatString.starts_with str pre
                  | Line str, `Contains sub ->
                      BatString.exists str sub
                  | _ ->
                      false)
            actions
        in
          res
      end
    with Not_found ->
      (* Nothing match standard action condition, have a look
       * to fmatches
       *)
      action_fmatch event cont
  in

  (* Read a line from process *)
  let expect_input_line cont =
    let input_len =
      try
        read t.expect_stdout buff 0 (String.length buff)
      with End_of_file
        | Unix_error(EPIPE, "read", _) ->
        0
    in

    let input_str =
      String.sub buff 0 input_len
    in

    let () =
      t.verbose (Printf.sprintf "Receive: %S" input_str);
    in

    (* Modify continuation if we reach the end of the stream *)
    let cont =
      if input_len = 0 then
        (* Nothing to process anymore *)
        fun () ->
          action_match
            Eof
            (fun () -> action_default)
      else
        cont
    in

    let lines =
      BatString.nsplit (t.prev ^ input_str) "\n"
    in

    let rec scan_lines =
      function
        | [ln] when
            ln <> "" &&
            ln.[(String.length ln) - 1] <> '\n' ->
            begin
              action_match (Line ln)
                (fun () ->
                   t.prev <- ln;
                   cont ())
            end

        | hd :: tl ->
            begin
              action_match (Line hd) (fun () -> scan_lines tl)
            end

        | [] ->
            begin
              t.prev <- "";
              cont ()
            end
    in
      scan_lines lines

  in

  (* Read lines from process, considering timeout *)
  let rec expect_input_line_timeout timeout time_begin =
    let time_left =
      timeout -. ((Unix.gettimeofday ()) -. time_begin)
    in
      if time_left > 0.0 then
        begin
          match Unix.select [t.expect_stdout] [] [] time_left with
            | [], _, _ ->
                expect_input_line_timeout
                  timeout time_begin

            | _ ->
                expect_input_line
                  (fun () ->
                     expect_input_line_timeout
                       timeout time_begin)
        end
      else
        begin
          action_match Timeout (fun () -> action_default)
        end
  in

  (* Read lines from process *)
  let rec expect_input_line_notimeout () =
    expect_input_line
      expect_input_line_notimeout
  in

    (* Read a stdout until something is found *)
    match t.timeout with
      | Some timeout ->
          expect_input_line_timeout
            timeout
            (Unix.gettimeofday ())
      | None ->
          expect_input_line_notimeout ()
;;

let close t =
  let rec waitpid_non_intr () =
    try
      waitpid [] t.pid
    with Unix_error (EINTR, _, _) ->
      waitpid_non_intr ()
  in
  let safe_close fd =
    try
      close fd
    with _ ->
      ()
  in
    safe_close t.expect_stdout;
    safe_close t.expect_stdin;
    snd (waitpid_non_intr ())
;;

let with_spawn
      ?verbose ?verbose_output ?timeout ?env ?use_stderr prog args f a =
  let t =
    spawn ?verbose ?verbose_output ?timeout ?env ?use_stderr prog args
  in
    try
      let res =
        f t a
      in
      let exit_code =
        close t
      in
        res, exit_code

    with e ->
      ignore(close t);
      raise e
;;

