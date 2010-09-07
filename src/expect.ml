

open Unix;;

type t =
    {
      id:            string;
      expect_stdin:  out_channel;
      expect_stdout: in_channel;
      expect_stderr: in_channel;
      timeout:       float option;
      verbose:       bool;
      mutable prev:  string;
    }
;;

type expect_match =
  | ExpectEof
  | ExpectRegexp of Str.regexp
  | ExpectFun of (string -> bool)
  | ExpectExact of string
  | ExpectTimeout 
;;

let spawn ?(verbose=false) ?(timeout=Some 10.0) prg args =
  let command_line =
    (* TODO: we should escape this string, to keep blank and other
     * problematic char
     *)
    String.concat " " (prg :: (Array.to_list args))
  in
  let (self_stdout, self_stdin, self_stderr) = 
    open_process_full
      command_line
      [||]
  in
    if verbose then
      print_endline command_line;

    {
      id            = command_line;
      expect_stdin  = self_stdin;
      expect_stdout = self_stdout;
      expect_stderr = self_stderr;
      timeout       = timeout;
      verbose       = verbose;
      prev          = "";
    }
;;

let set_timeout t timeout =
  {t with timeout = timeout}
;;

let send t str =
  if t.verbose then
    begin
      print_string str;
      flush Pervasives.stdout
    end;
  output_string t.expect_stdin str;
  flush t.expect_stdin;
;;

type expect_event =
  | Eof
  | Timeout
  | Line of string
;;

let expect t actions action_default =
  let buff = 
    String.make 4096 'x'
  in

  (* Test if an event can be associated with an action or continue *)
  let action_match event cont =
    try
      begin
        let _, res = 
          List.find 
            (fun (action_condition, _) ->
                match event, action_condition with
                  | Eof, ExpectEof
                  | Timeout, ExpectTimeout ->
                      true
                  | Line str, ExpectRegexp regexp ->
                      (
                        try
                          ignore(Str.search_forward regexp str 0);
                          true
                        with Not_found ->
                          false
                      )
                  | Line str, ExpectFun f ->
                      f str
                  | Line str, ExpectExact s ->
                      str = s
                  | _ ->
                      false)
            actions
        in
          res
      end
    with Not_found ->
      cont ()
  in

  (* Read a line from process *)
  let expect_input_line cont = 
    let input_len = 
      try 
        input t.expect_stdout buff 0 (String.length buff)
      with End_of_file ->
        0
    in

    let input_str = 
      String.sub buff 0 input_len
    in

    let () = 
      if t.verbose then
        begin
          print_string input_str;
          flush Pervasives.stdout 
        end
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
      let rec lines_aux str = 
        try 
          let str_len = String.length str in
          let pos_nl = String.index str '\n' in
          let hd = String.sub str 0 (pos_nl + 1) in
          let tl = 
            if (pos_nl + 1) < str_len then
              String.sub str (pos_nl + 1) (str_len - (pos_nl + 1))
            else
              ""
          in
            hd :: (lines_aux tl)
        with Not_found ->
          [str]
      in
        lines_aux (t.prev ^ input_str)
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
          let fd = 
            Unix.descr_of_in_channel t.expect_stdout
          in
            match Unix.select [fd] [] [fd] time_left with 
              | [], _, [] ->
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
  close_process_full 
    (
      t.expect_stdout,
      t.expect_stdin,
      t.expect_stderr
    )
;;

let with_spawn ?verbose ?timeout prog args f a =  
  let t = 
    spawn ?verbose ?timeout prog args
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

