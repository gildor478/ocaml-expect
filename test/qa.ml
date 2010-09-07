
let std () =
  print_string "What's your name? "; flush stdout;
  print_endline ("Hello "^(read_line ()))

let oasis1 () = 
  print_string 
    "The program will ask some questions to create the OASIS file. If you answer\n\
     '?' to a question, an help text will be displayed.\n\
     \n\
     ???name ";
  flush stdout;
  ignore (read_line ())

let () = 
  match Sys.argv.(1) with
    | "std" -> std ()
    | "oasis1" -> oasis1 ()
    | str -> failwith ("Unknown test suite: "^str)
