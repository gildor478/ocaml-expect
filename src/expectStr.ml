
open Expect

let expect t ?fmatches actions action_default =
  let actions = 
    List.map
      (fun (exp, a) ->
         let exp = 
           match exp with 
             | #expect_match as x -> 
                 x
             | `Regexp rgxp ->
                 `Fun 
                   (fun s -> 
                      try
                        ignore(Str.search_forward rgxp s 0);
                        true
                      with Not_found ->
                        false)
         in
           exp, a)
      actions
  in
    expect t ?fmatches actions action_default
