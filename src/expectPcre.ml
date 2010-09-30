
open Expect

let expect t actions action_default =
  let actions = 
    List.map
      (fun (exp, a) ->
         let exp = 
           match exp with 
             | #expect_match as x -> 
                 x
             | `Rex rex ->
                 `Fun (fun s -> Pcre.pmatch ~rex s)
             | `Pat pat ->
                 `Fun (fun s -> Pcre.pmatch ~pat s)
         in
           exp, a)
      actions
  in
    expect t actions action_default


