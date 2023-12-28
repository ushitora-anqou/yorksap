open Yorksap
open Util

let test_split_list_case1 () =
  assert (split_list 2 [ 1; 2; 3; 4; 5 ] = [ [ 1; 2 ]; [ 3; 4 ]; [ 5 ] ]);
  ()

let () =
  let open Alcotest in
  run "util"
    [ ("split_list", [ test_case "case1" `Quick test_split_list_case1 ]) ]
