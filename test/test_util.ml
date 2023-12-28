open Yorksap
open Util

let test_split_list_case1 () =
  assert (split_list 2 [ 1; 2; 3; 4; 5 ] = [ [ 1; 2 ]; [ 3; 4 ]; [ 5 ] ]);
  assert (
    split_list 6 [ 1; 2; 3; 4; 5; 6; 7; 8 ] = [ [ 1; 2; 3; 4; 5; 6 ]; [ 7; 8 ] ]);
  ()

let () =
  let open Alcotest in
  run "util"
    [ ("split_list", [ test_case "case1" `Quick test_split_list_case1 ]) ]
