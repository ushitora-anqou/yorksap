module Int_map = struct
  include Map.Make (Int)
end

module Farray = struct
  include CCPersistentArray

  let ( .@() ) = get
  let ( .@()<- ) = set

  let fold_lefti f init ary =
    fold_left (fun (acc, i) x -> (f i acc x, i + 1)) (init, 0) ary |> fst
end

let split_list n l =
  l
  |> List.fold_left
       (fun acc x ->
         if List.length (List.hd acc) < n then (x :: List.hd acc) :: List.tl acc
         else [ x ] :: List.rev (List.hd acc) :: List.tl acc)
       [ [] ]
  |> function
  | [] -> assert false
  | x :: xs -> List.rev x :: xs |> List.rev
