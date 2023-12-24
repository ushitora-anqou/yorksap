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
