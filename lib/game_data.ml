open Game_model

module London = struct
  let map =
    let ( >>= ) = Result.bind in
    let link src by dst x =
      x
      |> Map.add_link
           (Link.make ~src:(Loc.make ~id:src ()) ~dst:(Loc.make ~id:dst ()) ~by
              ())
      >>= Map.add_link
            (Link.make ~src:(Loc.make ~id:dst ()) ~dst:(Loc.make ~id:src ()) ~by
               ())
    in
    Ok Map.empty >>= link 1 `Taxi 8 >>= link 1 `Taxi 9 >>= link 1 `Bus 58
    >>= link 1 `Bus 46 >>= link 1 `Ug 46 >>= link 115 `Boat 108
    >>= link 108 `Bus 105 >>= link 115 `Taxi 127 >>= link 155 `Taxi 156
    >>= link 115 `Taxi 114 >>= link 114 `Taxi 113 >>= link 113 `Taxi 125
    >>= link 89 `Taxi 105 >>= link 89 `Bus 105 >>= link 89 `Ug 67
    >>= link 13 `Taxi 23 >>= link 13 `Bus 23 |> Result.get_ok
end
