open Game_model

type t = {
  map : Map.t;
  init_loc_candidates : Loc.t list;
  disclosure_clocks : int list;
}

module Option = struct
  include Option

  let ( let* ) = bind
  let ( >>= ) = bind
end

module Result = struct
  include Result

  let ( let* ) = bind
  let ( >>= ) = bind
end

let of_yojson j =
  let ( let* ) = Result.bind in
  let ( >>= ) = Result.bind in
  let assoc x l =
    match List.assoc_opt x l with
    | None -> Error (Printf.sprintf "%s not found" x)
    | Some v -> Ok v
  in
  let to_list = function
    | `List x -> Ok x
    | _ -> Error "expected list but got something else"
  in
  let to_assoc = function
    | `Assoc x -> Ok x
    | _ -> Error "expected assoc but got something else"
  in
  let to_int_list j =
    j |> to_list
    >>= List.fold_left
          (fun acc x ->
            let* acc = acc in
            match x with
            | `Int i -> Ok (i :: acc)
            | _ -> Error "expected int list but got something else ")
          (Ok [])
  in

  let root = Yojson.Safe.Util.to_assoc j in

  let* map =
    let aux (map : (Map.t, string) result) j =
      let* by, src, dst =
        match j with
        | `List [ `String by; `Int src; `Int dst ] ->
            let by =
              match by with
              | "taxi" -> `Taxi
              | "bus" -> `Bus
              | "ug" -> `Ug
              | "boat" -> `Boat
              | _ -> failwith ""
            in
            Ok (by, Loc.make ~id:src (), Loc.make ~id:dst ())
        | _ -> Error "link is invalid"
      in
      let* map = map in
      let* map =
        Map.add_link (Link.make ~src ~dst ~by ()) map
        |> Result.map_error Error.to_string
      in
      let* map =
        Map.add_link (Link.make ~src:dst ~dst:src ~by ()) map
        |> Result.map_error Error.to_string
      in
      Ok map
    in

    root |> assoc "map" >>= to_assoc >>= assoc "links" >>= to_list
    >>= List.fold_left aux (Ok Map.empty)
  in

  let* init_loc_candidates =
    root
    |> assoc "init_loc_candidates"
    >>= to_int_list
    |> Result.map (List.map (fun id -> Loc.make ~id ()))
    |> Result.map_error (fun s -> "invalid init_loc_candidates: " ^ s)
  in

  let* disclosure_clocks =
    root |> assoc "disclosure_clocks" >>= to_int_list
    |> Result.map_error (fun s -> "invalid disclosure_clocks: " ^ s)
  in

  Ok { map; init_loc_candidates; disclosure_clocks }

let array_shuffle_in_place a =
  (* Fisher--Yates shuffle *)
  let open Array in
  let n = length a in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = a.(j) in
    a.(j) <- a.(i);
    a.(i) <- tmp
  done;
  ()

let generate_init_locs n t =
  if n >= List.length t.init_loc_candidates then
    invalid_arg "n is larger than length of init_loc_candidates";
  let ary = Array.of_list t.init_loc_candidates in
  array_shuffle_in_place ary;
  List.init n (fun i -> ary.(i))
