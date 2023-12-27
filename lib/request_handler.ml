(*
let get_api_v1_game ~game_id:_ =
  let open Game_model in
  let split_list n l =
    l
    |> List.fold_left
         (fun acc x ->
           if List.length (List.hd acc) < n then
             (x :: List.hd acc) :: List.tl acc
           else List.rev (List.hd acc) :: List.tl acc)
         [ [] ]
    |> List.rev
  in
  let move_single_to_string = function
    | `Taxi _ -> "taxi"
    | `Bus _ -> "bus"
    | `Ug _ -> "ug"
    | `Secret _ -> "secret"
  in
  let move_single_to_string' = function
    | `Taxi -> "taxi"
    | `Bus -> "bus"
    | `Ug -> "ug"
    | `Secret -> "secret"
  in

  let serialized_game = "" (* Retrieved by game_id *) in
  let is_mr_x = false (* Retrieved by session *) in
  let g =
    serialized_game |> Yojson.Safe.from_string
    |> Game.of_yojson ~map:Game_data.London.map
    |> Result.get_ok
  in
  let masked_move_to_yojson : Move.t_with_hidden -> Yojson.Safe.t = function
    | `Hidden v ->
        `List [ `String "hidden"; `String (move_single_to_string' v) ]
    | (`Taxi loc | `Bus loc | `Ug loc | `Secret loc) as v ->
        `List [ `String (move_single_to_string v); `Int (Loc.id loc) ]
    | `Double _ -> assert false
  in
  let agent_to_yojson agent =
    `Assoc
      [
        ("loc", `Int (agent |> Agent.loc |> Loc.id));
        ( "tickets",
          let ts = agent |> Agent.ticket_set in
          `List
            [
              `Int ts.taxi;
              `Int ts.bus;
              `Int ts.ug;
              `Int ts.secret;
              `Int ts.double;
            ] );
      ]
  in
  let history_to_yojson ~from ~num_agents history =
    let init_locs, moves = history |> History.get_view ~from in
    `List
      (`List
         (init_locs
         |> List.map (function None -> `Null | Some l -> `Int (Loc.id l)))
      :: (moves |> split_list num_agents
         |> List.map (List.map masked_move_to_yojson)
         |> List.map (fun xs -> `List xs)))
  in
  let move_to_yojson (move : Move.t) : Yojson.Safe.t =
    let rec aux : Move.t -> Yojson.Safe.t = function
      | (`Taxi loc | `Bus loc | `Ug loc | `Secret loc) as x ->
          `List [ `String (move_single_to_string x); `Int (Loc.id loc) ]
      | `Double (first, second) ->
          `List
            [ `String "double"; aux (first :> Move.t); aux (second :> Move.t) ]
    in
    aux move
  in
  let is_finished = Game.has_finished g in
  let _ : Yojson.Safe.t =
    `Assoc
      [
        ("turn", `Int (Game.turn g));
        ("clock", `Int (Game.clock g));
        ("is_finished", `Bool is_finished);
        ("agents", `List (g |> Game.agents |> List.map agent_to_yojson));
        ( "history",
          g |> Game.history
          |> history_to_yojson
               ~from:(if is_finished || is_mr_x then `MrX else `Police)
               ~num_agents:(g |> Game.agents |> List.length) );
        ( "possible_moves",
          `List (g |> Game.derive_possible_moves |> List.map move_to_yojson) );
      ]
  in
  ()
  *)
