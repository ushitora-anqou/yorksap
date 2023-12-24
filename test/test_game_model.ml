open Yorksap
open Game_model

let map = Game_data.London.map

let expect_ok = function
  | Ok x -> x
  | Error msg ->
      failwith
        (Printf.sprintf "expect ok but got error: %s\n" (Error.to_string msg))

let test_game () =
  let init_mr_x_loc = Loc.make ~id:155 () in
  let init_police_locs =
    [ 138; 50; 53; 198; 155 ] |> List.map (fun id -> Loc.make ~id ())
  in
  let init_locs = init_mr_x_loc :: init_police_locs in
  let g = Game.make ~init_locs ~map () in

  assert (
    map
    |> Map.get_links ~src:(Loc.make ~id:1 ())
    |> List.map (fun l -> (Link.dst l, Link.by l))
    |> List.sort compare
    = List.sort compare
        [
          (Loc.make ~id:8 (), `Taxi);
          (Loc.make ~id:9 (), `Taxi);
          (Loc.make ~id:58 (), `Bus);
          (Loc.make ~id:46 (), `Bus);
          (Loc.make ~id:46 (), `Ug);
        ]);

  assert (Game.agents g |> List.hd |> Agent.role = `MrX);
  Game.agents g |> List.tl
  |> List.iter (fun a -> assert (Agent.role a = `Police));

  assert (Game.agents g |> List.hd |> Agent.loc = init_mr_x_loc);
  List.combine (Game.agents g |> List.tl |> List.map Agent.loc) init_police_locs
  |> List.iter (fun (g, e) -> assert (g = e));

  assert (Game.agents g |> List.length = 6);
  assert (Game.turn g = 0);
  assert (Game.clock g = 1);

  let ts = Game.agents g |> List.hd |> Agent.ticket_set in
  assert (
    ts.taxi = 12 && ts.bus = 8 && ts.ug = 4 && ts.secret = 5 && ts.double = 2);
  Game.agents g |> List.tl
  |> List.iter (fun a ->
         let ts = Agent.ticket_set a in
         assert (
           ts.taxi = 10 && ts.bus = 8 && ts.ug = 4 && ts.secret = 0
           && ts.double = 0));

  let new_mr_x_loc = Loc.make ~id:156 () in
  let g = Game.move_agent (`Taxi new_mr_x_loc) g |> expect_ok in
  assert (Game.turn g = 1);
  assert (Game.clock g = 1);
  assert (Game.agents g |> List.hd |> Agent.loc = new_mr_x_loc);
  assert ((Game.agents g |> List.hd |> Agent.ticket_set).taxi = 11);
  assert (
    Game.history g
    |> History.get_view ~from:`MrX
    = (init_locs |> List.map Option.some, [ `Taxi new_mr_x_loc ]));
  ()

let test_invalid_move () =
  let g =
    Game.make
      ~init_locs:[ Loc.make ~id:115 (); Loc.make ~id:1 (); Loc.make ~id:8 () ]
      ~map ()
  in
  let g = Game.move_agent (`Taxi (Loc.make ~id:127 ())) g |> expect_ok in
  let res = Game.move_agent (`Taxi (Loc.make ~id:8 ())) g in
  assert (Result.is_error res);
  ()

let test_game_finished () =
  let g =
    Game.make ~init_locs:[ Loc.make ~id:9 (); Loc.make ~id:8 () ] ~map ()
  in
  assert (not (Game.is_finished g));
  let g = Game.move_agent (`Taxi (Loc.make ~id:1 ())) g |> expect_ok in
  assert (not (Game.is_finished g));
  let g = Game.move_agent (`Taxi (Loc.make ~id:1 ())) g |> expect_ok in
  assert (Game.is_finished g);
  ()

let test_use_boat () =
  let init_locs = [ Loc.make ~id:115 () ] in
  let g = Game.make ~init_locs ~map () in
  let new_loc = Loc.make ~id:108 () in
  let g = Game.move_agent (`Secret new_loc) g |> expect_ok in
  assert (Game.agents g |> List.hd |> Agent.loc = new_loc);
  assert ((Game.agents g |> List.hd |> Agent.ticket_set).secret = 4);
  assert (
    Game.history g
    |> History.get_view ~from:`MrX
    = (init_locs |> List.map Option.some, [ `Secret new_loc ]));
  ()

let test_open_locs () =
  let init_locs = [ Loc.make ~id:115 (); Loc.make ~id:1 () ] in
  let g =
    let ( >>= ) = Result.bind in
    Game.make ~init_locs ~map ()
    |> Result.ok
    >>= Game.move_agent (`Taxi (Loc.make ~id:114 ()))
    >>= Game.move_agent (`Taxi (Loc.make ~id:8 ()))
    >>= Game.move_agent (`Taxi (Loc.make ~id:113 ()))
    >>= Game.move_agent (`Taxi (Loc.make ~id:1 ()))
    >>= Game.move_agent (`Taxi (Loc.make ~id:125 ()))
    |> expect_ok
  in

  assert (
    match Game.history g |> History.get_view ~from:`Police |> snd with
    | [ `Hidden `Taxi; `Taxi loc1; `Hidden `Taxi; `Taxi loc2; `Taxi loc3 ]
      when Loc.id loc1 = 8 && Loc.id loc2 = 1 && Loc.id loc3 = 125 ->
        true
    | _ -> false);
  ()

let test_use_double_move () =
  let init_locs = [ Loc.make ~id:115 (); Loc.make ~id:1 () ] in
  let g = Game.make ~init_locs ~map () in
  let new_loc1 = Loc.make ~id:108 () in
  let new_loc2 = Loc.make ~id:105 () in
  let g =
    Game.move_agent (`Double (`Secret new_loc1, `Bus new_loc2)) g |> expect_ok
  in
  assert (Game.turn g = 1);
  assert (Game.clock g = 2);
  assert (Game.agents g |> List.hd |> Agent.loc = new_loc2);
  assert ((Game.agents g |> List.hd |> Agent.ticket_set).secret = 4);
  assert ((Game.agents g |> List.hd |> Agent.ticket_set).bus = 7);
  assert ((Game.agents g |> List.hd |> Agent.ticket_set).double = 1);
  assert (
    Game.history g
    |> History.get_view ~from:`MrX
    = ( init_locs |> List.map Option.some,
        [ `Double (`Secret new_loc1, `Bus new_loc2) ] ));

  let new_loc = Loc.make ~id:8 () in
  let g = Game.move_agent (`Taxi new_loc) g |> expect_ok in
  assert (
    Game.history g
    |> History.get_view ~from:`MrX
    = ( init_locs |> List.map Option.some,
        [ `Double (`Secret new_loc1, `Bus new_loc2); `Taxi new_loc ] ));
  ()

let test_yojson_serialization () =
  let init_mr_x_loc = Loc.make ~id:155 () in
  let init_police_locs =
    [ 138; 50; 53; 198; 155 ] |> List.map (fun id -> Loc.make ~id ())
  in
  let init_locs = init_mr_x_loc :: init_police_locs in
  let g = Game.make ~init_locs ~map () in
  let g' =
    match g |> Game.to_yojson |> Game.of_yojson ~map with
    | Ok g -> g
    | Error msg -> failwith msg
  in
  assert (Game.history g = Game.history g');
  assert (Game.is_finished g = Game.is_finished g');
  assert (Game.turn g = Game.turn g');
  assert (Game.clock g = Game.clock g');
  assert (List.length (Game.agents g) = List.length (Game.agents g'));
  assert (Game.agents g = Game.agents g');
  assert (g = g')

let test_giving_tickets_to_mr_x () =
  let init_locs = [ Loc.make ~id:115 (); Loc.make ~id:1 () ] in
  let g = Game.make ~init_locs ~map () in
  let g = Game.move_agent (`Taxi (Loc.make ~id:114 ())) g |> expect_ok in
  assert ((List.nth (Game.agents g) 0 |> Agent.ticket_set).taxi = 11);
  assert ((List.nth (Game.agents g) 1 |> Agent.ticket_set).taxi = 10);
  let g = Game.move_agent (`Taxi (Loc.make ~id:8 ())) g |> expect_ok in
  assert ((List.nth (Game.agents g) 0 |> Agent.ticket_set).taxi = 12);
  assert ((List.nth (Game.agents g) 1 |> Agent.ticket_set).taxi = 9);
  ()

let test_game_over_by_timeout () =
  let init_locs = [ Loc.make ~id:13 (); Loc.make ~id:89 () ] in
  let g = Game.make ~init_locs ~map () in
  let moves =
    List.init 22 Fun.id
    |> List.fold_left
         (fun acc i ->
           let mr_x =
             if i < 22 then
               if i mod 2 = 0 then `Taxi (Loc.make ~id:23 ())
               else `Taxi (Loc.make ~id:13 ())
             else if i mod 2 = 0 then `Bus (Loc.make ~id:23 ())
             else `Bus (Loc.make ~id:13 ())
           in
           let police =
             if i < 10 then
               if i mod 2 = 0 then `Taxi (Loc.make ~id:105 ())
               else `Taxi (Loc.make ~id:89 ())
             else if i < 18 then
               if i mod 2 = 0 then `Bus (Loc.make ~id:105 ())
               else `Bus (Loc.make ~id:89 ())
             else if i mod 2 = 0 then `Ug (Loc.make ~id:67 ())
             else `Ug (Loc.make ~id:89 ())
           in
           police :: mr_x :: acc)
         []
    |> List.rev
  in
  let g =
    moves
    |> List.fold_left (fun g move -> Game.move_agent move g |> expect_ok) g
  in
  let g = g |> Game.move_agent (`Bus (Loc.make ~id:23 ())) |> expect_ok in
  let g = g |> Game.skip_turn |> expect_ok in
  assert (Game.clock g = 24);
  assert (not (Game.is_finished g));
  let g = g |> Game.move_agent (`Bus (Loc.make ~id:13 ())) |> expect_ok in
  let g = g |> Game.skip_turn |> expect_ok in
  assert (Game.is_finished g);
  ()

(* FIXME test skip *)
(* FIXME derived possible moves *)
(* FIXME game finishes at 24:00 *)
let () =
  let open Alcotest in
  run "game_model"
    [
      ( "cases",
        [
          test_case "basics" `Quick test_game;
          test_case "invalid move" `Quick test_invalid_move;
          test_case "game over check" `Quick test_game_finished;
          test_case "able to use boat" `Quick test_use_boat;
          test_case "enclosure of locs" `Quick test_open_locs;
          test_case "able to use double-move tickets" `Quick
            test_use_double_move;
          test_case "serialization" `Quick test_yojson_serialization;
          test_case "give tickets to Mr.X" `Quick test_giving_tickets_to_mr_x;
          test_case "game over by timeout" `Quick test_game_over_by_timeout;
        ] );
    ]
