module Logg = Logg
module Int_map = Map.Make (Int)

module type S = sig
  module Error : sig
    type t
  end

  module Loc : sig
    type t

    val make : id:int -> unit -> t
    val id : t -> int
    val show : t -> string
  end

  module Link : sig
    type t
    type transport = [ `Taxi | `Bus | `Ug | `Boat ]

    val make : src:Loc.t -> dst:Loc.t -> by:transport -> unit -> t
    val src : t -> Loc.t
    val dst : t -> Loc.t
    val by : t -> transport
    val show : t -> string
  end

  module Map : sig
    type t

    val empty : t
    val add_loc : Loc.t -> t -> (t, Error.t) result
    val add_link : Link.t -> t -> (t, Error.t) result
    val get_links : src:Loc.t -> t -> Link.t list
    val show : t -> string
  end

  module Ticket : sig
    type basic = [ `Taxi | `Bus | `Ug | `Secret ]
    type t = [ basic | `Double of basic * basic ]
    type set = { taxi : int; bus : int; ug : int; secret : int; double : int }

    val show : t -> string
  end

  module Agent : sig
    type t

    val make : ?pos:Loc.t -> ticket_set:Ticket.set -> is_mr_x:bool -> unit -> t
    val role : t -> [ `MrX | `Police ]
    val pos : t -> Loc.t
    val ticket_set : t -> Ticket.set
    val move : dst:Loc.t -> by:Ticket.t -> t -> (t, Error.t) result
    val show : t -> string
  end

  module History : sig
    type t

    val make : init_mr_x_loc:Loc.t -> init_police_locs:Loc.t list -> unit -> t
    val add : agent_id:int -> by:Ticket.t -> dst:Loc.t -> t
    val get : agent_id:int -> clock:int -> t -> (Ticket.t * Loc.t) option
    (* agent_id is 0 (Mr.X) or 1-4 (police). clock is 1 through 24. *)
  end

  module Game : sig
    type t

    val make :
      init_mr_x_loc:Loc.t ->
      init_police_locs:Loc.t list ->
      map:Map.t ->
      unit ->
      t

    val mr_x : t -> Agent.t
    val police : t -> Agent.t list
    val history : t -> History.t
    val turn : t -> int (* Turn 0 is Mr.X's. Turn 1 through 4 are police's. *)
    val clock : t -> int (* 1 through 24 *)
    val move_agent : by:Ticket.t -> dst:Loc.t -> t -> (t, Error.t) result
    val show : t -> string
    val to_yojson : t -> Yojson.Safe.t
  end
end

module Test (Yrksp : S) = struct
  open Yrksp

  let test_game () =
    let map =
      let loc id = Map.add_loc (Loc.make ~id ()) in
      let link src by dst =
        Map.add_link
          (Link.make ~src:(Loc.make ~id:src ()) ~dst:(Loc.make ~id:dst ()) ~by
             ())
      in
      let ( >>= ) = Result.bind in
      Result.get_ok
        (Ok Map.empty >>= loc 1 >>= loc 8 >>= loc 9 >>= loc 46 >>= loc 50
       >>= loc 53 >>= loc 58 >>= loc 155 >>= loc 198 >>= link 1 `Taxi 8
       >>= link 1 `Taxi 9 >>= link 1 `Bus 58 >>= link 1 `Bus 46
       >>= link 1 `Ug 46)
    in

    let init_mr_x_loc = Loc.make ~id:155 () in
    let init_police_locs =
      [ 138; 50; 53; 198 ] |> List.map (fun id -> Loc.make ~id ())
    in
    let g = Game.make ~init_mr_x_loc ~init_police_locs ~map () in

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

    assert (Game.mr_x g |> Agent.role = `MrX);
    Game.police g |> List.iter (fun a -> assert (Agent.role a = `Police));

    assert (Game.mr_x g |> Agent.pos = init_mr_x_loc);
    List.combine (Game.police g |> List.map Agent.pos) init_police_locs
    |> List.iter (fun (g, e) -> assert (g = e));

    assert (Game.police g |> List.length = 4);
    assert (Game.history g |> History.get ~agent_id:0 ~clock:0 |> Option.is_some);
    assert (Game.history g |> History.get ~agent_id:0 ~clock:1 |> Option.is_none);
    assert (Game.turn g = 0);
    assert (Game.clock g = 1);

    let ts = Game.mr_x g |> Agent.ticket_set in
    assert (
      ts.taxi = 12 && ts.bus = 9 && ts.ug = 4 && ts.secret = 5 && ts.double = 2);
    Game.police g
    |> List.iter (fun a ->
           let ts = Agent.ticket_set a in
           assert (
             ts.taxi = 10 && ts.bus = 8 && ts.ug = 4 && ts.secret = 0
             && ts.double = 0));

    let new_mr_x_loc = Loc.make ~id:156 () in
    let g = Game.move_agent ~by:`Taxi ~dst:new_mr_x_loc g |> Result.get_ok in
    assert (Game.turn g = 1);
    assert (Game.clock g = 1);
    assert (Game.mr_x g |> Agent.pos = new_mr_x_loc);
    assert ((Game.mr_x g |> Agent.ticket_set).taxi = 11);
    assert (Game.history g |> History.get ~agent_id:1 ~clock:1 |> Option.is_none);

    assert (
      Yojson.Safe.equal (Game.to_yojson g)
        (`Assoc
          [
            ("turn", `Int 0);
            ("clock", `Int 1);
            ( "history",
              `List
                [
                  `List [ `Int 155; `Int 138; `Int 50; `Int 53; `Int 198 ];
                  `List [ `List [ `String "taxi"; `Int 156 ] ];
                ] );
            ( "agent",
              `List
                [
                  `Assoc
                    [
                      ("loc", `Int 156);
                      ( "tickets",
                        `List [ `Int 11; `Int 9; `Int 4; `Int 5; `Int 2 ] );
                    ];
                  `Assoc
                    [
                      ("loc", `Int 138);
                      ( "tickets",
                        `List [ `Int 10; `Int 8; `Int 4; `Int 0; `Int 0 ] );
                    ];
                  `Assoc
                    [
                      ("loc", `Int 50);
                      ( "tickets",
                        `List [ `Int 10; `Int 8; `Int 4; `Int 0; `Int 0 ] );
                    ];
                  `Assoc
                    [
                      ("loc", `Int 53);
                      ( "tickets",
                        `List [ `Int 10; `Int 8; `Int 4; `Int 0; `Int 0 ] );
                    ];
                  `Assoc
                    [
                      ("loc", `Int 198);
                      ( "tickets",
                        `List [ `Int 10; `Int 8; `Int 4; `Int 0; `Int 0 ] );
                    ];
                ] );
          ]));

    ()
end
