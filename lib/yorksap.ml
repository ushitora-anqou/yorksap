module Logg = Logg

module Int_map = struct
  include Map.Make (Int)
end

module type S = sig
  module type Loc_sig = sig
    type t

    val make : id:int -> unit -> t
    val id : t -> int
    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end

  module Loc : Loc_sig

  module type Link_sig = sig
    type t
    type transport = [ `Taxi | `Bus | `Ug | `Boat ]

    val make : src:Loc.t -> dst:Loc.t -> by:transport -> unit -> t
    val src : t -> Loc.t
    val dst : t -> Loc.t
    val by : t -> transport
    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end

  module Link : Link_sig

  module type Map_sig = sig
    type t

    val empty : t
    val add_loc : Loc.t -> t -> (t, [ `Duplicate ]) result
    val add_link : Link.t -> t -> (t, [ `Duplicate ]) result
    val get_links : src:Loc.t -> t -> Link.t list
    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end

  module Map : Map_sig

  module TicketSet : sig
    type t = { taxi : int; bus : int; ug : int; secret : int; double : int }

    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end

  module Move : sig
    type single =
      [ `Taxi of Loc.t | `Bus of Loc.t | `Ug of Loc.t | `Secret of Loc.t ]

    type t = [ single | `Double of single * single ]

    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end

  module Agent : sig
    type role = [ `MrX | `Police ] [@@deriving show]
    type t

    val make : loc:Loc.t -> ticket_set:TicketSet.t -> role:role -> unit -> t
    val role : t -> role
    val loc : t -> Loc.t
    val ticket_set : t -> TicketSet.t
    val move : Move.t -> t -> (t, [ `InvalidMove | `NoTicket ]) result
    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end

  module History : sig
    type t

    type entry =
      [ `Hidden of [ `Taxi | `Bus | `Ug | `Secret ] | Move.single ] option

    val make : init_locs:Loc.t list -> unit -> t
    val add : Move.t -> t -> t
    val get_view : from:Agent.role -> t -> Loc.t list * entry list

    (*
    val get : agent_id:int -> clock:int -> t -> Move.single option
    (* agent_id is 0 (Mr.X) or 1-4 (police). clock is 1 through 24. *)
    *)
    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end

  module Game : sig
    type t

    val make : init_locs:Loc.t list -> map:Map.t -> unit -> t
    val is_finished : t -> bool
    val agents : t -> Agent.t list
    val history : t -> History.t
    val turn : t -> int (* Turn 0 is Mr.X's. Turn 1 through 4 are police's. *)
    val clock : t -> int (* 1 through 24 *)
    val move_agent : Move.t -> t -> (t, [ `InvalidMove | `NoTicket ]) result
    val derive_possible_moves : t -> Move.t list
    val show : t -> string
    val to_yojson : t -> Yojson.Safe.t
    val of_yojson : Yojson.Safe.t -> t
  end
end

module type Farray_S = sig
  type 'a t

  val make : int -> 'a -> 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val sub : off:int -> len:int -> 'a t -> 'a t
  val length : 'a t -> int
  val assign : int -> 'a -> 'a t -> 'a t
  val get : int -> 'a t -> 'a
  val ( .@() ) : 'a t -> int -> 'a
  val ( .@()<- ) : 'a t -> int -> 'a -> 'a t
end

module Farray : Farray_S = struct
  type 'a t

  let make = assert false
  let of_list = assert false
  let to_list = assert false
  let sub = assert false
  let length = assert false
  let assign = assert false
  let get = assert false
  let ( .@() ) = get
  let ( .@()<- ) = assign
end

module M = struct
  module Loc : sig
    type t

    val make : id:int -> unit -> t
    val id : t -> int
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end = struct
    type t = int

    let make ~id () = id
    let id x = x
    let show x = string_of_int x
    let pp fmt x = Format.pp_print_int fmt x

    let of_yojson x =
      Yojson.Safe.Util.to_int_option x
      |> Option.to_result ~none:"invalid loc yojson"

    let to_yojson x = `Int x
  end

  module Link : sig
    type t
    type transport = [ `Taxi | `Bus | `Ug | `Boat ]

    val make : src:Loc.t -> dst:Loc.t -> by:transport -> unit -> t
    val src : t -> Loc.t
    val dst : t -> Loc.t
    val by : t -> transport
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end = struct
    type transport = [ `Taxi | `Bus | `Ug | `Boat ] [@@deriving show, yojson]

    type t = { src : Loc.t; dst : Loc.t; by : transport }
    [@@deriving show, yojson]

    let make ~src ~dst ~by () = { src; dst; by }
    let src { src; _ } = src
    let dst { dst; _ } = dst
    let by { by; _ } = by
  end

  module Map : sig
    type t

    val empty : t
    val add_link : Link.t -> t -> (t, [ `Duplicate ]) result
    val get_links : src:Loc.t -> t -> Link.t list
    val show : t -> string
    val pp : Format.formatter -> t -> unit
  end = struct
    type t = { links : Link.t list Int_map.t [@opaque] } [@@deriving show]

    let empty = { links = Int_map.empty }

    let add_link link map =
      Ok
        {
          links =
            map.links
            |> Int_map.update
                 (link |> Link.src |> Loc.id)
                 (function
                   | None -> Some [] | Some links -> Some (link :: links));
        }

    let get_links ~src map =
      map.links |> Int_map.find_opt (Loc.id src) |> Option.value ~default:[]
  end

  module Ticket_set : sig
    type t = { taxi : int; bus : int; ug : int; secret : int; double : int }

    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end = struct
    type t = { taxi : int; bus : int; ug : int; secret : int; double : int }
    [@@deriving show, yojson]
  end

  module rec Move : sig
    type single =
      [ `Taxi of Loc.t | `Bus of Loc.t | `Ug of Loc.t | `Secret of Loc.t ]

    type single_with_hidden = [ single | `Hidden of Ticket.t ] [@@deriving show]
    type t = [ single | `Double of single * single ]

    type t_with_hidden =
      [ single_with_hidden | `Double of single_with_hidden * single_with_hidden ]

    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
    val single_of_yojson : Yojson.Safe.t -> (single, string) result
    val single_to_yojson : single -> Yojson.Safe.t
  end = struct
    type single =
      [ `Taxi of Loc.t | `Bus of Loc.t | `Ug of Loc.t | `Secret of Loc.t ]
    [@@deriving show, yojson]

    type single_with_hidden = [ single | `Hidden of Ticket.t ]
    [@@deriving show, yojson]

    type t = [ single | `Double of single * single ] [@@deriving show, yojson]

    type t_with_hidden =
      [ single_with_hidden | `Double of single_with_hidden * single_with_hidden ]
    [@@deriving show]
  end

  and Ticket : sig
    type t = [ `Taxi | `Bus | `Ug | `Secret ]

    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val of_move_single : Move.single -> t
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end = struct
    type t = [ `Taxi | `Bus | `Ug | `Secret ] [@@deriving show, yojson]

    let of_move_single = function
      | `Taxi _ -> `Taxi
      | `Bus _ -> `Bus
      | `Ug _ -> `Ug
      | `Secret _ -> `Secret
  end

  module Agent : sig
    type role = [ `MrX | `Police ] [@@deriving show]
    type t

    val make :
      loc:Loc.t ->
      ticket_set:Ticket_set.t ->
      role:role ->
      map:Map.t ->
      unit ->
      t

    val role : t -> role
    val loc : t -> Loc.t
    val ticket_set : t -> Ticket_set.t
    val move : Move.t -> t -> (t, [ `NoSuchLink | `NoTicket ]) result
    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val of_yojson : map:Map.t -> Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end = struct
    type role = [ `MrX | `Police ] [@@deriving show, yojson]

    type t = {
      loc : Loc.t;
      ticket_set : Ticket_set.t;
      role : role;
      map : Map.t;
    }
    [@@deriving show]

    let of_yojson ~map j =
      let ( let* ) = Result.bind in
      let m = Yojson.Safe.Util.to_assoc j in
      let* loc =
        List.assoc_opt "loc" m |> Option.to_result ~none:"invalid loc"
      in
      let* loc = Loc.of_yojson loc in
      let* ticket_set =
        List.assoc_opt "ticket_set" m
        |> Option.to_result ~none:"invalid ticket_set"
      in
      let* ticket_set = Ticket_set.of_yojson ticket_set in
      let* role =
        List.assoc_opt "role" m |> Option.to_result ~none:"invalid role"
      in
      let* role = role_of_yojson role in
      Ok { loc; ticket_set; role; map }

    let to_yojson t =
      `Assoc
        [
          ("loc", Loc.to_yojson t.loc);
          ("ticket_set", Ticket_set.to_yojson t.ticket_set);
          ("role", role_to_yojson t.role);
        ]

    let make ~loc ~ticket_set ~role ~map () = { loc; ticket_set; role; map }
    let role t = t.role
    let loc t = t.loc
    let ticket_set t = t.ticket_set

    let single_move (move : Move.single) t =
      let dst, by =
        match move with
        | `Taxi loc -> (loc, `Taxi)
        | `Bus loc -> (loc, `Bus)
        | `Ug loc -> (loc, `Ug)
        | `Secret loc -> (loc, `Secret)
      in
      match
        (* Check if a link exists correctly from `t.loc to `dst` by `by` *)
        t.map |> Map.get_links ~src:t.loc
        |> List.find_opt (fun l ->
               Link.dst l = dst
               &&
               match by with
               | #Link.transport as by when Link.by l = by -> true
               | _ ->
                   (* a secret ticket can be used for any transport including boats *)
                   by = `Secret)
      with
      | None -> Error `NoSuchLink
      | Some _ ->
          (* Check if a ticket for `by` exists *)
          (match by with
          | `Taxi when t.ticket_set.taxi > 0 ->
              Ok { t.ticket_set with taxi = t.ticket_set.taxi - 1 }
          | `Bus when t.ticket_set.bus > 0 ->
              Ok { t.ticket_set with bus = t.ticket_set.bus - 1 }
          | `Ug when t.ticket_set.ug > 0 ->
              Ok { t.ticket_set with ug = t.ticket_set.ug - 1 }
          | `Secret when t.ticket_set.secret > 0 ->
              Ok { t.ticket_set with secret = t.ticket_set.secret - 1 }
          | _ -> Error `NoTicket)
          |> Result.map (fun ticket_set -> { t with ticket_set })

    let move (move : Move.t) t =
      match move with
      | #Move.single as m -> single_move m t
      | `Double (first, second) ->
          let ( >>= ) = Result.bind in
          single_move first t >>= single_move second
  end

  module History : sig
    type t

    val make : init_locs:Loc.t list -> unit -> t
    val add : Move.t -> t -> t

    val get_view :
      from:Agent.role -> t -> Loc.t option list * Move.t_with_hidden list

    val show : t -> string
    val pp : Format.formatter -> t -> unit
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end = struct
    type t = { init_locs : Loc.t list; moves : Move.t list }
    [@@deriving show, yojson]

    let make ~init_locs () = { init_locs; moves = [] }
    let add move t = { t with moves = move :: t.moves }
    let mask_init_locs t = None :: (List.tl t.init_locs |> List.map Option.some)

    let mask_move ~turn ~clock (move : Move.single) : Move.single_with_hidden =
      if
        turn = 0
        (* Mr.X *)
        && (clock = 3 || clock = 8 || clock = 13 || clock = 18 || clock = 24)
        || turn <> 0 (* Police *)
      then (move :> Move.single_with_hidden)
      else `Hidden (Ticket.of_move_single move)

    let mask_moves t =
      (* Mask each move in t.moves *)
      let num_agents = List.length t.init_locs in
      let aux ((acc : Move.t_with_hidden list), (turn, clock)) move =
        let acc, clock =
          match move with
          | #Move.single as move ->
              let masked_move = mask_move ~turn ~clock move in
              ((masked_move :> Move.t_with_hidden) :: acc, clock)
          | `Double (first, second) ->
              assert (turn = 0 (* Mr.X *));
              let masked_first = mask_move ~turn ~clock first in
              let clock = clock + 1 in
              let masked_second = mask_move ~turn ~clock second in
              (`Double (masked_first, masked_second) :: acc, clock)
        in
        ( acc,
          ( (turn + 1) mod num_agents,
            if turn + 1 = num_agents then clock + 1 else clock ) )
      in
      t.moves |> List.fold_left aux ([], (0, 1))

    let get_view ~from t =
      match from with
      | `MrX ->
          ( t.init_locs |> List.map Option.some,
            (t.moves :> Move.t_with_hidden list) )
      | `Police -> (mask_init_locs t, fst (mask_moves t))
  end

  module Game : sig
    type t

    val make : init_locs:Loc.t list -> map:Map.t -> unit -> t
    val is_finished : t -> bool
    val agents : t -> Agent.t list
    val history : t -> History.t
    val turn : t -> int (* Turn 0 is Mr.X's. Turn 1 through 4 are police's. *)
    val clock : t -> int (* 1 through 24 *)
    val move_agent : Move.t -> t -> (t, [ `NoSuchLink | `NoTicket ]) result
    val skip_turn : t -> (t, [ `CantSkip ]) result
    val derive_possible_moves : t -> Move.t list
    val show : t -> string
    val of_yojson : Yojson.Safe.t -> (t, string) result
    val to_yojson : t -> Yojson.Safe.t
  end = struct
    type t = {
      history : History.t;
      agents : Agent.t Farray.t; [@opaque]
      map : Map.t;
      turn : int;
      clock : int;
      is_finished : bool;
    }
    [@@deriving show]

    let of_yojson = assert false
    let to_yojson = assert false
    let is_finished t = t.is_finished

    let make ~init_locs ~map () =
      let history = History.make ~init_locs () in
      let agents =
        init_locs
        |> List.mapi (fun i loc ->
               let ticket_set =
                 let open Ticket_set in
                 if i = 0 then
                   { taxi = 12; bus = 0; ug = 4; secret = 5; double = 2 }
                 else { taxi = 10; bus = 8; ug = 4; secret = 0; double = 0 }
               in
               let role = if i = 0 then `MrX else `Police in
               Agent.make ~loc ~ticket_set ~role ~map ())
        |> Farray.of_list
      in
      { history; agents; map; turn = 0; clock = 1; is_finished = false }

    let agents t = Farray.to_list t.agents
    let history t = t.history
    let turn t = t.turn
    let clock t = t.clock

    let move_agent move t =
      let ( let* ) = Result.bind in
      let* agent =
        let a = t.agents.Farray.@(t.turn) in
        Agent.move move a
      in
      let t =
        let num_agents = Farray.length t.agents in
        {
          t with
          turn = (t.turn + 1) mod num_agents;
          clock = (if t.turn + 1 = num_agents then t.clock + 1 else t.clock);
          agents = t.agents.Farray.@(t.turn) <- agent;
        }
      in
      Ok t

    let derive_possible_moves t =
      let a = t.agents.Farray.@(t.turn) in
      t.map
      |> Map.get_links ~src:(Agent.loc a)
      |> List.filter_map (fun l ->
             let dst = Link.dst l in
             let move =
               match Link.by l with
               | `Taxi -> `Taxi dst
               | `Bus -> `Bus dst
               | `Ug -> `Ug dst
               | `Boat -> `Secret dst
             in
             match a |> Agent.move move with
             | Ok _ -> Some move
             | Error _ -> None)

    let skip_turn t =
      if derive_possible_moves t = [] then Error `CantSkip
      else
        let t =
          let num_agents = Farray.length t.agents in
          {
            t with
            turn = (t.turn + 1) mod num_agents;
            clock = (if t.turn + 1 = num_agents then t.clock + 1 else t.clock);
          }
        in
        Ok t
  end
end

module Test (Yrksp : S) = struct
  open Yrksp

  let map =
    let loc id = Map.add_loc (Loc.make ~id ()) in
    let link src by dst =
      Map.add_link
        (Link.make ~src:(Loc.make ~id:src ()) ~dst:(Loc.make ~id:dst ()) ~by ())
    in
    let ( >>= ) = Result.bind in
    List.init 198 (fun i -> i + 1)
    |> List.fold_left (fun m i -> m >>= loc i) (Ok Map.empty)
    >>= link 1 `Taxi 8 >>= link 1 `Taxi 9 >>= link 1 `Bus 58 >>= link 1 `Bus 46
    >>= link 1 `Ug 46 >>= link 115 `Boat 108 >>= link 108 `Bus 105
    >>= link 115 `Taxi 127 |> Result.get_ok

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
    List.combine
      (Game.agents g |> List.tl |> List.map Agent.loc)
      init_police_locs
    |> List.iter (fun (g, e) -> assert (g = e));

    assert (Game.agents g |> List.length = 6);
    assert (Game.turn g = 0);
    assert (Game.clock g = 1);

    let ts = Game.agents g |> List.hd |> Agent.ticket_set in
    assert (
      ts.taxi = 12 && ts.bus = 9 && ts.ug = 4 && ts.secret = 5 && ts.double = 2);
    Game.agents g |> List.tl
    |> List.iter (fun a ->
           let ts = Agent.ticket_set a in
           assert (
             ts.taxi = 10 && ts.bus = 8 && ts.ug = 4 && ts.secret = 0
             && ts.double = 0));

    let new_mr_x_loc = Loc.make ~id:156 () in
    let g = Game.move_agent (`Taxi new_mr_x_loc) g |> Result.get_ok in
    assert (Game.turn g = 1);
    assert (Game.clock g = 1);
    assert (Game.agents g |> List.hd |> Agent.loc = new_mr_x_loc);
    assert ((Game.agents g |> List.hd |> Agent.ticket_set).taxi = 11);
    assert (
      Game.history g
      |> History.get_view ~from:`MrX
      = (init_locs, [ Some (`Taxi new_mr_x_loc) ]));
    ()

  let test_invalid_move () =
    let g =
      Game.make
        ~init_locs:[ Loc.make ~id:115 (); Loc.make ~id:1 (); Loc.make ~id:8 () ]
        ~map ()
    in
    let g = Game.move_agent (`Taxi (Loc.make ~id:127 ())) g |> Result.get_ok in
    let res = Game.move_agent (`Taxi (Loc.make ~id:8 ())) g in
    assert (Result.is_error res);
    ()

  let test_game_finished () =
    let g =
      Game.make ~init_locs:[ Loc.make ~id:9 (); Loc.make ~id:8 () ] ~map ()
    in
    assert (not (Game.is_finished g));
    let g = Game.move_agent (`Taxi (Loc.make ~id:1 ())) g |> Result.get_ok in
    assert (not (Game.is_finished g));
    let g = Game.move_agent (`Taxi (Loc.make ~id:1 ())) g |> Result.get_ok in
    assert (Game.is_finished g);
    ()

  let test_use_boat () =
    let init_locs = [ Loc.make ~id:115 () ] in
    let g = Game.make ~init_locs ~map () in
    let new_loc = Loc.make ~id:108 () in
    let g = Game.move_agent (`Secret new_loc) g |> Result.get_ok in
    assert (Game.agents g |> List.hd |> Agent.loc = new_loc);
    assert ((Game.agents g |> List.hd |> Agent.ticket_set).secret = 4);
    assert (
      Game.history g
      |> History.get_view ~from:`MrX
      = (init_locs, [ Some (`Secret new_loc) ]));
    ()

  let test_open_locs () =
    let init_locs = [ Loc.make ~id:115 (); Loc.make ~id:1 () ] in
    let g =
      let ( >>= ) = Result.bind in
      Game.make ~init_locs ~map ()
      |> Result.ok
      >>= Game.move_agent (`Taxi (Loc.make ~id:114 ()))
      >>= Game.move_agent (`Taxi (Loc.make ~id:8 ()))
      >>= Game.move_agent (`Taxi (Loc.make ~id:115 ()))
      >>= Game.move_agent (`Taxi (Loc.make ~id:1 ()))
      >>= Game.move_agent (`Taxi (Loc.make ~id:114 ()))
      |> Result.get_ok
    in
    assert (
      match Game.history g |> History.get_view ~from:`Police |> snd with
      | [
       Some (`Hidden `Taxi);
       Some (`Taxi _);
       Some (`Hidden `Taxi);
       Some (`Taxi _);
       Some (`Taxi last_loc);
      ]
        when last_loc = Loc.make ~id:114 () ->
          true
      | _ -> false);
    ()

  let test_use_double_move () =
    let init_locs = [ Loc.make ~id:115 (); Loc.make ~id:1 () ] in
    let g = Game.make ~init_locs ~map () in
    let new_loc1 = Loc.make ~id:108 () in
    let new_loc2 = Loc.make ~id:105 () in
    let g =
      Game.move_agent (`Double (`Secret new_loc1, `Bus new_loc2)) g
      |> Result.get_ok
    in
    assert (Game.turn g = 1);
    assert (Game.clock g = 2);
    assert (Game.agents g |> List.hd |> Agent.loc = new_loc2);
    assert ((Game.agents g |> List.hd |> Agent.ticket_set).secret = 4);
    assert ((Game.agents g |> List.hd |> Agent.ticket_set).bus = 8);
    assert (
      Game.history g
      |> History.get_view ~from:`MrX
      = (init_locs, [ Some (`Secret new_loc1); None; Some (`Bus new_loc2) ]));

    let new_loc = Loc.make ~id:8 () in
    let g = Game.move_agent (`Taxi new_loc) g |> Result.get_ok in
    assert (
      Game.history g
      |> History.get_view ~from:`MrX
      = ( init_locs,
          [
            Some (`Secret new_loc1);
            None;
            Some (`Bus new_loc2);
            Some (`Taxi new_loc);
          ] ));

    ()

  let get_api_v1_game ~game_id:_ =
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
    let g = serialized_game |> Yojson.Safe.from_string |> Game.of_yojson in
    let history_entry_to_yojson = function
      | None -> `Null
      | Some (`Hidden v) ->
          `List [ `String "hidden"; `String (move_single_to_string' v) ]
      | Some ((`Taxi loc | `Bus loc | `Ug loc | `Secret loc) as v) ->
          `List [ `String (move_single_to_string v); `Int (Loc.id loc) ]
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
        (`List (init_locs |> List.map (fun l -> `Int (Loc.id l)))
        :: (moves |> split_list num_agents
           |> List.map (List.map history_entry_to_yojson)
           |> List.map (fun xs -> `List xs)))
    in
    let move_to_yojson (move : Move.t) : Yojson.Safe.t =
      let rec aux : Move.t -> Yojson.Safe.t = function
        | (`Taxi loc | `Bus loc | `Ug loc | `Secret loc) as x ->
            `List [ `String (move_single_to_string x); `Int (Loc.id loc) ]
        | `Double (first, second) ->
            `List
              [
                `String "double"; aux (first :> Move.t); aux (second :> Move.t);
              ]
      in
      aux move
    in
    let _ : Yojson.Safe.t =
      `Assoc
        [
          ("turn", `Int (Game.turn g));
          ("clock", `Int (Game.clock g));
          ("is_finished", `Bool (Game.is_finished g));
          ("agents", `List (g |> Game.agents |> List.map agent_to_yojson));
          ( "history",
            g |> Game.history
            |> history_to_yojson
                 ~from:(if Game.is_finished g || is_mr_x then `MrX else `Police)
                 ~num_agents:(g |> Game.agents |> List.length) );
          ( "possible_moves",
            `List (g |> Game.derive_possible_moves |> List.map move_to_yojson)
          );
        ]
    in
    ()

  (* FIXME test skip *)
end
