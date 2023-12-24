open Util

module Error : sig
  type t

  val to_string : t -> string
  val duplicate_link : (_, t) result
  val no_link : src:int -> dst:int -> (_, t) result
  val no_ticket : (_, t) result
  val someone_is_already_there : (_, t) result
  val can't_skip : (_, t) result
end = struct
  type t = string

  let to_string = Fun.id
  let duplicate_link = Error "duplicate link"
  let no_link ~src ~dst = Error (Printf.sprintf "no link from %d to %d" src dst)
  let no_ticket = Error "no ticket"
  let someone_is_already_there = Error "someone is already there"
  let can't_skip = Error "can't skip"
end

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
  val show_transport : transport -> string
  val pp_transport : Format.formatter -> transport -> unit
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
  val add_link : Link.t -> t -> (t, Error.t) result
  val get_links : src:Loc.t -> t -> Link.t list
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end = struct
  type t = { links : Link.t list Int_map.t [@opaque] } [@@deriving show]

  let empty = { links = Int_map.empty }

  let add_link link map =
    (* FIXME: return Error.duplicate_link *)
    Ok
      {
        links =
          map.links
          |> Int_map.update
               (link |> Link.src |> Loc.id)
               (function
                 | None -> Some [ link ] | Some links -> Some (link :: links));
      }

  let get_links ~src map =
    map.links |> Int_map.find_opt (Loc.id src) |> Option.value ~default:[]
end

module Ticket_set : sig
  type t = { taxi : int; bus : int; ug : int; secret : int; double : int }

  val default_for_mr_x : t
  val default_for_police : t
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val of_yojson : Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end = struct
  type t = { taxi : int; bus : int; ug : int; secret : int; double : int }
  [@@deriving show, yojson]

  let default_for_mr_x = { taxi = 12; bus = 9; ug = 4; secret = 5; double = 2 }

  let default_for_police =
    { taxi = 10; bus = 8; ug = 4; secret = 0; double = 0 }
end

module rec Move : sig
  type single =
    [ `Taxi of Loc.t | `Bus of Loc.t | `Ug of Loc.t | `Secret of Loc.t ]

  type single_with_hidden = [ single | `Hidden of Ticket.t ] [@@deriving show]
  type t = [ single | `Double of single * single ]

  type t_with_hidden =
    [ single_with_hidden | `Double of single_with_hidden * single_with_hidden ]

  val get_dst : Move.t -> Loc.t
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val show_t_with_hidden : t_with_hidden -> string
  val pp_t_with_hidden : Format.formatter -> t_with_hidden -> unit
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

  let rec get_dst : Move.t -> Loc.t = function
    | `Taxi loc -> loc
    | `Bus loc -> loc
    | `Ug loc -> loc
    | `Secret loc -> loc
    | `Double (_, second) -> get_dst (second :> Move.t)
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
    loc:Loc.t -> ticket_set:Ticket_set.t -> role:role -> map:Map.t -> unit -> t

  val role : t -> role
  val loc : t -> Loc.t
  val ticket_set : t -> Ticket_set.t
  val move : Move.t -> t -> (t, Error.t) result
  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val of_yojson : map:Map.t -> Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end = struct
  type role = [ `MrX | `Police ] [@@deriving show, yojson]

  type t = { loc : Loc.t; ticket_set : Ticket_set.t; role : role; map : Map.t }
  [@@deriving show]

  let of_yojson ~map j =
    let ( let* ) = Result.bind in
    let m = Yojson.Safe.Util.to_assoc j in
    let* loc = List.assoc_opt "loc" m |> Option.to_result ~none:"invalid loc" in
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
    | None -> Error.no_link ~src:(Loc.id t.loc) ~dst:(Loc.id dst)
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
        | _ -> Error.no_ticket)
        |> Result.map (fun ticket_set -> { t with loc = dst; ticket_set })

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
    t.moves |> List.rev |> List.fold_left aux ([], (0, 1)) |> fst |> List.rev

  let get_view ~from t =
    match from with
    | `MrX ->
        ( t.init_locs |> List.map Option.some,
          (t.moves :> Move.t_with_hidden list) |> List.rev )
    | `Police -> (mask_init_locs t, mask_moves t)
end

module Game : sig
  type t

  val make : init_locs:Loc.t list -> map:Map.t -> unit -> t
  val is_finished : t -> bool
  val agents : t -> Agent.t list
  val history : t -> History.t
  val turn : t -> int (* Turn 0 is Mr.X's. Turn 1 through 4 are police's. *)
  val clock : t -> int (* 1 through 24 *)
  val move_agent : Move.t -> t -> (t, Error.t) result
  val skip_turn : t -> (t, Error.t) result
  val derive_possible_moves : t -> Move.t list
  val show : t -> string
  val of_yojson : map:Map.t -> Yojson.Safe.t -> (t, string) result
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

  let of_yojson ~map j =
    let ( let* ) = Result.bind in
    let m = Yojson.Safe.Util.to_assoc j in
    let* history =
      m |> List.assoc_opt "history" |> Option.to_result ~none:"invalid history"
    in
    let* history = History.of_yojson history in
    let* agents =
      m |> List.assoc_opt "agents"
      |> Option.map Yojson.Safe.Util.to_list
      |> Option.to_result ~none:"invalid agents"
    in
    let* agents =
      agents
      |> List.fold_left
           (fun acc j ->
             let* acc = acc in
             let* agent = Agent.of_yojson ~map j in
             Ok (agent :: acc))
           (Ok [])
      |> Result.map (fun xs -> xs |> List.rev |> Farray.of_list)
    in
    let* turn =
      Option.bind (m |> List.assoc_opt "turn") Yojson.Safe.Util.to_int_option
      |> Option.to_result ~none:"invalid turn"
    in
    let* clock =
      Option.bind (m |> List.assoc_opt "clock") Yojson.Safe.Util.to_int_option
      |> Option.to_result ~none:"invalid turn"
    in
    let* is_finished =
      Option.bind
        (m |> List.assoc_opt "is_finished")
        Yojson.Safe.Util.to_bool_option
      |> Option.to_result ~none:"invalid turn"
    in
    Ok { history; agents; map; turn; clock; is_finished }

  let to_yojson t =
    `Assoc
      [
        ("history", History.to_yojson t.history);
        ( "agents",
          `List (t.agents |> Farray.to_list |> List.map Agent.to_yojson) );
        ("turn", `Int t.turn);
        ("clock", `Int t.clock);
        ("is_finished", `Bool t.is_finished);
      ]

  let is_finished t = t.is_finished

  let make ~init_locs ~map () =
    let history = History.make ~init_locs () in
    let agents =
      init_locs
      |> List.mapi (fun i loc ->
             let ticket_set =
               if i = 0 then Ticket_set.default_for_mr_x
               else Ticket_set.default_for_police
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

  let has_finished t =
    t.is_finished
    ||
    (* Check if the current loc of Mr.X is the same as one of the policemen *)
    let mr_x_loc = t.agents.Farray.@(0) |> Agent.loc in
    t.agents
    |> Farray.fold_lefti
         (fun i b agent ->
           b || if i = 0 then false else b || Agent.loc agent = mr_x_loc)
         false
  (* FIXME: check if Mr.X cannot move anywhere *)

  let move_agent move t =
    let ( let* ) = Result.bind in
    let* () =
      (* Check if anyone is not at the destination *)
      if
        let dst = Move.get_dst move in
        t.agents
        |> Farray.fold_lefti
             (fun i b agent ->
               b || (Agent.loc agent = dst && i <> 0 && t.turn <> 0))
             false
      then Error.someone_is_already_there
      else Ok ()
    in
    let* agent =
      let a = t.agents.Farray.@(t.turn) in
      Agent.move move a
    in
    let num_agents = Farray.length t.agents in
    let clock =
      match move with
      | `Double _ -> t.clock + 1
      | _ when t.turn + 1 = num_agents -> t.clock + 1
      | _ -> t.clock
    in
    let t =
      {
        t with
        clock;
        turn = (t.turn + 1) mod num_agents;
        agents = t.agents.Farray.@(t.turn) <- agent;
        history = History.add move t.history;
      }
    in
    let t = { t with is_finished = has_finished t } in
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
           match a |> Agent.move move with Ok _ -> Some move | Error _ -> None)

  let skip_turn t =
    if derive_possible_moves t = [] then Error.can't_skip
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
