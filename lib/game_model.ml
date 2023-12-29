open Util
open Game_map
module Error = Game_error

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

  let default_for_mr_x = { taxi = 12; bus = 8; ug = 4; secret = 5; double = 2 }

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

  val get_dst : Move.single -> Loc.t
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

  let get_dst : Move.single -> Loc.t = function
    | `Taxi loc -> loc
    | `Bus loc -> loc
    | `Ug loc -> loc
    | `Secret loc -> loc
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
  val add_ticket : value:int -> [ Ticket.t | `Double ] -> t -> t
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

  let add_ticket ~value ticket t =
    let ticket_set =
      match ticket with
      | `Taxi -> { t.ticket_set with taxi = t.ticket_set.taxi + value }
      | `Bus -> { t.ticket_set with bus = t.ticket_set.bus + value }
      | `Ug -> { t.ticket_set with ug = t.ticket_set.ug + value }
      | `Secret -> { t.ticket_set with secret = t.ticket_set.secret + value }
      | `Double -> { t.ticket_set with double = t.ticket_set.double + value }
    in
    { t with ticket_set }

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
        if
          not
            ((by = `Taxi && t.ticket_set.taxi > 0)
            || (by = `Bus && t.ticket_set.bus > 0)
            || (by = `Ug && t.ticket_set.ug > 0)
            || (by = `Secret && t.ticket_set.secret > 0))
        then Error.no_ticket
        else
          let t = t |> add_ticket ~value:(-1) by in
          Ok { t with loc = dst }

  let move (move : Move.t) t =
    match move with
    | #Move.single as m -> single_move m t
    | `Double (first, second) when t.ticket_set.double > 0 ->
        let ( let* ) = Result.bind in
        let* t = single_move first t in
        let* t = single_move second t in
        Ok (t |> add_ticket ~value:(-1) `Double)
    | _ -> Error.no_ticket
end

module History : sig
  type t

  val make : game_data:Game_data.t -> init_locs:Loc.t list -> unit -> t
  val add : Move.t -> t -> t

  val get_view :
    from:Agent.role -> t -> Loc.t option list * Move.t_with_hidden list

  val show : t -> string
  val pp : Format.formatter -> t -> unit
  val of_yojson : game_data:Game_data.t -> Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end = struct
  type t' = { init_locs : Loc.t list; moves : Move.t list } [@@deriving yojson]

  type t = {
    game_data : Game_data.t; [@opaque]
    init_locs : Loc.t list;
    moves : Move.t list;
  }
  [@@deriving show]

  let of_yojson ~game_data j =
    let ( let* ) = Result.bind in
    let* t' = t'_of_yojson j in
    Ok { game_data; init_locs = t'.init_locs; moves = t'.moves }

  let to_yojson t = t'_to_yojson { init_locs = t.init_locs; moves = t.moves }
  let make ~game_data ~init_locs () = { init_locs; moves = []; game_data }
  let add move t = { t with moves = move :: t.moves }
  let mask_init_locs t = None :: (List.tl t.init_locs |> List.map Option.some)

  let mask_move ~game_data ~turn ~clock (move : Move.single) :
      Move.single_with_hidden =
    if
      turn = 0
      (* Mr.X *)
      && Game_data.is_disclosure_clock clock game_data
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
            let masked_move =
              mask_move ~game_data:t.game_data ~turn ~clock move
            in
            ((masked_move :> Move.t_with_hidden) :: acc, clock)
        | `Double (first, second) ->
            assert (turn = 0 (* Mr.X *));
            let masked_first =
              mask_move ~game_data:t.game_data ~turn ~clock first
            in
            let clock = clock + 1 in
            let masked_second =
              mask_move ~game_data:t.game_data ~turn ~clock second
            in
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

  val make : ?init_locs:Loc.t list -> game_data:Game_data.t -> unit -> t
  val get_game_status : t -> [ `Continuing | `Police_won | `MrX_won ]
  val agents : t -> Agent.t list
  val history : t -> History.t
  val turn : t -> int (* Turn 0 is Mr.X's. Turn 1 through 4 are police's. *)
  val clock : t -> int (* 1 through 24 *)
  val move_agent : Move.t -> t -> (t, Error.t) result
  val skip_turn : t -> (t, Error.t) result
  val derive_possible_moves : t -> Move.t list
  val show : t -> string
  val of_yojson : game_data:Game_data.t -> Yojson.Safe.t -> (t, string) result
  val to_yojson : t -> Yojson.Safe.t
end = struct
  type t = {
    history : History.t;
    agents : Agent.t Farray.t; [@opaque]
    game_data : Game_data.t;
    turn : int;
    clock : int;
  }
  [@@deriving show]

  let of_yojson ~game_data j =
    let ( let* ) = Result.bind in
    let m = Yojson.Safe.Util.to_assoc j in
    let* history =
      m |> List.assoc_opt "history" |> Option.to_result ~none:"invalid history"
    in
    let* history = History.of_yojson ~game_data history in
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
             let* agent = Agent.of_yojson ~map:game_data.map j in
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
    Ok { history; agents; game_data; turn; clock }

  let to_yojson t =
    `Assoc
      [
        ("history", History.to_yojson t.history);
        ( "agents",
          `List (t.agents |> Farray.to_list |> List.map Agent.to_yojson) );
        ("turn", `Int t.turn);
        ("clock", `Int t.clock);
      ]

  let make ?init_locs ~game_data () =
    let init_locs =
      match init_locs with
      | Some x -> x
      | None -> game_data |> Game_data.generate_init_locs 6
    in
    let history = History.make ~game_data ~init_locs () in
    let agents =
      init_locs
      |> List.mapi (fun i loc ->
             let ticket_set =
               if i = 0 then Ticket_set.default_for_mr_x
               else Ticket_set.default_for_police
             in
             let role = if i = 0 then `MrX else `Police in
             Agent.make ~loc ~ticket_set ~role ~map:game_data.map ())
      |> Farray.of_list
    in
    { game_data; history; agents; turn = 0; clock = 1 }

  let agents t = Farray.to_list t.agents
  let history t = t.history
  let turn t = t.turn
  let clock t = t.clock

  let move_agent (move : Move.t) t =
    let ( let* ) = Result.bind in

    (* Make sure no agent exists at the destination *)
    let* () =
      if
        (*
          allowed:
            - a policeman moves to where Mr.X exists (i.turn <> 0 && i = 0)
          denied:
            - a policeman moves to where another policeman exists (i.turn <> 0 && i <> 0)
            - Mr.X moves to where a policeman exists (i.turn = 0 && i <> 0)
          don't care:
            - Mr.X moves to where Mr.X exists (i.turn = 0 && i = 0)
        *)
        let dsts =
          match move with
          | #Move.single as move -> [ Move.get_dst move ]
          | `Double (first, second) ->
              [ Move.get_dst first; Move.get_dst second ]
        in
        t.agents
        |> Farray.fold_lefti
             (fun i b agent -> b || (List.mem (Agent.loc agent) dsts && i <> 0))
             false
      then Error.someone_is_already_there
      else Ok ()
    in

    (* Move the agent *)
    let* agent =
      let a = t.agents.Farray.@(t.turn) in
      Agent.move move a
    in
    let agents = t.agents.Farray.@(t.turn) <- agent in

    (* Give Mr.X a ticket if a policeman used one *)
    let agents =
      match move with
      | #Move.single as by when t.turn <> 0 ->
          agents.Farray.@(0) <-
            agents.Farray.@(0)
            |> Agent.add_ticket ~value:1
                 (Ticket.of_move_single by :> [ Ticket.t | `Double ])
      | _ -> agents
    in

    (* Move the clock forward *)
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
        agents;
        clock;
        history = History.add move t.history;
        turn = (t.turn + 1) mod num_agents;
      }
    in
    Ok t

  let derive_possible_moves t =
    let rec enumerate_all_moves prev_move loc : Move.t list =
      let single_moves : Move.single list =
        t.game_data.map |> Map.get_links ~src:loc
        |> List.map (fun link ->
               let dst = Link.dst link in
               match Link.by link with
               | `Taxi -> [ `Taxi dst; `Secret dst ]
               | `Bus -> [ `Bus dst; `Secret dst ]
               | `Ug -> [ `Ug dst; `Secret dst ]
               | `Boat -> [ `Secret dst ])
        |> List.flatten
      in
      match prev_move with
      | Some prev_move ->
          single_moves
          |> List.map (fun (move : Move.single) -> `Double (prev_move, move))
      | None ->
          let double_moves =
            single_moves
            |> List.map (fun (move : Move.single) ->
                   let dst = Move.get_dst move in
                   enumerate_all_moves (Some move) dst)
            |> List.flatten
          in
          (single_moves :> Move.t list) @ double_moves
    in
    (* Enumerate all moves from the current loc, then filter out impossible ones *)
    let a = t.agents.Farray.@(t.turn) in
    enumerate_all_moves None (Agent.loc a)
    |> List.filter (fun move -> t |> move_agent move |> Result.is_ok)

  let skip_turn t =
    if derive_possible_moves t <> [] then Error.can't_skip
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

  let get_game_status t =
    let time_over = t.clock > 24 in
    let mr_x_arrested =
      (* Check if the current loc of Mr.X is the same as one of the policemen *)
      let mr_x_loc = t.agents.Farray.@(0) |> Agent.loc in
      t.agents
      |> Farray.fold_lefti
           (fun i b agent ->
             b || if i = 0 then false else b || Agent.loc agent = mr_x_loc)
           false
    in
    let mr_x_can't_move =
      (* If the current turn is Mr.X's, check if Mr.X can move *)
      t.turn = 0 && derive_possible_moves t = []
    in
    if time_over then `MrX_won
    else if mr_x_arrested || mr_x_can't_move then `Police_won
    else `Continuing
end
