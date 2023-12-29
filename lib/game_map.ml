open Util

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
  val add_link : Link.t -> t -> (t, Game_error.t) result
  val get_links : src:Loc.t -> t -> Link.t list
  val show : t -> string
  val pp : Format.formatter -> t -> unit
end = struct
  type t = { links : Link.t list Int_map.t [@opaque] } [@@deriving show]

  let empty = { links = Int_map.empty }

  let add_link link map =
    (* FIXME: return Game_error.duplicate_link *)
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
