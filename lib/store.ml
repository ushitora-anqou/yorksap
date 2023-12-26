type config = { dsn : string }
type 'a t = { pool : (Caqti_eio.connection, Caqti_error.t) Caqti_eio.Pool.t }

let stringify_error x = x |> Result.map_error Caqti_error.show

let connect env ~sw { dsn } =
  let ( let* ) = Result.bind in
  let* pool =
    Uri.of_string dsn
    |> Caqti_eio_unix.connect_pool ~sw ~stdenv:(env :> Caqti_eio.stdenv)
    |> stringify_error
  in
  Ok { pool }

let use f t = t.pool |> Caqti_eio.Pool.use f |> stringify_error

let find query arg t =
  t
  |> use (fun c ->
         let (module Db : Caqti_eio.CONNECTION) = c in
         Db.find query arg)

let exec query arg t =
  t
  |> use (fun c ->
         let (module Db : Caqti_eio.CONNECTION) = c in
         Db.exec query arg)

let collect_list query arg t =
  t
  |> use (fun c ->
         let (module Db : Caqti_eio.CONNECTION) = c in
         Db.collect_list query arg)

module Q = struct
  open Caqti_request.Infix

  let create_table_room =
    (Caqti_type.unit ->. Caqti_type.unit)
      {|
CREATE TABLE IF NOT EXISTS room (
  uuid BLOB PRIMARY KEY,
  name BLOB,
  game BLOB
) STRICT
|}

  let insert_room =
    (Caqti_type.(t3 octets octets octets) ->. Caqti_type.unit)
      {|INSERT INTO room (uuid, name, game) VALUES (?, ?, ?)|}

  let select_rooms =
    (Caqti_type.unit ->* Caqti_type.(t2 octets octets))
      {|SELECT uuid, name FROM room|}

  let select_room_by_uuid =
    (Caqti_type.octets ->! Caqti_type.(t2 octets octets))
      {|SELECT uuid, name FROM room WHERE uuid = ?|}
end

let create_table_room t = t |> exec Q.create_table_room ()
let insert_room ~uuid ~name ~game t = t |> exec Q.insert_room (uuid, name, game)
let select_rooms t = t |> collect_list Q.select_rooms ()
let select_room_by_uuid ~uuid t = t |> find Q.select_room_by_uuid uuid
