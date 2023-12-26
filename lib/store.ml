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
  uuid TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  game TEXT NOT NULL
) STRICT
|}

  let create_table_user =
    (Caqti_type.unit ->. Caqti_type.unit)
      {|
CREATE TABLE IF NOT EXISTS user (
  id INTEGER PRIMARY KEY,
  room_uuid TEXT NOT NULL,
  turn INT NOT NULL,
  name TEXT NOT NULL,

  UNIQUE (room_uuid, turn),
  FOREIGN KEY (room_uuid) REFERENCES room(uuid)
) STRICT
|}

  let insert_room =
    (Caqti_type.(t3 string string string) ->. Caqti_type.unit)
      {|INSERT INTO room (uuid, name, game) VALUES (?, ?, ?)|}

  let select_rooms =
    (Caqti_type.unit ->* Caqti_type.(t2 string string))
      {|SELECT uuid, name FROM room|}

  let select_room_by_uuid =
    (Caqti_type.string ->! Caqti_type.(t2 string string))
      {|SELECT uuid, name FROM room WHERE uuid = ?|}

  let select_game_by_uuid =
    (Caqti_type.string ->! Caqti_type.string)
      {|SELECT game FROM room WHERE uuid = ?|}

  let update_game_by_uuid =
    (Caqti_type.(t2 string string) ->. Caqti_type.unit)
      {|UPDATE room SET game = ? WHERE uuid = ?|}

  let insert_user =
    (Caqti_type.(t3 string int string) ->. Caqti_type.unit)
      {|INSERT INTO user (room_uuid, turn, name) VALUES (?, ?, ?)|}

  let select_users_by_room_uuid =
    (Caqti_type.string ->* Caqti_type.(t2 int string))
      {|SELECT turn, name FROM user WHERE room_uuid = ? ORDER BY turn ASC|}
end

let create_table_room t = t |> exec Q.create_table_room ()
let create_table_user t = t |> exec Q.create_table_user ()
let insert_room ~uuid ~name ~game t = t |> exec Q.insert_room (uuid, name, game)
let select_rooms t = t |> collect_list Q.select_rooms ()
let select_room_by_uuid ~uuid t = t |> find Q.select_room_by_uuid uuid
let select_game_by_uuid ~uuid t = t |> find Q.select_game_by_uuid uuid

let update_game_by_uuid ~uuid ~game t =
  t |> exec Q.update_game_by_uuid (game, uuid)

let insert_user ~room_uuid ~turn ~name t =
  t |> exec Q.insert_user (room_uuid, turn, name)

let select_users_by_room_uuid ~room_uuid t =
  t |> collect_list Q.select_users_by_room_uuid room_uuid
