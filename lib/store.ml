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
  encrypted_password TEXT NOT NULL,
  access_token TEXT NOT NULL,

  UNIQUE (room_uuid, turn),
  UNIQUE (room_uuid, name),
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
    (Caqti_type.(t3 string string string) ->. Caqti_type.unit)
      {|UPDATE room SET game = ? WHERE uuid = ? AND game = ?|}

  let insert_user =
    (Caqti_type.(t5 string int string string string) ->. Caqti_type.unit)
      {|INSERT INTO user (room_uuid, turn, name, encrypted_password, access_token) VALUES (?, ?, ?, ?, ?)|}

  let select_user_by_room_uuid_and_name =
    (Caqti_type.(t2 string string) ->! Caqti_type.(t3 int string string))
      {|SELECT turn, encrypted_password, access_token FROM user WHERE room_uuid = ? AND name = ?|}

  let select_users_by_room_uuid =
    (Caqti_type.string ->* Caqti_type.(t2 int string))
      {|SELECT turn, name FROM user WHERE room_uuid = ? ORDER BY turn ASC|}

  let select_user_by_access_token =
    (Caqti_type.string ->! Caqti_type.int)
      {|SELECT turn FROM user WHERE access_token = ?|}
end

let create_table_room t = t |> exec Q.create_table_room ()
let create_table_user t = t |> exec Q.create_table_user ()
let insert_room ~uuid ~name ~game t = t |> exec Q.insert_room (uuid, name, game)
let select_rooms t = t |> collect_list Q.select_rooms ()
let select_room_by_uuid ~uuid t = t |> find Q.select_room_by_uuid uuid
let select_game_by_uuid ~uuid t = t |> find Q.select_game_by_uuid uuid

let update_game_by_uuid ~uuid ~old_game ~new_game t =
  t |> exec Q.update_game_by_uuid (new_game, uuid, old_game)

let insert_user ~room_uuid ~turn ~name ~encrypted_password ~access_token t =
  t
  |> exec Q.insert_user (room_uuid, turn, name, encrypted_password, access_token)

let select_user_by_room_uuid_and_name ~room_uuid ~name t =
  t |> find Q.select_user_by_room_uuid_and_name (room_uuid, name)

let select_users_by_room_uuid ~room_uuid t =
  t |> collect_list Q.select_users_by_room_uuid room_uuid

let select_user_by_access_token ~access_token t =
  t |> find Q.select_user_by_access_token access_token

let create_room ~room_name ~user_name ~encrypted_user_password ~room_id
    ~access_token ~game t =
  t
  |> use @@ fun c ->
     let (module Db : Caqti_eio.CONNECTION) = c in
     match Db.start () with
     | Error e -> Error e
     | Ok () -> (
         try
           Db.exec Q.insert_room (room_id, room_name, game) |> Result.get_ok;
           Db.exec Q.insert_user
             (room_id, 0, user_name, encrypted_user_password, access_token)
           |> Result.get_ok;
           Db.commit () |> Result.get_ok;
           Ok ()
         with _ -> Db.rollback ())
