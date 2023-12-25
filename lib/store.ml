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

module Q = struct
  open Caqti_request.Infix

  let query = (Caqti_type.(t2 int int) ->! Caqti_type.int) "SELECT ? + ?"
end
