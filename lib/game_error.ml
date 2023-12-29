type t = string

let to_string = Fun.id
let duplicate_link = Error "duplicate link"
let no_link ~src ~dst = Error (Printf.sprintf "no link from %d to %d" src dst)
let no_ticket = Error "no ticket"
let someone_is_already_there = Error "someone is already there"
let can't_skip = Error "can't skip"
