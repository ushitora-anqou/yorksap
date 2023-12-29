type t

val to_string : t -> string
val duplicate_link : (_, t) result
val no_link : src:int -> dst:int -> (_, t) result
val no_ticket : (_, t) result
val someone_is_already_there : (_, t) result
val can't_skip : (_, t) result
