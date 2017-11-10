
type t =
  | Evaled of elem list
  | ToEval of (unit -> t list)

and elem =
  | Element of string * t
