
type main_error =
  [
    | `Bug
    | `Message     of string
    | `SystemError of string
  ]

type error = [ main_error | Otfm.error ]

let ( >>= ) x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(e :> error)
(*
let ( >>=@ ) x f =
  match x with
  | Ok(v)    -> f v
  | Error(e) -> Error(OtfmError(e))
*)
let return v = Ok(v)

let err e = Error(e)

let report_result (r : (unit, error) result) : unit =
  match r with
  | Ok(())   -> ()
  | Error(e) ->
      match e with
      | #Otfm.error as oe -> Otfm.pp_error Format.std_formatter oe
      | `Bug              -> print_endline "bug"
      | `Message(msg)     -> print_endline msg
      | `SystemError(msg) -> print_endline msg


type tree =
  | Evaled of elem list
  | ToEval of (unit -> (elem list, error) result)

and elem =
  | Element of string * tree
