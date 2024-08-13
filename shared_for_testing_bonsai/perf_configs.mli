open! Core
open Bonsai_perf_shared

module Dynamic_num : sig
  type t =
    | Let_arr
    | Map
    | Assoc_simple
    | Assoc
  [@@deriving compare, sexp_of, enumerate]

  include
    Config
    with type t := t
     and type input = float Opaque_map.t
     and type output = float
     and type action = Nothing.t

  val startup_inputs : (string * float Opaque_map.t) list
  val scenarios : (float Opaque_map.t, Nothing.t) Scenario.t list
end

module Switch : sig
  type t =
    | Arr_then_match of { uses_state : bool }
    | Match_sub of { uses_state : bool }
  [@@deriving compare, sexp_of, enumerate]

  include
    Config
    with type t := t
     and type input = bool
     and type output = string
     and type action = Nothing.t

  val startup_inputs : (string * bool) list
  val scenarios : (bool, Nothing.t) Scenario.t list
end
