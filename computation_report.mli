open! Core
open Bonsai_perf_shared

module Incr_report : sig
  type t =
    { node_count : int
    ; nodes_created : int
    ; max_height : int
    ; max_node_id : int
    ; annotated_counts_diff : Bonsai.Private.Annotate_incr.Counts.t
    }
  [@@deriving sexp_of]

  val measure : (unit -> 'a) -> 'a * t
end

module Startup : sig
  val run
    :  (Bonsai.graph -> 'a Bonsai.t)
    -> Bonsai.Private.Skeleton.Counts.t * Incr_report.t

  val print_many
    :  (string, Bonsai.Private.Skeleton.Counts.t * Incr_report.t) Base.List.Assoc.t
    -> unit

  val run_and_print_compare
    :  (module Config with type t = 'conf and type input = 'input and type action = 'action)
    -> (string * 'input) list
    -> 'conf list
    -> unit
end

module Interaction : sig
  val run
    :  get_inject:('r -> 'action -> unit Ui_effect.t)
    -> (Bonsai.graph -> 'r Bonsai.t)
    -> 'action Interaction.Finalized.t list
    -> Incr_report.t

  val run_and_print_compare
    :  (module Config with type t = 'conf and type input = 'input and type action = 'action)
    -> ('input, 'action) Scenario.t list
    -> 'conf list
    -> unit
end
