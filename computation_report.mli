open! Core
open Bonsai_perf_shared

module Incr_report : sig
  type t =
    { node_count : int
    ; nodes_created : int
    ; nodes_recomputed : int
    ; nodes_invalidated : int
    ; max_height : int
    ; max_node_id : int
    ; annotated_counts_diff : Bonsai.Private.Annotate_incr.Counts.t
    }
  [@@deriving sexp_of]

  val measure : (unit -> 'a) -> 'a * t
end

module Startup : sig
  val run
    :  (local_ Bonsai.graph -> 'a Bonsai.t)
    -> Bonsai.Private.Skeleton.Counts.t * Incr_report.t

  val print_many
    :  (string, Bonsai.Private.Skeleton.Counts.t * Incr_report.t) Base.List.Assoc.t
    -> unit

  val run_and_print_compare
    :  (module Config with type t = 'conf and type input = 'input and type action = 'action)
    -> (string * 'input) list
    -> 'conf list
    -> unit

  val diff_pairs_incr_summary_only
    :  ?title:string
    -> (module Config
          with type t = 'conf
           and type input = 'input
           and type action = 'action)
    -> (string * 'input) list
    -> (string * 'conf * 'conf) list
    -> unit
end

module Interaction : sig
  val run
    :  get_inject:('r -> ('action -> unit Ui_effect.t))
    -> (local_ Bonsai.graph -> 'r Bonsai.t)
    -> 'action Interaction.Finalized.t list
    -> Incr_report.t

  (** [print_max_height] defaults to [false] b/c it's the same for all interactions
      [print_node_count] defaults to [true]
      [print_max_node_id] defaults to [false] b/c it's almost the same as [print_num_created].
      [print_num_created] defaults to [true]
      [print_num_recomputed] defaults to [true]
      [print_num_invalidated] defaults to [true]
    *)
  val run_and_print_compare
    :  ?print_max_height:bool
    -> ?print_node_count:bool
    -> ?print_max_node_id:bool
    -> ?print_num_created:bool
    -> ?print_num_recomputed:bool
    -> ?print_num_invalidated:bool
    -> ?title:string
    -> (module Config
          with type t = 'conf
           and type input = 'input
           and type action = 'action)
    -> ('input, 'action) Scenario.t list
    -> 'conf list
    -> unit

  val diff_pairs
    :  ?print_max_height:bool
    -> ?print_node_count:bool
    -> ?print_max_node_id:bool
    -> ?print_num_created:bool
    -> ?print_num_recomputed:bool
    -> ?print_num_invalidated:bool
    -> ?title:string
    -> (module Config
          with type t = 'conf
           and type input = 'input
           and type action = 'action)
    -> ('input, 'action) Scenario.t list
    -> (string * 'conf * 'conf) list
    -> unit
end
