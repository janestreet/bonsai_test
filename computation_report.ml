open! Core
open Bonsai_perf_shared

module Mini_profile = struct
  let pending = Hashtbl.create (module String)
  let times = Hashtbl.create (module String)
  let start ~label = Hashtbl.set pending ~key:label ~data:(Time_ns.now ())

  let stop ~label =
    let stop = Time_ns.now () in
    match Hashtbl.find pending label with
    | None -> failwith ("No start found for: " ^ label)
    | Some start ->
      let diff = Time_ns.diff stop start in
      Hashtbl.update times label ~f:(function
        | None -> diff
        | Some x -> Time_ns.Span.(x + diff))
  ;;

  let clear () =
    Hashtbl.clear pending;
    Hashtbl.clear times
  ;;

  let _print () =
    Hashtbl.to_alist times |> [%sexp_of: (string * Time_ns.Span.t) list] |> print_s
  ;;
end

module Incr_report = struct
  type t =
    { node_count : int
    ; nodes_created : int
    ; max_height : int
    ; max_node_id : int
    ; annotated_counts_diff : Bonsai.Private.Annotate_incr.Counts.t
    }
  [@@deriving sexp_of]

  let measure f =
    let nodes_created_before = Incremental.State.num_nodes_created Ui_incr.State.t in
    let annotated_before = Bonsai.Private.Annotate_incr.Counts.current () in
    Mini_profile.start ~label:"run f";
    let r = f () in
    Mini_profile.stop ~label:"run f";
    let nodes_created_after = Incremental.State.num_nodes_created Ui_incr.State.t in
    let annotated_after = Bonsai.Private.Annotate_incr.Counts.current () in
    let nodes_created = nodes_created_after - nodes_created_before in
    let annotated_counts =
      Bonsai.Private.Annotate_incr.Counts.diff
        ~after:annotated_after
        ~before:annotated_before
    in
    Mini_profile.start ~label:"skeleton";
    let skeleton = Incremental_skeleton.(snapshot ~normalize:true Ui_incr.State.t) in
    Mini_profile.stop ~label:"skeleton";
    Mini_profile.start ~label:"max height";
    let max_height =
      List.max_elt
        ~compare:Int.ascending
        (List.map skeleton.nodes ~f:(fun node -> node.height))
      |> Option.value_exn
    in
    Mini_profile.stop ~label:"max height";
    let node_count = List.length skeleton.nodes in
    Mini_profile.start ~label:"max node id";
    let max_node_id =
      List.max_elt
        ~compare:Int.ascending
        (List.map skeleton.nodes ~f:(fun node ->
           node.id |> Incremental.For_analyzer.Node_id.to_int))
      |> Option.value_exn
    in
    Mini_profile.stop ~label:"max node id";
    let report =
      { node_count
      ; nodes_created
      ; max_height
      ; max_node_id
      ; annotated_counts_diff = annotated_counts
      }
    in
    r, report
  ;;
end

module Startup = struct
  let run c =
    let bonsai_node_counts = Bonsai.Debug.bonsai_node_counts c in
    let driver, incr_report =
      Incr_report.measure (fun () ->
        Bonsai_driver.create
          ~time_source:(Bonsai.Time_source.create ~start:Time_ns.epoch)
          c)
    in
    Bonsai_driver.Expert.invalidate_observers driver;
    bonsai_node_counts, incr_report
  ;;

  let print_many reports =
    print_endline "======= Startup Incr Node Stats =======";
    Expectable.print_alist
      (fun (_, { Incr_report.max_height; node_count; max_node_id; nodes_created; _ }) ->
        [%sexp
          { max_height : int; node_count : int; max_node_id : int; nodes_created : int }])
      reports;
    print_endline "======= Startup Incr Annotated Node Counts =======";
    List.Assoc.map reports ~f:(fun (_, { annotated_counts_diff; _ }) ->
      annotated_counts_diff)
    |> Expectable.print_alist [%sexp_of: Bonsai.Private.Annotate_incr.Counts.t];
    print_endline "======= Bonsai Computation Nodes =======";
    List.Assoc.map reports ~f:(fun (bonsai_node_counts, _) ->
      Bonsai.Private.Skeleton.Counts.computation bonsai_node_counts)
    |> Expectable.print_alist [%sexp_of: Bonsai.Private.Skeleton.Counts.Computation.t];
    print_endline "======= Bonsai Value Nodes =======";
    List.Assoc.map reports ~f:(fun (bonsai_node_counts, _) ->
      Bonsai.Private.Skeleton.Counts.value bonsai_node_counts)
    |> Expectable.print_alist [%sexp_of: Bonsai.Private.Skeleton.Counts.Value.t]
  ;;

  let const_value_not_constant_folded x = Bonsai.Expert.Var.(value (create x))

  let run_and_print_compare
    (type conf input action)
    (module Config : Config
      with type t = conf
       and type input = input
       and type action = action)
    inputs
    configs
    =
    List.cartesian_product inputs configs
    |> List.map ~f:(fun ((input_name, input), config) ->
      let c = Config.computation config (const_value_not_constant_folded input) in
      String.uncapitalize (Config.name config) ^ ": " ^ input_name, run c)
    |> print_many
  ;;
end

module Interaction = struct
  let run ~get_inject c interactions =
    Mini_profile.start ~label:"prepare driver";
    let time_source = Bonsai.Time_source.create ~start:Time_ns.epoch in
    let driver = Bonsai_driver.create ~time_source c in
    let inject_action = Bonsai_driver.result driver |> get_inject in
    Mini_profile.stop ~label:"prepare driver";
    let (), incr_report =
      Incr_report.measure (fun () ->
        List.iter
          interactions
          ~f:
            (Interaction.Finalized.handle
               ~driver
               ~time_source
               ~inject_action
               ~handle_profile:(fun _ -> ())))
    in
    Bonsai_driver.Expert.invalidate_observers driver;
    incr_report
  ;;

  let run_and_print_compare
    (type conf input action)
    (module Config : Config
      with type t = conf
       and type input = input
       and type action = action)
    scenarios
    configs
    =
    Mini_profile.clear ();
    let reports =
      List.map scenarios ~f:(fun { Scenario.initial; test_name; interaction } ->
        let cells =
          List.map configs ~f:(fun config ->
            (* This has to happen in an inner loop, so that we get a fresh set of vars
                   for each run. *)
            let input = Input.create initial in
            let interactions =
              Interaction.finalize ~filter_profiles:true (interaction input)
            in
            let report =
              run
                ~get_inject:Config.get_inject
                (Config.computation config (Input.value input))
                interactions
            in
            String.uncapitalize (Config.name config), report)
        in
        test_name, cells)
    in
    Expect_test_helpers_base.expect_test_output () |> (ignore : string -> unit);
    let print_report_for_field ~f =
      List.Assoc.map reports ~f:(List.Assoc.map ~f)
      |> Expectable.print_alist [%sexp_of: (string * int) list]
    in
    print_endline "======= Max Height =======";
    print_report_for_field ~f:(fun { Incr_report.max_height; _ } -> max_height);
    print_endline "======= Node Count =======";
    print_report_for_field ~f:(fun { Incr_report.node_count; _ } -> node_count);
    print_endline "======= Max Node ID =======";
    print_report_for_field ~f:(fun { Incr_report.max_node_id; _ } -> max_node_id);
    print_endline "======= Nodes Created =======";
    print_report_for_field ~f:(fun { Incr_report.nodes_created; _ } -> nodes_created)
  ;;
end
