open! Core
open! Bonsai_test

let computation graph =
  let open Bonsai.Let_syntax in
  let queries, set_queries = Bonsai.state [] graph in
  let scoper, set_scoper = Bonsai.state 0 graph in
  let memo, reset =
    Bonsai.with_model_resetter
      ~f:
        (Bonsai.scope_model
           (module Int)
           ~on:scoper
           ~for_:
             (Bonsai.Memo.create
                (module Int)
                ~f:(fun input _ ->
                  let%arr input in
                  input * -1)))
      graph
  in
  let queries =
    let%arr queries in
    String.Map.of_alist_exn queries
  in
  let results =
    Bonsai.assoc
      (module String)
      queries
      ~f:(fun _key data graph ->
        let r = Bonsai.Memo.lookup ~equal:Int.equal memo data graph in
        Bonsai.both data r)
      graph
  in
  let inject =
    let%arr set_queries and reset and set_scoper in
    function
    | `Set_queries x -> set_queries x
    | `Reset -> reset
    | `Scope_model x -> set_scoper x
  in
  let result_view =
    let%arr results and memo in
    let results =
      Map.to_alist results
      |> List.map ~f:(fun (name, (input, output)) ->
        let output = Option.value_map output ~f:Int.to_string ~default:"?" in
        [%string "%{name}: %{input#Int} -> %{output}"])
      |> String.concat_lines
    in
    let query_counts =
      match Bonsai.Debug.memo_query_counts memo with
      | [] -> "No polled queries"
      | query_counts ->
        let s =
          List.map query_counts ~f:(fun (key, count) ->
            [%string "%{key#Int} (%{count#Int})"])
          |> String.concat ~sep:"; "
        in
        "Polled Queries: " ^ s
    in
    [%string "%{results}\n%{query_counts}"]
  in
  Bonsai.both result_view inject
;;

module Result_spec = struct
  type t =
    string
    * ([ `Set_queries of (string * int) list | `Reset | `Scope_model of int ]
       -> unit Effect.t)

  type incoming =
    [ `Set_queries of (string * int) list
    | `Reset
    | `Scope_model of int
    ]

  let view (x, _) = x
  let incoming (_, inject) t = inject t
end

let%expect_test "BUG: If Memo reset, but not [lookup]s, [lookup]s are permanently broken" =
  let handle = Handle.create (module Result_spec) computation in
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "three", 3 ] ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "one_dup", 1 ] ];
  Handle.show handle;
  (* We don't need to do a round-trip, because we have a result for "1" cached. *)
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1 (2); 2 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1 (2); 2 (1)
    |}];
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "three", 3 ] ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> ?
    two: 2 -> -2

    Polled Queries: 1 (2); 2 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.do_actions handle [ `Reset ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}]
;;

let%expect_test "BUG: If Memo changed via scope model, but not [lookup]s, [lookup]s are \
                 permanently broken"
  =
  let handle = Handle.create (module Result_spec) computation in
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "three", 3 ] ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "one_dup", 1 ] ];
  Handle.show handle;
  (* We don't need to do a round-trip, because we have a result for "1" cached. *)
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1 (2); 2 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    one_dup: 1 -> -1
    two: 2 -> -2

    Polled Queries: 1 (2); 2 (1)
    |}];
  Handle.do_actions handle [ `Set_queries [ "one", 1; "two", 2; "three", 3 ] ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> ?
    two: 2 -> -2

    Polled Queries: 1 (2); 2 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> -1
    three: 3 -> -3
    two: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.do_actions handle [ `Scope_model 1 ];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  Handle.show handle;
  [%expect
    {|
    one: 1 -> ?
    three: 3 -> ?
    two: 2 -> ?

    No polled queries
    |}];
  Handle.do_actions handle [ `Set_queries [ "two_dup", 2; "two", 2; "three", 3 ] ];
  (* Old state is still there! *)
  Handle.do_actions handle [ `Scope_model 0 ];
  Handle.show handle;
  [%expect
    {|
    three: 3 -> -3
    two: 2 -> -2
    two_dup: 2 -> -2

    Polled Queries: 1 (1); 2 (1); 3 (1)
    |}];
  Handle.show handle;
  ();
  [%expect
    {|
    three: 3 -> -3
    two: 2 -> -2
    two_dup: 2 -> -2

    Polled Queries: 1 (1); 2 (2); 3 (1)
    |}];
  ()
;;
