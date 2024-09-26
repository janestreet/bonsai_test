open! Core
open! Import
open! Bonsai_test

module Bonsai = struct
  include Bonsai
  module Var = Bonsai.Proc.Var
  module Effect = Bonsai.Effect
end

module Effect = Bonsai.Effect
open Bonsai.Let_syntax

let%test_module "mirror with var" =
  (module struct
    let prepare_test ~store ~interactive =
      let store = Bonsai.Var.create store in
      let interactive = Bonsai.Var.create interactive in
      let store_set =
        (fun value ->
          printf "store set to \"%s\"" value;
          Bonsai.Var.set store value)
        |> Ui_effect.of_sync_fun
      in
      let interactive_set =
        (fun value ->
          printf "interactive set to \"%s\"" value;
          Bonsai.Var.set interactive value)
        |> Ui_effect.of_sync_fun
      in
      let component graph =
        let (_ : unit Bonsai.t) =
          Bonsai_extra.mirror
            ~equal:[%equal: String.t]
            ~store_set:(return store_set)
            ~interactive_set:(return interactive_set)
            ~store_value:(Bonsai.Var.value store)
            ~interactive_value:(Bonsai.Var.value interactive)
            ()
            graph
        in
        let%map store = Bonsai.Var.value store
        and interactive = Bonsai.Var.value interactive in
        sprintf "store: %s, interactive: %s" store interactive
      in
      let handle = Handle.create (Result_spec.string (module String)) component in
      handle, store, interactive
    ;;

    let%expect_test "starts stable" =
      let handle, _store, _interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}]
    ;;

    let%expect_test "starts unstable" =
      let handle, _store, _interactive = prepare_test ~store:"a" ~interactive:"b" in
      Handle.show handle;
      [%expect
        {|
        store: a, interactive: b
        interactive set to "a"
        |}];
      Handle.show handle;
      [%expect {| store: a, interactive: a |}]
    ;;

    let%expect_test "starts stable and then interactive changes" =
      let handle, _store, interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Bonsai.Var.set interactive "b";
      Handle.show handle;
      [%expect
        {|
        store: a, interactive: b
        store set to "b"
        |}];
      Handle.show handle;
      [%expect {| store: b, interactive: b |}]
    ;;

    let%expect_test "starts stable and then store changes" =
      let handle, store, _interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Bonsai.Var.set store "b";
      Handle.show handle;
      [%expect
        {|
        store: b, interactive: a
        interactive set to "b"
        |}];
      Handle.show handle;
      [%expect {| store: b, interactive: b |}]
    ;;

    let%expect_test "starts stable and then both change at the same time" =
      let handle, store, interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Bonsai.Var.set store "b";
      Bonsai.Var.set interactive "c";
      Handle.show handle;
      [%expect
        {|
        store: b, interactive: c
        store set to "c"
        |}];
      Handle.show handle;
      [%expect {| store: c, interactive: c |}]
    ;;

    let%expect_test "starts stable and then interactive changes once a frame" =
      let handle, _store, interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Bonsai.Var.set interactive "b";
      Handle.show handle;
      [%expect
        {|
        store: a, interactive: b
        store set to "b"
        |}];
      Bonsai.Var.set interactive "c";
      Handle.show handle;
      [%expect
        {|
        store: b, interactive: c
        store set to "c"
        |}];
      Bonsai.Var.set interactive "d";
      Handle.show handle;
      [%expect
        {|
        store: c, interactive: d
        store set to "d"
        |}];
      Handle.show handle;
      [%expect {| store: d, interactive: d |}]
    ;;

    let%expect_test "starts stable and then store changes once a frame" =
      let handle, store, _interactive = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Bonsai.Var.set store "b";
      Handle.show handle;
      [%expect
        {|
        store: b, interactive: a
        interactive set to "b"
        |}];
      Bonsai.Var.set store "c";
      Handle.show handle;
      [%expect
        {|
        store: c, interactive: b
        store set to "b"
        |}];
      Bonsai.Var.set store "d";
      Handle.show handle;
      [%expect
        {|
        store: d, interactive: b
        interactive set to "d"
        |}];
      Handle.show handle;
      [%expect {| store: d, interactive: d |}]
    ;;
  end)
;;

let%test_module "mirror with state" =
  (module struct
    let prepare_test ~store ~interactive =
      let print_set name value =
        Effect.of_thunk (fun () -> printf "%s set to \"%s\"\n" name value)
      in
      let component graph =
        let store_value, store_set = Bonsai.state store graph in
        let interactive_value, interactive_set = Bonsai.state interactive graph in
        let store_set =
          let%arr store_set in
          fun value ->
            let%bind.Effect () = print_set "store" value in
            store_set value
        in
        let interactive_set =
          let%arr interactive_set in
          fun value ->
            let%bind.Effect () = print_set "interactive" value in
            interactive_set value
        in
        let (_ : unit Bonsai.t) =
          Bonsai_extra.mirror
            ~equal:[%equal: String.t]
            ~store_set
            ~interactive_set
            ~store_value
            ~interactive_value
            ()
            graph
        in
        let%map store_value and interactive_value and store_set and interactive_set in
        store_value, store_set, interactive_value, interactive_set
      in
      Handle.create
        (module struct
          type t = string * (string -> unit Effect.t) * string * (string -> unit Effect.t)

          type incoming =
            [ `Store_set of string
            | `Interactive_set of string
            ]

          let view (store, _, interactive, _) =
            sprintf "store: %s, interactive: %s" store interactive
          ;;

          let incoming (_, store_set, _, interactive_set) = function
            | `Store_set value -> store_set value
            | `Interactive_set value -> interactive_set value
          ;;
        end)
        component
    ;;

    let%expect_test "starts stable" =
      let handle = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}]
    ;;

    let%expect_test "starts unstable" =
      let handle = prepare_test ~store:"a" ~interactive:"b" in
      Handle.show handle;
      [%expect
        {|
        store: a, interactive: b
        interactive set to "a"
        |}];
      Handle.show handle;
      [%expect {| store: a, interactive: a |}]
    ;;

    let%expect_test "starts stable and then interactive changes" =
      let handle = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Handle.do_actions handle [ `Interactive_set "b" ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "b"
        store: a, interactive: b
        store set to "b"
        |}];
      Handle.show handle;
      [%expect {| store: b, interactive: b |}]
    ;;

    let%expect_test "starts stable and then store changes" =
      let handle = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Handle.do_actions handle [ `Store_set "b" ];
      Handle.show handle;
      [%expect
        {|
        store set to "b"
        store: b, interactive: a
        interactive set to "b"
        |}];
      Handle.show handle;
      [%expect {| store: b, interactive: b |}]
    ;;

    let%expect_test "starts stable and then both change at the same time" =
      let handle = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Handle.do_actions handle [ `Store_set "b"; `Interactive_set "c" ];
      Handle.show handle;
      [%expect
        {|
        store set to "b"
        interactive set to "c"
        store: b, interactive: c
        store set to "c"
        |}];
      Handle.show handle;
      [%expect {| store: c, interactive: c |}]
    ;;

    let%expect_test "starts stable and then both change at the same time" =
      let handle = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Handle.do_actions handle [ `Store_set "b"; `Interactive_set "c" ];
      Handle.show handle;
      [%expect
        {|
        store set to "b"
        interactive set to "c"
        store: b, interactive: c
        store set to "c"
        |}];
      Handle.show handle;
      [%expect {| store: c, interactive: c |}]
    ;;

    let%expect_test "starts stable and then interactive changes once a frame" =
      let handle = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Handle.do_actions handle [ `Interactive_set "b" ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "b"
        store: a, interactive: b
        store set to "b"
        |}];
      Handle.do_actions handle [ `Interactive_set "c" ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "c"
        store: b, interactive: c
        store set to "c"
        |}];
      Handle.do_actions handle [ `Interactive_set "d" ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "d"
        store: c, interactive: d
        store set to "d"
        |}];
      Handle.show handle;
      [%expect {| store: d, interactive: d |}]
    ;;

    let%expect_test "starts stable and then store changes once a frame" =
      let handle = prepare_test ~store:"a" ~interactive:"a" in
      Handle.show handle;
      [%expect {| store: a, interactive: a |}];
      Handle.do_actions handle [ `Store_set "b" ];
      Handle.show handle;
      [%expect
        {|
        store set to "b"
        store: b, interactive: a
        interactive set to "b"
        |}];
      Handle.do_actions handle [ `Store_set "c" ];
      Handle.show handle;
      [%expect
        {|
        store set to "c"
        store: c, interactive: b
        store set to "b"
        |}];
      Handle.do_actions handle [ `Store_set "d" ];
      Handle.show handle;
      [%expect
        {|
        store set to "d"
        store: d, interactive: b
        interactive set to "d"
        |}];
      Handle.show handle;
      [%expect {| store: d, interactive: d |}]
    ;;
  end)
;;

let%test_module "mirror' with var" =
  (module struct
    let prepare_test ~store ~interactive =
      let store = Bonsai.Var.create store in
      let interactive = Bonsai.Var.create interactive in
      let store_set =
        (fun value ->
          printf "store set to \"%s\"" value;
          Bonsai.Var.set store (Some value))
        |> Ui_effect.of_sync_fun
      in
      let interactive_set =
        (fun value ->
          printf "interactive set to \"%s\"" value;
          Bonsai.Var.set interactive (Some value))
        |> Ui_effect.of_sync_fun
      in
      let component graph =
        let (_ : unit Bonsai.t) =
          Bonsai_extra.mirror'
            ()
            ~equal:[%equal: String.t]
            ~store_set:(return store_set)
            ~interactive_set:(return interactive_set)
            ~store_value:(Bonsai.Var.value store)
            ~interactive_value:(Bonsai.Var.value interactive)
            graph
        in
        let%map store = Bonsai.Var.value store
        and interactive = Bonsai.Var.value interactive in
        sprintf
          "store: %s, interactive: %s"
          (Option.value store ~default:"<none>")
          (Option.value interactive ~default:"<none>")
      in
      let handle = Handle.create (Result_spec.string (module String)) component in
      handle, store, interactive
    ;;

    let%expect_test "starts both none" =
      let handle, _store, _interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}]
    ;;

    let%expect_test "starts interactive some" =
      let handle, _store, _interactive =
        prepare_test ~store:None ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect
        {|
        store: <none>, interactive: hi
        store set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts store some" =
      let handle, _store, _interactive =
        prepare_test ~store:(Some "hi") ~interactive:None
      in
      Handle.show handle;
      [%expect
        {|
        store: hi, interactive: <none>
        interactive set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some (same value)" =
      let handle, _store, _interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some (different values)" =
      let handle, _store, _interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hello")
      in
      Handle.show handle;
      [%expect
        {|
        store: hi, interactive: hello
        interactive set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, store set " =
      let handle, store, _interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Bonsai.Var.set store (Some "hi");
      Handle.show handle;
      [%expect
        {|
        store: hi, interactive: <none>
        interactive set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, interactive set " =
      let handle, _store, interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Bonsai.Var.set interactive (Some "hi");
      Handle.show handle;
      [%expect
        {|
        store: <none>, interactive: hi
        store set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, both set to same value" =
      let handle, store, interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Bonsai.Var.set interactive (Some "hi");
      Bonsai.Var.set store (Some "hi");
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, both set to different values" =
      let handle, store, interactive = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Bonsai.Var.set interactive (Some "hi");
      Bonsai.Var.set store (Some "hello");
      Handle.show handle;
      [%expect
        {|
        store: hello, interactive: hi
        store set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some, both set to different values" =
      let handle, store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set interactive (Some "abc");
      Bonsai.Var.set store (Some "def");
      Handle.show handle;
      [%expect
        {|
        store: def, interactive: abc
        store set to "abc"
        |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;

    let%expect_test "starts both some (same value), store reset to none" =
      let handle, store, _interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set store None;
      Handle.show handle;
      (* The noneness isn't propagated to interactive *)
      [%expect {| store: <none>, interactive: hi |}]
    ;;

    let%expect_test "starts both some (same value), interactive reset to none" =
      let handle, _store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set interactive None;
      Handle.show handle;
      (* The noneness isn't propagated to the store *)
      [%expect {| store: hi, interactive: <none> |}]
    ;;

    let%expect_test "starts both some (same value), both set to none" =
      let handle, store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set store None;
      Bonsai.Var.set interactive None;
      Handle.show handle;
      (* The noneness isn't propagated to the store *)
      [%expect {| store: <none>, interactive: <none> |}]
    ;;

    let%expect_test "starts both some (same value), interactive set to none, both swap" =
      let handle, store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set store None;
      Handle.show handle;
      [%expect {| store: <none>, interactive: hi |}];
      Bonsai.Var.set store (Some "abc");
      Bonsai.Var.set interactive None;
      Handle.show handle;
      [%expect
        {|
        store: abc, interactive: <none>
        interactive set to "abc"
        |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;

    let%expect_test "starts both some (same value), store set to none, both swap" =
      let handle, store, interactive =
        prepare_test ~store:(Some "hi") ~interactive:(Some "hi")
      in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Bonsai.Var.set interactive None;
      Handle.show handle;
      [%expect {| store: hi, interactive: <none> |}];
      Bonsai.Var.set interactive (Some "abc");
      Bonsai.Var.set store None;
      Handle.show handle;
      [%expect
        {|
        store: <none>, interactive: abc
        store set to "abc"
        |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;
  end)
;;

let%test_module "mirror' with state" =
  (module struct
    let prepare_test ~store ~interactive =
      let print_set name value =
        Effect.of_thunk (fun () -> printf "%s set to \"%s\"\n" name value)
      in
      let component graph =
        let store_value, store_set = Bonsai.state_opt ?default_model:store graph in
        let interactive_value, interactive_set =
          Bonsai.state_opt ?default_model:interactive graph
        in
        let store_set_some =
          let%arr store_set in
          fun value ->
            let%bind.Effect () = print_set "store" value in
            store_set (Some value)
        in
        let store_set_opt =
          let%arr store_set in
          fun store ->
            let%bind.Effect () =
              print_set "store" (Option.value store ~default:"<none>")
            in
            store_set store
        in
        let interactive_set_some =
          let%arr interactive_set in
          fun value ->
            let%bind.Effect () = print_set "interactive" value in
            interactive_set (Some value)
        in
        let interactive_set_opt =
          let%arr interactive_set in
          fun interactive ->
            let%bind.Effect () =
              print_set "interactive" (Option.value interactive ~default:"<none>")
            in
            interactive_set interactive
        in
        let (_ : unit Bonsai.t) =
          Bonsai_extra.mirror'
            ~equal:[%equal: String.t]
            ~store_set:store_set_some
            ~interactive_set:interactive_set_some
            ~store_value
            ~interactive_value
            ()
            graph
        in
        let%map store_value
        and interactive_value
        and store_set_opt
        and interactive_set_opt in
        store_value, store_set_opt, interactive_value, interactive_set_opt
      in
      Handle.create
        (module struct
          type t =
            string option
            * (string option -> unit Effect.t)
            * string option
            * (string option -> unit Effect.t)

          type incoming =
            [ `Store_set of string option
            | `Interactive_set of string option
            ]

          let view (store, _, interactive, _) =
            sprintf
              "store: %s, interactive: %s"
              (Option.value store ~default:"<none>")
              (Option.value interactive ~default:"<none>")
          ;;

          let incoming (_, store_set, _, interactive_set) = function
            | `Store_set value -> store_set value
            | `Interactive_set value -> interactive_set value
          ;;
        end)
        component
    ;;

    let%expect_test "starts both none" =
      let handle = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}]
    ;;

    let%expect_test "starts interactive some" =
      let handle = prepare_test ~store:None ~interactive:(Some "hi") in
      Handle.show handle;
      [%expect
        {|
        store: <none>, interactive: hi
        store set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts store some" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:None in
      Handle.show handle;
      [%expect
        {|
        store: hi, interactive: <none>
        interactive set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some (same value)" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some (different values)" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hello") in
      Handle.show handle;
      [%expect
        {|
        store: hi, interactive: hello
        interactive set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, store set " =
      let handle = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Handle.do_actions handle [ `Store_set (Some "hi") ];
      Handle.show handle;
      [%expect
        {|
        store set to "hi"
        store: hi, interactive: <none>
        interactive set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, interactive set " =
      let handle = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Handle.do_actions handle [ `Interactive_set (Some "hi") ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "hi"
        store: <none>, interactive: hi
        store set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both none, both set to same value" =
      let handle = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Handle.do_actions handle [ `Interactive_set (Some "hi") ];
      Handle.do_actions handle [ `Store_set (Some "hi") ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "hi"
        store set to "hi"
        store: hi, interactive: hi
        |}]
    ;;

    let%expect_test "starts both none, both set to different values" =
      let handle = prepare_test ~store:None ~interactive:None in
      Handle.show handle;
      [%expect {| store: <none>, interactive: <none> |}];
      Handle.do_actions handle [ `Interactive_set (Some "hi") ];
      Handle.do_actions handle [ `Store_set (Some "hello") ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "hi"
        store set to "hello"
        store: hello, interactive: hi
        store set to "hi"
        |}];
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}]
    ;;

    let%expect_test "starts both some, both set to different values" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Handle.do_actions handle [ `Interactive_set (Some "abc") ];
      Handle.do_actions handle [ `Store_set (Some "def") ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "abc"
        store set to "def"
        store: def, interactive: abc
        store set to "abc"
        |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;

    let%expect_test "starts both some (same value), store reset to none" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Handle.do_actions handle [ `Store_set None ];
      Handle.show handle;
      (* The noneness isn't propagated to interactive *)
      [%expect
        {|
        store set to "<none>"
        store: <none>, interactive: hi
        |}]
    ;;

    let%expect_test "starts both some (same value), interactive reset to none" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Handle.do_actions handle [ `Interactive_set None ];
      Handle.show handle;
      (* The noneness isn't propagated to the store *)
      [%expect
        {|
        interactive set to "<none>"
        store: hi, interactive: <none>
        |}]
    ;;

    let%expect_test "starts both some (same value), both set to none" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Handle.do_actions handle [ `Store_set None ];
      Handle.do_actions handle [ `Interactive_set None ];
      Handle.show handle;
      (* The noneness isn't propagated to the store *)
      [%expect
        {|
        store set to "<none>"
        interactive set to "<none>"
        store: <none>, interactive: <none>
        |}]
    ;;

    let%expect_test "starts both some (same value), interactive set to none, both swap" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Handle.do_actions handle [ `Store_set None ];
      Handle.show handle;
      [%expect
        {|
        store set to "<none>"
        store: <none>, interactive: hi
        |}];
      Handle.do_actions handle [ `Store_set (Some "abc") ];
      Handle.do_actions handle [ `Interactive_set None ];
      Handle.show handle;
      [%expect
        {|
        store set to "abc"
        interactive set to "<none>"
        store: abc, interactive: <none>
        interactive set to "abc"
        |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;

    let%expect_test "starts both some (same value), store set to none, both swap" =
      let handle = prepare_test ~store:(Some "hi") ~interactive:(Some "hi") in
      Handle.show handle;
      [%expect {| store: hi, interactive: hi |}];
      Handle.do_actions handle [ `Interactive_set None ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "<none>"
        store: hi, interactive: <none>
        |}];
      Handle.do_actions handle [ `Interactive_set (Some "abc") ];
      Handle.do_actions handle [ `Store_set None ];
      Handle.show handle;
      [%expect
        {|
        interactive set to "abc"
        store set to "<none>"
        store: <none>, interactive: abc
        store set to "abc"
        |}];
      Handle.show handle;
      [%expect {| store: abc, interactive: abc |}]
    ;;
  end)
;;
