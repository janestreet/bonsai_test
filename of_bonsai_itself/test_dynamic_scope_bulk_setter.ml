open! Core
open! Import
open! Bonsai_test
open Bonsai.Let_syntax

let id1 = Bonsai.Dynamic_scope.create ~name:"id1" ~fallback:"fallback1" ()
let id2 = Bonsai.Dynamic_scope.create ~name:"id2" ~fallback:"fallback2" ()
let id3 = Bonsai.Dynamic_scope.create ~name:"id3" ~fallback:0 ()

let%expect_test "bulk_setter with single variable" =
  let component graph =
    Bonsai.Dynamic_scope.Bulk_setter.set
      [ id1, Bonsai.return "set1" ]
      ~inside:(fun graph -> Bonsai.Dynamic_scope.lookup id1 graph)
      graph
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| set1 |}]
;;

let%expect_test "bulk_setter with multiple variables" =
  let component graph =
    Bonsai.Dynamic_scope.Bulk_setter.set
      [ id1, Bonsai.return "set1"; id2, Bonsai.return "set2"; id3, Bonsai.return 42 ]
      ~inside:(fun graph ->
        let v1 = Bonsai.Dynamic_scope.lookup id1 graph in
        let v2 = Bonsai.Dynamic_scope.lookup id2 graph in
        let v3 = Bonsai.Dynamic_scope.lookup id3 graph in
        let%arr v1 and v2 and v3 in
        [%string "%{v1}, %{v2}, %{v3#Int}"])
      graph
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| "set1, set2, 42" |}]
;;

let%expect_test "bulk_setter values are dynamic" =
  let var1 = Bonsai.Expert.Var.create "initial1" in
  let var2 = Bonsai.Expert.Var.create 100 in
  let component graph =
    Bonsai.Dynamic_scope.Bulk_setter.set
      [ id1, Bonsai.Expert.Var.value var1; id3, Bonsai.Expert.Var.value var2 ]
      ~inside:(fun graph ->
        let v1 = Bonsai.Dynamic_scope.lookup id1 graph in
        let v2 = Bonsai.Dynamic_scope.lookup id3 graph in
        let%arr v1 and v2 in
        [%string "%{v1}, %{v2#Int}"])
      graph
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| "initial1, 100" |}];
  Bonsai.Expert.Var.set var1 "changed1";
  Bonsai.Expert.Var.set var2 200;
  Handle.show handle;
  [%expect {| "changed1, 200" |}]
;;

let%expect_test "bulk_setter with empty list uses fallback" =
  let component graph =
    Bonsai.Dynamic_scope.Bulk_setter.set
      []
      ~inside:(fun graph -> Bonsai.Dynamic_scope.lookup id1 graph)
      graph
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| fallback1 |}]
;;

let%expect_test "bulk_setter nesting - inner shadows outer" =
  let component graph =
    Bonsai.Dynamic_scope.Bulk_setter.set
      [ id1, Bonsai.return "outer" ]
      ~inside:(fun graph ->
        let outer = Bonsai.Dynamic_scope.lookup id1 graph in
        let inner =
          Bonsai.Dynamic_scope.Bulk_setter.set
            [ id1, Bonsai.return "inner" ]
            ~inside:(fun graph -> Bonsai.Dynamic_scope.lookup id1 graph)
            graph
        in
        let%arr outer and inner in
        [%string "outer: %{outer}, inner: %{inner}"])
      graph
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| "outer: outer, inner: inner" |}]
;;

let%expect_test "bulk_setter with regular set - interoperability" =
  let component graph =
    Bonsai.Dynamic_scope.Bulk_setter.set
      [ id1, Bonsai.return "bulk_set" ]
      ~inside:(fun graph ->
        Bonsai.Dynamic_scope.set
          id2
          (Bonsai.return "regular_set")
          ~inside:(fun graph ->
            let v1 = Bonsai.Dynamic_scope.lookup id1 graph in
            let v2 = Bonsai.Dynamic_scope.lookup id2 graph in
            let%arr v1 and v2 in
            [%string "%{v1}, %{v2}"])
          graph)
      graph
  in
  let handle = Handle.create (Result_spec.sexp (module String)) component in
  Handle.show handle;
  [%expect {| "bulk_set, regular_set" |}]
;;
