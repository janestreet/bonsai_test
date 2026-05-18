open! Core
open! Import
open! Bonsai_test
open Bonsai.Let_syntax

module Expect_test_config = struct
  include Expect_test_config

  let sanitize s = Expect_test_helpers_core.hide_positions_in_string (sanitize s)
end

let%expect_test "single variable" =
  let var = Bonsai.Expert.Var.create 5 in
  let value = Bonsai.Expert.Var.value var in
  let component _graph =
    let%arr_debug a = value in
    a
  in
  let handle = Handle.create (Result_spec.string (module Int)) component in
  Handle.recompute_view handle;
  [%expect
    {| [a] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 5 |}];
  Bonsai.Expert.Var.set var 10;
  Handle.recompute_view handle;
  [%expect
    {| [a] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 10 |}]
;;

let%expect_test "let-punned binding" =
  let var = Bonsai.Expert.Var.create 5 in
  let value = Bonsai.Expert.Var.value var in
  let component _graph =
    let%arr_debug value in
    value
  in
  let handle = Handle.create (Result_spec.string (module Int)) component in
  Handle.recompute_view handle;
  [%expect
    {| [value] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 5 |}];
  Bonsai.Expert.Var.set var 10;
  Handle.recompute_view handle;
  [%expect
    {| [value] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 10 |}]
;;

let%expect_test "multiple variables with and" =
  let var1 = Bonsai.Expert.Var.create 1 in
  let var2 = Bonsai.Expert.Var.create "hello" in
  let value1 = Bonsai.Expert.Var.value var1 in
  let value2 = Bonsai.Expert.Var.value var2 in
  let component _graph =
    let%arr_debug a = value1
    and b = value2 in
    [%string "%{a#Int}: %{b}"]
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.recompute_view handle;
  [%expect
    {|
    [b] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL)
    [a] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL)
    |}];
  Handle.show handle;
  [%expect {| 1: hello |}];
  Bonsai.Expert.Var.set var1 2;
  Handle.recompute_view handle;
  [%expect
    {| [a] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 2: hello |}];
  Bonsai.Expert.Var.set var2 "world";
  Handle.recompute_view handle;
  [%expect
    {| [b] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 2: world |}]
;;

let%expect_test "type constraint in arr_debug" =
  let var = Bonsai.Expert.Var.create 5 in
  let value = Bonsai.Expert.Var.value var in
  let component _graph =
    let%arr_debug (a : int) = value in
    [%string "%{a#Int}"]
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  Handle.recompute_view handle;
  [%expect
    {| [a] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 5 |}];
  Bonsai.Expert.Var.set var 10;
  Handle.recompute_view handle;
  [%expect
    {| [a] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 10 |}];
  ()
;;

module My_record = struct
  type t =
    { x : int
    ; y : string
    ; z : bool
    }
  [@@deriving sexp]
end

let%expect_test "record destructuring with ignored field" =
  let var = Bonsai.Expert.Var.create { My_record.x = 1; y = "a"; z = true } in
  let value = Bonsai.Expert.Var.value var in
  let component _graph =
    let%arr_debug { x; _ } = value in
    x
  in
  let handle = Handle.create (Result_spec.string (module Int)) component in
  Handle.recompute_view handle;
  [%expect
    {| [x] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 1 |}];
  Bonsai.Expert.Var.set var { x = 1; y = "b"; z = false };
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.show handle;
  [%expect {| 1 |}];
  Bonsai.Expert.Var.set var { x = 2; y = "b"; z = true };
  Handle.recompute_view handle;
  [%expect
    {| [x] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 2 |}]
;;

let%expect_test "record destructuring with field type annotation" =
  let var = Bonsai.Expert.Var.create { My_record.x = 1; y = "a"; z = true } in
  let value = Bonsai.Expert.Var.value var in
  let component _graph =
    let%arr_debug { x : int; _ } = value in
    x
  in
  let handle = Handle.create (Result_spec.string (module Int)) component in
  Handle.recompute_view handle;
  [%expect
    {| [x] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 1 |}];
  Bonsai.Expert.Var.set var { x = 1; y = "b"; z = false };
  Handle.recompute_view handle;
  [%expect {| |}];
  Handle.show handle;
  [%expect {| 1 |}];
  Bonsai.Expert.Var.set var { x = 2; y = "b"; z = true };
  Handle.recompute_view handle;
  [%expect
    {| [x] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 2 |}]
;;

let%expect_test "record destructuring with multiple & non-punned fields" =
  let var = Bonsai.Expert.Var.create { My_record.x = 1; y = "a"; z = true } in
  let value = Bonsai.Expert.Var.value var in
  let component _graph =
    let%arr_debug { x = foo; y = _; z } = value in
    ignore z;
    foo
  in
  let handle = Handle.create (Result_spec.string (module Int)) component in
  Handle.recompute_view handle;
  [%expect
    {|
    [z] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL)
    [foo] initialized: <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL)
    |}];
  Handle.show handle;
  [%expect {| 1 |}];
  Bonsai.Expert.Var.set var { x = 1; y = "b"; z = false };
  Handle.recompute_view handle;
  [%expect
    {| [z] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| 1 |}];
  Bonsai.Expert.Var.set var { x = 2; y = "b"; z = true };
  Handle.recompute_view handle;
  [%expect
    {|
    [z] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL)
    [foo] changed: <opaque> -> <opaque> (lib/bonsai/test/of_bonsai_itself/test_arr_debug.ml:LINE:COL)
    |}];
  Handle.show handle;
  [%expect {| 2 |}]
;;
