open! Core
open! Import
open! Bonsai_test
open Bonsai.Let_syntax

module Expect_test_config = struct
  include Expect_test_config

  let sanitize s = Expect_test_helpers_core.hide_positions_in_string (sanitize s)
end

let%expect_test "debug_node with sexp_of logs old and new values on change" =
  let var = Bonsai.Expert.Var.create "hello" in
  let value = Bonsai.Expert.Var.value var in
  let component graph =
    Bonsai.Debug.debug_node
      ~name:"msg"
      ~sexp_of:[%sexp_of: string]
      ~equal:[%equal: string]
      value
      graph
  in
  let handle = Handle.create (Result_spec.string (module String)) component in
  [%expect
    {| [msg] initialized: hello (lib/bonsai/test/of_bonsai_itself/test_debug_node.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| hello |}];
  Bonsai.Expert.Var.set var "world";
  Handle.recompute_view handle;
  [%expect
    {| [msg] changed: hello -> world (lib/bonsai/test/of_bonsai_itself/test_debug_node.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| world |}];
  ()
;;

module Weird_type = struct
  type t =
    { x : int
    ; y : int
    }
  [@@deriving equal, sexp]
end

let%expect_test "debug_node with custom equality function" =
  (* only check x component *)
  let weird_equal (a : Weird_type.t) (b : Weird_type.t) = a.x = b.x in
  let var = Bonsai.Expert.Var.create { Weird_type.x = 5; y = 10 } in
  let value = Bonsai.Expert.Var.value var in
  let component graph =
    Bonsai.Debug.debug_node
      ~name:"weird_value"
      ~sexp_of:[%sexp_of: Weird_type.t]
      ~equal:weird_equal
      value
      graph
  in
  let handle = Handle.create (Result_spec.sexp (module Weird_type)) component in
  [%expect
    {| [weird_value] initialized: ((x 5) (y 10)) (lib/bonsai/test/of_bonsai_itself/test_debug_node.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| ((x 5) (y 10)) |}];
  Bonsai.Expert.Var.set var { x = 10; y = 10 };
  Handle.recompute_view handle;
  [%expect
    {| [weird_value] changed: ((x 5) (y 10)) -> ((x 10) (y 10)) (lib/bonsai/test/of_bonsai_itself/test_debug_node.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| ((x 10) (y 10)) |}];
  Bonsai.Expert.Var.set var { x = 10; y = 15 };
  Handle.show handle;
  [%expect {| ((x 10) (y 15)) |}]
;;

let%expect_test "debug_node with custom sexp function" =
  let var = Bonsai.Expert.Var.create { Weird_type.x = 5; y = 10 } in
  let weird_sexp_of (a : Weird_type.t) =
    [%message "this is a weird value" (a.x : int) (a.y : int)]
  in
  let value = Bonsai.Expert.Var.value var in
  let component graph =
    Bonsai.Debug.debug_node
      ~name:"weird_value"
      ~sexp_of:weird_sexp_of
      ~equal:[%equal: Weird_type.t]
      value
      graph
  in
  let handle = Handle.create (Result_spec.sexp (module Weird_type)) component in
  [%expect
    {| [weird_value] initialized: ("this is a weird value" (a.x 5) (a.y 10)) (lib/bonsai/test/of_bonsai_itself/test_debug_node.ml:LINE:COL) |}];
  Handle.show handle;
  [%expect {| ((x 5) (y 10)) |}];
  ()
;;

let%expect_test "Let_syntax.debug_node outside of graph context raises" =
  let var = Bonsai.Expert.Var.create 1 in
  let value = Bonsai.Expert.Var.value var in
  Expect_test_helpers_core.require_does_raise (fun () ->
    ignore
      (Let_syntax.debug_node
         ~name:"no_graph_debug_node"
         ~sexp_of:[%sexp_of: int]
         ~equal:[%equal: int]
         value
       : int Bonsai.t));
  [%expect
    {|
    ("Let_syntax.debug_node called outside of the context of a graph"
     (here lib/bonsai/test/of_bonsai_itself/test_debug_node.ml:LINE:COL))
    |}]
;;
