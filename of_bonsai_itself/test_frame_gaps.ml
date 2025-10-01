open! Core
open Bonsai
open Bonsai_test

(* NOTE: This is a small demo demo'ing an awkward situation in which [on_change]
   introduced a "frame gap". This test cases demonstrates/documents this behavior. *)

let component input (local_ graph) =
  let state, set_state = Bonsai.state false graph in
  Bonsai.Edge.on_change ~equal:[%equal: bool] input ~callback:set_state graph;
  state
;;

let%expect_test _ =
  let var = Bonsai.Expert.Var.create false in
  let handle =
    Handle.create
      (Result_spec.sexp (module Bool))
      (fun (local_ graph) -> component (Bonsai.Expert.Var.value var) graph)
  in
  Handle.show handle;
  [%expect {| false |}];
  Bonsai.Expert.Var.set var true;
  Handle.show handle;
  (* NOTE: Did not update! *)
  [%expect {| false |}];
  (* NOTE: Updates in the next frame! *)
  Handle.show handle;
  [%expect {| true |}]
;;
