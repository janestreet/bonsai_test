open! Core
module Bonsai_proc = Bonsai.Proc

module Effect = struct
  include Ui_effect

  module External = Define (struct
      module Action = String

      let handle str = printf "External event: %s\n" str
    end)

  let sequence l = Many l
  let no_op = Ignore
  let external_ = External.inject
end

module Incr = Ui_incr
include Expect_test_helpers_core

let dummy_source_code_position =
  Source_code_position.
    { pos_fname = "file_name.ml"; pos_lnum = 0; pos_bol = 0; pos_cnum = 0 }
;;

let opaque_const x = Bonsai.Proc.read (Bonsai.Proc.Var.value (Bonsai.Proc.Var.create x))
let opaque_const_value x = Bonsai.Proc.Var.value (Bonsai.Proc.Var.create x)

let opaque_computation c =
  let open Bonsai_proc.Let_syntax in
  if%sub opaque_const_value true then c else assert false
;;
