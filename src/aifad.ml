(* AIFAD - Automated Induction of Functions over Algebraic Datatypes

   Copyright © 2002 Austrian Research Institute for Artificial Intelligence
   Copyright © 2003- Markus Mottl <markus.mottl@gmail.com>

   This library is free software; you can redistribute it and/or modify it under
   the terms of the GNU Lesser General Public License as published by the Free
   Software Foundation; either version 2.1 of the License, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
   details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA *)

open Format
open Utils
open Model_utils
open Model_data
open C45_io
open Data_io
open Learn
open Apply
open Eval
open Rand_gen
open Cmd_args

let () =
  match (action, is_algdt, maybe_spec, maybe_model_name) with
  | Learn, true, Some spec, _ -> learn spec
  | Learn, false, Some spec, _ -> learn_c45 spec
  | Apply, _, _, Some model_name -> apply model_name maybe_pred_name
  | Eval eval_name, true, _, Some model_name ->
      let _, cispec_info, _ = load_model_data model_name in
      eval cispec_info eval_name
  | Eval eval_name, true, Some spec, _ ->
      let _, cispec_info =
        do_open_in spec (fun sp_ic -> read_spec (Lexing.from_channel sp_ic))
      in
      eval cispec_info eval_name
  | Eval eval_name, false, _, Some model_name ->
      let _, _, cispec_info, _, _ = load_c45_model_data model_name in
      eval_c45 cispec_info eval_name
  | Eval eval_name, false, Some spec, _ ->
      eval_c45 (calc_cispec_info (read_c45_spec spec)) eval_name
  | Print, _, _, Some model_name -> (
      match open_mdat model_name with
      | `Model (dispec_info, cispec_info, model) ->
          print_model "" "`" dispec_info cispec_info model
      | `C45Model (_, dispec_info, cispec_info, _, model) ->
          print_model "t__" "`V" dispec_info cispec_info model
      | _ -> failwith "aifad: error loading model: unknown format")
  | RandGen n, true, Some spec, _ ->
      let dispec_info, cispec_info =
        do_open_in spec (fun sp_ic -> read_spec (Lexing.from_channel sp_ic))
      in
      set_margin 10000;
      set_max_indent 1000;
      write_rand_samples std_formatter dispec_info cispec_info n with_target;
      print_flush ()
  | RandGen n, false, Some spec, _ ->
      let c45_spec = read_c45_spec spec in
      let dispec_info = calc_dispec_info mv c45_spec in
      let cispec_info = calc_cispec_info c45_spec in
      set_margin 10000;
      set_max_indent 1000;
      write_rand_c45_samples stdout dispec_info cispec_info n with_target mv
        rand_mv_prob;
      print_flush ()
  | _, _, Some _, None ->
      Printf.eprintf "%s: model name required for this action\n" Sys.argv.(0);
      Arg.usage args usage;
      exit 2
  | _, _, None, _ ->
      Printf.eprintf "%s: data specification required for this action\n"
        Sys.argv.(0);
      Arg.usage args usage;
      exit 2
