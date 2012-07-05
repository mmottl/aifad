(*  File: learn.ml

    AIFAD - Automated Induction of Functions over Algebraic Datatypes

    Author: Markus Mottl
    email:  markus.mottl@gmail.com
    WWW:    http://www.ocaml.info

    Copyright (C) 2002  Austrian Research Institute for Artificial Intelligence
    Copyright (C) 2003- Markus Mottl

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Utils
open Algdt_types
open Algdt_utils
open Model_utils
open Complexity
open Model_data
open C45_io
open Data_io
open Typing
open Cmd_args


(* Learn random gain models *)

let watchdog s v =
  let channel = Event.new_channel () in
  let watchdog_thread () =
    Thread.delay s;
    Event.sync (Event.send channel v) in
  ignore (Thread.create watchdog_thread ());
  Event.receive channel

let timed_sync s v event = Event.sync (Event.choose [event; watchdog s v])

let learn_many dfspec dvars cfspec cvars cinit_tps model rand_model =
  let calc_complexity model =
    calc_model_complexity dfspec dvars cfspec cinit_tps model in
  let c = calc_complexity model in
  let res_ref = ref (model, c) in
  let ch = Event.new_channel () in
  let rec loop n model c =
    if n <= 0 then Event.sync (Event.send ch ())
    else
      let new_model = rand_model dvars cvars in
      let new_c = calc_complexity new_model in
      if new_c <= c then (
        res_ref := new_model, new_c;
        loop (n - 1) new_model new_c)
      else loop (n - 1) model c in
  ignore (Thread.create (loop n_rand_gain model) c);
  let the_time = Unix.gettimeofday () -. Cmd_args.start_time in
  let tdiff = t_rand_gain -. the_time -. the_time in
  timed_sync tdiff () (Event.receive ch);
  let new_model, new_c = !res_ref in
  Printf.eprintf "Model complexity: %f\n" new_c;
  flush stderr;
  new_model


(* Learn AIFAD-data *)

let learn spec =
  let
    { ispec = dispec } as dispec_info,
    ({ ispec = cispec } as cispec_info) =
      do_open_in spec (fun sp_ic -> read_spec (Lexing.from_channel sp_ic)) in

  let dfspec, dinit_tps = flatten_ispec dispec in
  let cfspec, cinit_tps = flatten_ispec cispec in

  let dsamples, csamples =
    match maybe_data_name with
    | Some data_name ->
        do_open_in data_name (fun data_ic ->
          let data_lexbuf = Lexing.from_channel data_ic in
          read_samples data_lexbuf dispec_info dfspec cispec_info cfspec)
    | None ->
        let data_lexbuf = Lexing.from_channel stdin in
        read_samples data_lexbuf dispec_info dfspec cispec_info cfspec in

  let dvars = make_vars dfspec dinit_tps dsamples in
  let cvars = make_vars cfspec cinit_tps csamples in

  let module Spec = struct
    let (dfspec, cfspec) as fspecs = dfspec, cfspec

    let find_split, find_rand_split =
      if indep_entropy then
        if shallow_entropy then
          let module GainSpec = struct
            let dfspec, cfspec = fspecs
            include Is_entropy end in
          let module Gain = Gain_impl.Make (GainSpec) in
          Gain.choose_gain_ratio gain_c45 with_min_gr,
          Gain.rand_gain_ratio with_min_gr
        else
          let module GainSpec = struct
            let dfspec, cfspec = fspecs
            include Id_entropy end in
          let module Gain = Gain_impl.Make (GainSpec) in
          Gain.choose_gain_ratio gain_c45 with_min_gr,
          Gain.rand_gain_ratio with_min_gr
      else if shallow_entropy then
        let module GainSpec = struct
          let dfspec, cfspec = fspecs
          include Ds_entropy end in
        let module Gain = Gain_impl.Make (GainSpec) in
        Gain.choose_gain_ratio gain_c45 with_min_gr,
        Gain.rand_gain_ratio with_min_gr
      else
        let module GainSpec = struct
          let dfspec, cfspec = fspecs
          include Dd2_entropy end in
        let module Gain = Gain_impl.Make (GainSpec) in
        Gain.choose_gain_ratio gain_c45 with_min_gr,
        Gain.rand_gain_ratio with_min_gr

    let most_prob_csums =
      if indep_most_prob then Most_prob.indep_most_prob_sums cfspec
      else Most_prob.dep_most_prob_sums cfspec

    let split_null_branches = split_null_branches

    let factorize_models =
      if factorize then Factor.factorize_models
      else fun _ -> Factor.FactorNone
  end in

  let module Split = Split_impl.Make (Spec) in
  let model = Split.derive_model dvars cvars in

  let module RandSpec = struct
    include Spec
    let find_split = find_rand_split
  end in
  let module SplitRand = Split_impl.Make (RandSpec) in

  let model =
    learn_many
      dfspec dvars cfspec cvars cinit_tps model SplitRand.derive_model in

  let mdat = `Model (dispec_info, cispec_info, model) in
  maybe_save_mdat mdat maybe_model_name;
  if print_hmod then print_model "" "`" dispec_info cispec_info model


(* Learn C4.5-data *)

let learn_c45 spec =
  let c45_spec = read_c45_spec spec in

  let dispec_info, dvars, cispec_info, cvars =
    match maybe_data_name with
    | Some data_name -> do_open_in data_name (read_c45_data c45_spec mv)
    | None -> read_c45_data c45_spec mv stdin in

  let dfspec, _ =
    if Array.fold_left coll_n_cnstrs 0 dispec_info.cnstr_tbl = 0 then
      empty_fspec, [||]
    else flatten_ispec dispec_info.ispec in
  let cfspec, cinit_tps = flatten_ispec cispec_info.ispec in

  let module Spec = struct
    let (dfspec, cfspec) as fspecs = dfspec, cfspec

    let find_split, find_rand_split =
      if shallow_entropy then
        let module GainSpec = struct
          let dfspec, cfspec = fspecs
          include Is_entropy end in
        let module Gain = Gain_impl.Make (GainSpec) in
        Gain.choose_gain_ratio gain_c45 with_min_gr,
        Gain.rand_gain_ratio with_min_gr
      else
        let module GainSpec = struct
          let dfspec, cfspec = fspecs
          include Id_entropy end in
        let module Gain = Gain_impl.Make (GainSpec) in
        Gain.choose_gain_ratio gain_c45 with_min_gr,
        Gain.rand_gain_ratio with_min_gr

    let most_prob_csums = Most_prob.indep_most_prob_sums cfspec
    let split_null_branches = split_null_branches

    let factorize_models =
      if factorize then Factor.factorize_models
      else fun _ -> Factor.FactorNone
  end in

  let module Split = Split_impl.Make (Spec) in
  let model = Split.derive_model dvars cvars in

  let module RandSpec = struct
    include Spec
    let find_split = find_rand_split
  end in
  let module SplitRand = Split_impl.Make (RandSpec) in

  let model =
    learn_many
      dfspec dvars cfspec cvars cinit_tps model SplitRand.derive_model in

  let mdat = `C45Model (c45_spec, dispec_info, cispec_info, mv, model) in
  maybe_save_mdat mdat maybe_model_name;
  if print_hmod then print_model "t__" "`V" dispec_info cispec_info model
