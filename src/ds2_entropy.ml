(*  File: ds2_entropy.ml

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
open Entropy_utils

type split_info = var array

let rec vars_entropy_loop pre_histos pre_var_samples =
  let ixs_ref = ref [] in
  let n_vars_ref = ref 0 in
  let n_samples = Array.length pre_var_samples.(0) in
  for i = 0 to Array.length pre_histos - 1 do
    let histo = pre_histos.(i) in
    if not (histo_is_redundant histo n_samples) then (
      incr n_vars_ref;
      ixs_ref := i :: !ixs_ref)
  done;
  let n_vars = !n_vars_ref in
  if n_vars = 0 then 0.0
  else (
    let histos = Array.make n_vars [||] in
    let var_samples = Array.make n_vars [||] in
    let acti dst_ix src_ix =
      histos.(dst_ix) <- pre_histos.(src_ix);
      var_samples.(dst_ix) <- pre_var_samples.(src_ix) in
    list_iteri acti !ixs_ref;
    let n_vars_1 = n_vars - 1 in
    let histo = histos.(n_vars_1) in
    let samples = var_samples.(n_vars_1) in
    let e = calc_entropy histo n_samples in
    if n_vars_1 = 0 then e
    else (
      let make_histo ix = Array.make (Array.length histos.(ix)) 0 in
      let cnv_cnstr freq =
        Array.init n_vars_1 make_histo,
        Array.make_matrix n_vars_1 freq dummy_fdsum in
      let many_histos, many_var_samples = array_map2 cnv_cnstr histo in
      let many_sample_ixs = Array.make (Array.length histo) 0 in
      let n_vars_2 = n_vars_1 - 1 in
      let acti sample_ix sample =
        let cnstr = fdsum_cnstr sample in
        let dst_histos = many_histos.(cnstr) in
        let dst_var_samples = many_var_samples.(cnstr) in
        let dst_sample_ix = many_sample_ixs.(cnstr) in
        many_sample_ixs.(cnstr) <- dst_sample_ix + 1;
        for var_ix = 0 to n_vars_2 do
          let dst_sample = var_samples.(var_ix).(sample_ix) in
          dst_var_samples.(var_ix).(dst_sample_ix) <- dst_sample;
          let dst_cnstr = fdsum_cnstr dst_sample in
          let dst_histo = dst_histos.(var_ix) in
          dst_histo.(dst_cnstr) <- dst_histo.(dst_cnstr) + 1
        done in
      Array.iteri acti samples;
      let f_samples = float n_samples in
      let colli cnstr e new_histos =
        let new_var_samples = many_var_samples.(cnstr) in
        let f_sub_samples = float (Array.length new_var_samples.(0)) in
        let rest_e = vars_entropy_loop new_histos new_var_samples in
        e +. f_sub_samples /. f_samples *. rest_e in
      array_fold_lefti colli e many_histos))

let histo_samples_of_var var = var.histo, var.samples

let vars_entropy _ vars =
  let histos, var_samples = array_map2 histo_samples_of_var vars in
  vars_entropy_loop histos var_samples

let var_entropy _ var = calc_entropy var.histo (Array.length var.samples)

let calc_split_infos _ dvar cfspec cvars = split_vars dvar cfspec cvars
let calc_split_info_entropy = vars_entropy
