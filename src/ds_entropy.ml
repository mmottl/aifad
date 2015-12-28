(*
   AIFAD - Automated Induction of Functions over Algebraic Datatypes

   Author: Markus Mottl
   email:  markus.mottl@gmail.com
   WWW:    http://www.ocaml.info

   Copyright (C) 2002  Austrian Research Institute for Artificial Intelligence
   Copyright (C) 2003- Markus Mottl

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Utils
open Algdt_types
open Algdt_utils
open Entropy_utils

type split_info = var array

(* TODO: eliminate redundant constructors for more efficiency *)
let rec destr_vars_entropy ix histos var_samples ofs len =
  if len <= 1 then 0.0
  else
    let histo = histos.(ix) in
    if ix = 0 then
      if histo_is_redundant histo len then 0.0 else calc_entropy histo len
    else if histo_is_redundant histo len then
      destr_vars_entropy (ix - 1) histos var_samples ofs len
    else (
      let init_e = calc_entropy histo len in
      let make_histo ix = Array.make (Array.length histos.(ix)) 0 in
      let next_len = Array.length histos - 1 in
      let make_new_histos _ = Array.init next_len make_histo in
      let n_cnstrs = Array.length histo in
      let many_new_histos = Array.init n_cnstrs make_new_histos in
      let offsets = Array.make n_cnstrs 0 in
      ignore (
        array_fold_lefti (fun i n freq ->
          offsets.(i) <- ofs + n; n + freq) 0 histo);
      let last_cnstr = n_cnstrs - 1 in
      let samples = var_samples.(ix) in
      let f_len = float len in
      let rec loop e cur_cnstr cnt =
        if cnt = 0 then
          let new_e =
            let freq = histo.(cur_cnstr) in
            let split_e =
              let new_ofs = offsets.(cur_cnstr) - freq in
              destr_vars_entropy
                (ix - 1) many_new_histos.(cur_cnstr) var_samples new_ofs freq in
            e +. float freq /. f_len *. split_e in
          if cur_cnstr = last_cnstr then new_e
          else
            let next_cnstr = cur_cnstr + 1 in
            let next_cnt =
              histo.(next_cnstr) - offsets.(next_cnstr) + offsets.(cur_cnstr) in
            loop new_e next_cnstr next_cnt
        else (
          let sample_ix = offsets.(cur_cnstr) in
          let sample = samples.(sample_ix) in
          let cnstr = fdsum_cnstr sample in
          let dst_sample_ix = offsets.(cnstr) in
          offsets.(cnstr) <- dst_sample_ix + 1;
          let new_histos = many_new_histos.(cnstr) in
          if cnstr = cur_cnstr then (
            for var_ix = 0 to ix - 1 do
              let dst_sample = var_samples.(var_ix).(sample_ix) in
              let dst_cnstr = fdsum_cnstr dst_sample in
              let new_histo = new_histos.(var_ix) in
              new_histo.(dst_cnstr) <- new_histo.(dst_cnstr) + 1
            done;
            loop e cnstr (cnt - 1))
          else (
            samples.(sample_ix) <- samples.(dst_sample_ix);
            for var_ix = 0 to ix - 1 do
              let dst_samples = var_samples.(var_ix) in
              let dst_sample = dst_samples.(sample_ix) in
              let other_dst_sample = dst_samples.(dst_sample_ix) in
              dst_samples.(dst_sample_ix) <- dst_sample;
              dst_samples.(sample_ix) <- other_dst_sample;
              let new_histo = new_histos.(var_ix) in
              let dst_cnstr = fdsum_cnstr dst_sample in
              new_histo.(dst_cnstr) <- new_histo.(dst_cnstr) + 1
            done;
            loop e cur_cnstr cnt)) in
      loop init_e 0 histo.(0))

let var_entropy _ var = calc_entropy var.histo (Array.length var.samples)

let copy_samples_of_var var = Array.copy var.samples
let copy_histo_of_var var = Array.copy var.histo

let vars_entropy _ vars =
  let n_vars = Array.length vars in
  if n_vars = 0 then 0.0
  else
    let var_samples = Array.map copy_samples_of_var vars in
    let histos = Array.map copy_histo_of_var vars in
    let n_samples = Array.length vars.(0).samples in
    destr_vars_entropy (n_vars - 1) histos var_samples 0 n_samples

let calc_split_infos _ dvar cfspec cvars = split_vars dvar cfspec cvars
let calc_split_info_entropy = vars_entropy
