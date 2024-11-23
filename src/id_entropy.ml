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

open Utils
open Algdt_types
open Algdt_utils
open Entropy_utils

type split_info = var array

let rec var_entropy fspec { samples; tp; histo } =
  let n_samples = Array.length samples in
  if n_samples = 0 then 0.0
  else
    let subs_tps = fspec.(tp) in
    let cnvi cnstr freq =
      let cnv sub_tp =
        {
          samples = Array.make freq dummy_fdsum;
          tp = sub_tp;
          histo = Array.make (Array.length fspec.(sub_tp)) 0;
        }
      in
      Array.map cnv subs_tps.(cnstr)
    in
    let split_vars = Array.mapi cnvi histo in
    let split_ixs = Array.make (Array.length subs_tps) 0 in
    for sample_ix = 0 to n_samples - 1 do
      match samples.(sample_ix) with
      | FDAtom _ -> ()
      | FDStrct (split_cnstr, subs) ->
          let new_sample_ix = split_ixs.(split_cnstr) in
          split_ixs.(split_cnstr) <- new_sample_ix + 1;
          let new_vars = split_vars.(split_cnstr) in
          for i = 0 to Array.length new_vars - 1 do
            let ({ histo = new_histo } as new_var) = new_vars.(i) in
            let sample = subs.(i) in
            new_var.samples.(new_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            new_histo.(cnstr) <- new_histo.(cnstr) + 1
          done
    done;
    let f_samples = float n_samples in
    let coll e new_vars =
      if array_is_empty new_vars then e
      else
        let f_sub_samples = float (Array.length new_vars.(0).samples) in
        e +. (f_sub_samples /. f_samples *. vars_entropy fspec new_vars)
    in
    Array.fold_left coll (calc_entropy histo n_samples) split_vars

and vars_entropy fspec vars =
  Array.fold_left (fun e var -> e +. var_entropy fspec var) 0.0 vars

let calc_split_infos _ dvar cfspec cvars = split_vars dvar cfspec cvars
let calc_split_info_entropy = vars_entropy
