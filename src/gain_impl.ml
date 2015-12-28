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
open Gain_intf
open Entropy_utils
open Dshave

module Make (Spec : SPEC) = struct

open Spec

let calc_dom_entropy dvar dom_ix =
  let dentropy = var_entropy dfspec dvar in
  match dom_ix with
  | FinVar _ -> dentropy
  | ShVar (sh_chain, _) ->
      let n_samples = Array.length dvar.samples in
      let coll_dom_entropy dentropy (_, tp, _) =
        dentropy +. calc_unique_entropy (Array.length dfspec.(tp)) n_samples in
      one_list_fold_left coll_dom_entropy dentropy sh_chain

let split_entropy ({ histo = dhisto; samples = dsamples } as dvar) cvars =
  let f_samples = float (Array.length dsamples) in
  let colli dcnstr e split_info =
    let split_e = calc_split_info_entropy cfspec split_info in
    e +. float dhisto.(dcnstr) /. f_samples *. split_e in
  array_fold_lefti colli 0.0 (calc_split_infos dfspec dvar cfspec cvars)

let gains dvars cvars =
  let entropy_before = vars_entropy cfspec cvars in
  let cnv dvar = entropy_before -. split_entropy dvar cvars in
  Array.map cnv dvars

let gain_ratios dom_ixs dvars cvars =
  let entropy_before = vars_entropy cfspec cvars in
  let cnvi i dvar =
    let dentropy = calc_dom_entropy dvar dom_ixs.(i) in
    if dentropy = 0.0 then dentropy
    else (entropy_before -. split_entropy dvar cvars) /. dentropy in
  Array.mapi cnvi dvars


(* Compute minimum gain ratio *)

let calc_min_gr dvars entropy_before =
  let f_samples = float (Array.length dvars.(0).samples) in
  let var_info = log2 (float (Array.length dvars)) in
  var_info /. entropy_before /. f_samples


(* Compute gain ratio *)

let best_gain_ratio with_min_gr dom_ixs dvars cvars =
  let entropy_before = vars_entropy cfspec cvars in
  let min_gr = if with_min_gr then calc_min_gr dvars entropy_before else -1.0 in
  if min_gr > 1.0 then None
  else (
    let maybe_var_ix = ref None in
    let colli i best_gr dvar =
      let dentropy = calc_dom_entropy dvar dom_ixs.(i) in
      if dentropy = 0.0 then best_gr
      else
        let gr = (entropy_before -. split_entropy dvar cvars) /. dentropy in
        if gr >= 0.99999 then (maybe_var_ix := Some i; raise Exit)
        else if gr >= best_gr then (maybe_var_ix := Some i; gr)
        else best_gr in
    (try ignore (array_fold_lefti colli min_gr dvars) with Exit -> ());
    !maybe_var_ix)


(* Quinlan's very special way of computing the gain ratio as used in C4.5 *)

let epsilon = 0.001

let best_gain_ratio_c45 with_min_gr dom_ixs dvars cvars =
  let entropy_before = vars_entropy cfspec cvars in
  let min_gr = if with_min_gr then calc_min_gr dvars entropy_before else -1.0 in
  if min_gr > 1.0 then None
  else (
    let n_dvars = Array.length dvars in
    let gains = Array.make n_dvars 0.0 in
    let colli_gain_sum i gain_sum dvar =
      let gain = entropy_before -. split_entropy dvar cvars in
      gains.(i) <- gain;
      gain_sum +. gain in
    let avg_gain = array_fold_lefti colli_gain_sum 0.0 dvars /. float n_dvars in
    let maybe_var_ix = ref None in
    let colli i best_gr gain =
      if gain < avg_gain -. epsilon then best_gr
      else
        let dentropy = calc_dom_entropy dvars.(i) dom_ixs.(i) in
        if dentropy = 0.0 then best_gr
        else
          let gr = gain /. dentropy in
          if gr >= 0.99999 then (maybe_var_ix := Some i; raise Exit)
          else if gr >= best_gr then (maybe_var_ix := Some i; gr)
          else best_gr in
    (try ignore (array_fold_lefti colli min_gr gains) with Exit -> ());
    !maybe_var_ix)

let choose_gain_ratio gain_c45 =
  if gain_c45 then best_gain_ratio_c45
  else best_gain_ratio


(* Random choice dependent on gain ratio *)

let rec find_rand (r : float) grs n_grs ix =
  if ix >= n_grs then ix - 1
  else
    let gr = grs.(ix) in
    if r <= gr then ix
    else find_rand (r -. gr) grs n_grs (ix + 1)

let rand_gain_ratio with_min_gr dom_ixs dvars cvars =
  let entropy_before = vars_entropy cfspec cvars in
  let min_gr = if with_min_gr then calc_min_gr dvars entropy_before else -1.0 in
  if min_gr >= 1.0 then None
  else
    let grs = gain_ratios dom_ixs dvars cvars in
    if array_forall ((>) min_gr) grs then None
    else
      let r = Random.float (Array.fold_left (+.) 0.0 grs) in
      Some (find_rand r grs (Array.length grs) 0)

end
