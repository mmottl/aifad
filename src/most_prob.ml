(*  File: most_prob.ml

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

(* $Id: most_prob.ml,v 1.4 2006/01/17 00:23:38 mottl Exp $ *)

open Utils
open Algdt_types
open Algdt_utils
open Entropy_utils

(* Calculation of most frequent leaf models *)

(* Utility functions *)

let is_atomic_var fspec var = array_forall array_is_empty fspec.(var.tp)

let is_one_atomic_var fspec vars =
  Array.length vars = 1 && is_atomic_var fspec vars.(0)

let most_prob_cnstr var =
  let colli cnstr (_, max_freq as acc) freq =
    if max_freq < freq then cnstr, freq
    else acc in
  fst (array_fold_left1i colli (0, var.histo.(0)) var.histo)

let make_many_sub_vars fspec fspec_tp histo =
  let make_sub_var cnstr freq =
    let fspec_tp_cnstr = fspec_tp.(cnstr) in
    let n_sub_vars = Array.length fspec_tp_cnstr in
    if n_sub_vars = 0 then [||]
    else
      Array.init n_sub_vars (fun sub_var_ix ->
        let sub_tp = fspec_tp_cnstr.(sub_var_ix) in
        {
          samples = Array.make freq dummy_fdsum;
          tp = sub_tp;
          histo = Array.make (Array.length fspec.(sub_tp)) 0;
        }) in
  Array.mapi make_sub_var histo

let calc_scaled_probs f_total_samples histo n_samples =
  let ps = calc_probs histo n_samples in
  let f_samples = float n_samples in
  let rescale i p = ps.(i) <- p *. f_samples /. f_total_samples in
  Array.iteri rescale ps;
  ps

let make_sub_ixs histo = Array.make (Array.length histo) 0


(* Independent version: assumes independence of products! *)

let calc_many_sub_vars fspec { samples = samples; tp = tp; histo = histo } =
  let fspec_tp = fspec.(tp) in
  let many_sub_vars = make_many_sub_vars fspec fspec_tp histo in
  let sub_ixs = make_sub_ixs histo in
  for sample_ix = 0 to Array.length samples - 1 do
    match samples.(sample_ix) with
    | FDStrct (cnstr, subs) ->
        let sub_ix = sub_ixs.(cnstr) in
        sub_ixs.(cnstr) <- sub_ix + 1;
        let sub_vars = many_sub_vars.(cnstr) in
        for sub_var_ix = 0 to Array.length subs - 1 do
          let { samples = sub_samples; histo = sub_histo; _ } =
            sub_vars.(sub_var_ix) in
          let sub_sample = subs.(sub_var_ix) in
          sub_samples.(sub_ix) <- sub_sample;
          let sub_cnstr = fdsum_cnstr sub_sample in
          sub_histo.(sub_cnstr) <- sub_histo.(sub_cnstr) + 1
        done
    | FDAtom _ -> ()
  done;
  many_sub_vars

let rec indep_most_prob_sum fspec f_total_samples var =
  let n_samples = Array.length var.samples in
  let histo = var.histo in
  let scale = float n_samples /. f_total_samples in
  if is_atomic_var fspec var then
    let best_cnstr = most_prob_cnstr var in
    FDAtom best_cnstr, calc_prob histo best_cnstr n_samples *. scale
  else
    let fspec_tp = fspec.(var.tp) in
    let many_sub_vars = calc_many_sub_vars fspec var in
    let colli cnstr (_, best_sh as acc) sub_vars =
      if histo.(cnstr) = 0 then acc
      else
        let sub_res = Array.make (Array.length sub_vars) dummy_fdsum in
        let colli sub_var_ix sh sub_var =
          let fdsum, sub_sh =
            indep_most_prob_sum fspec f_total_samples sub_var in
          sub_res.(sub_var_ix) <- fdsum;
          sh +. sub_sh in
        let sub_sh = array_fold_lefti colli 0.0 sub_vars in
        let sh = sub_sh +. calc_prob histo cnstr n_samples *. scale in
        if sh > best_sh then
          if array_is_empty fspec_tp.(cnstr) then FDAtom cnstr, sh
          else FDStrct (cnstr, sub_res), sh
        else acc in
    array_fold_lefti colli (dummy_fdsum, 0.0) many_sub_vars

let indep_most_prob_sums fspec vars =
  let f_total_samples = float (Array.length vars.(0).samples) in
  let cnv var = fst (indep_most_prob_sum fspec f_total_samples var) in
  Array.map cnv vars


(* Dependent version *)

let make_zeros_ref _ = ref 0.0, ref 0.0

let rec dep_most_prob_sum fspec p_all irefs
    { samples = samples; tp = tp; histo = histo } =
  let fspec_tp = fspec.(tp) in
  let n_samples = Array.length samples in
  let ps = calc_probs histo n_samples in
  let is = calc_probs_icont ps in
  let make_sub_irefs cnstr freq =
    if array_is_empty fspec_tp.(cnstr) then [||]
    else Array.init freq make_zeros_ref in
  let many_sub_irefs = Array.mapi make_sub_irefs histo in
  let many_sub_vars = make_many_sub_vars fspec fspec_tp histo in
  let sub_ixs = make_sub_ixs histo in
  for sample_ix = 0 to n_samples - 1 do
    match samples.(sample_ix) with
    | FDAtom cnstr ->
        let sh_ref, all_ref = irefs.(sample_ix) in
        let icont_p_all = is.(cnstr) *. p_all in
        sh_ref := !sh_ref +. ps.(cnstr) *. icont_p_all;
        all_ref := !all_ref +. icont_p_all
    | FDStrct (cnstr, subs) ->
        let (sh_ref, all_ref as iref) = irefs.(sample_ix) in
        let icont_p_all = is.(cnstr) *. p_all in
        sh_ref := !sh_ref +. ps.(cnstr) *. icont_p_all;
        all_ref := !all_ref +. icont_p_all;
        let sub_ix = sub_ixs.(cnstr) in
        sub_ixs.(cnstr) <- sub_ix + 1;
        many_sub_irefs.(cnstr).(sub_ix) <- iref;
        let sub_vars = many_sub_vars.(cnstr) in
        for sub_var_ix = 0 to Array.length subs - 1 do
          let { samples = sub_samples; histo = sub_histo; _ } =
            sub_vars.(sub_var_ix) in
          let sub_sample = subs.(sub_var_ix) in
          sub_samples.(sub_ix) <- sub_sample;
          let sub_cnstr = fdsum_cnstr sub_sample in
          sub_histo.(sub_cnstr) <- sub_histo.(sub_cnstr) + 1
        done
  done;
  let acti cnstr sub_vars =
    if histo.(cnstr) > 0 then
      let act =
        dep_most_prob_sum fspec (p_all *. ps.(cnstr)) many_sub_irefs.(cnstr) in
      Array.iter act sub_vars in
  Array.iteri acti many_sub_vars

let dep_most_prob_sums fspec vars =
  if is_one_atomic_var fspec vars then [| FDAtom (most_prob_cnstr vars.(0)) |]
  else
    let n_total_samples = Array.length vars.(0).samples in
    let irefs = Array.init n_total_samples make_zeros_ref in
    let act = dep_most_prob_sum fspec 1.0 irefs in
    let _ = Array.iter act vars in
    let colli ix (_, best_ratio as acc) (sh_ref, all_ref) =
      let ratio = !sh_ref /. !all_ref in
      if ratio > best_ratio then ix, ratio
      else acc in
    let best_ix, _ = array_fold_lefti colli (0, 0.0) irefs in
    Array.map (fun var -> var.samples.(best_ix)) vars
