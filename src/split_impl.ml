(*  File: split_impl.ml

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

(* $Id: split_impl.ml,v 1.122 2006/01/17 00:23:38 mottl Exp $ *)

open Utils
open Algdt_types
open Algdt_utils
open Model_types
open Model_utils
open Factor
open Dshave

module Make (Spec : Split_intf.SPEC) = struct

open Spec

(* Split (co-)domain variables on some domain variable *)
(* (left, X (x, y), right) -> (left, x, y, right) *)

let split dom_ixs ix_cnt dvars cvars dvar_ix =
  let n_dvars_1 = Array.length dvars - 1 in
  let n_cvars_1 = Array.length cvars - 1 in
  let { samples = dsamples; tp = dtp; histo = dhisto } = dvars.(dvar_ix) in
  let subs_tps = dfspec.(dtp) in

  let n_splits = Array.length dhisto in
  let split_dvars = Array.make n_splits [||] in
  let split_cvars = Array.make n_splits [||] in
  let split_dom_ixs = Array.make n_splits [||] in
  let split_ix_cnt = Array.make n_splits ix_cnt in

  for split_cnstr = 0 to n_splits - 1 do
    let freq = dhisto.(split_cnstr) in
    let sub_tps = subs_tps.(split_cnstr) in
    let n_dsubs = Array.length sub_tps in
    let n_new_dvars = n_dvars_1 + n_dsubs in
    let new_dvars = Array.make n_new_dvars dummy_var in
    let new_dom_ixs = Array.make n_new_dvars dummy_sh_el in
    for i = 0 to dvar_ix - 1 do
      let cur_dvar = dvars.(i) in
      new_dvars.(i) <-
        {
          samples = Array.make freq dummy_fdsum;
          tp = cur_dvar.tp;
          histo = Array.make (Array.length cur_dvar.histo) 0;
        }
    done;
    Array.blit dom_ixs 0 new_dom_ixs 0 dvar_ix;
    let n_dsubs_1 = n_dsubs - 1 in
    for i = 0 to n_dsubs_1 do
      let tp = sub_tps.(i) in
      let ofs = dvar_ix + i in
      new_dvars.(ofs) <-
        {
          samples = Array.make freq dummy_fdsum;
          tp = tp;
          histo = Array.make (Array.length dfspec.(tp)) 0;
        };
      new_dom_ixs.(ofs) <- FinVar (ix_cnt + i)
    done;
    let dvar_ix1 = dvar_ix + 1 in
    for i = dvar_ix1 to n_dvars_1 do
      let cur_dvar = dvars.(i) in
      new_dvars.(i + n_dsubs_1) <-
        {
          samples = Array.make freq dummy_fdsum;
          tp = cur_dvar.tp;
          histo = Array.make (Array.length cur_dvar.histo) 0;
        }
    done;
    let n_last = n_dvars_1 - dvar_ix in
    Array.blit dom_ixs dvar_ix1 new_dom_ixs (dvar_ix + n_dsubs) n_last;
    split_dvars.(split_cnstr) <- new_dvars;
    split_dom_ixs.(split_cnstr) <- new_dom_ixs;
    split_ix_cnt.(split_cnstr) <- ix_cnt + n_dsubs;
    let make_new_cvar cvar =
      {
        samples = Array.make freq dummy_fdsum;
        tp = cvar.tp;
        histo = Array.make (Array.length cvar.histo) 0;
      } in
    split_cvars.(split_cnstr) <- Array.map make_new_cvar cvars
  done;

  let split_ixs = Array.make n_splits 0 in

  for sample_ix = 0 to Array.length dsamples - 1 do
    match dsamples.(sample_ix) with
    | FDAtom split_cnstr ->
        let new_sample_ix = split_ixs.(split_cnstr) in
        split_ixs.(split_cnstr) <- new_sample_ix + 1;
        let new_dvars = split_dvars.(split_cnstr) in
        for i = 0 to dvar_ix - 1 do
          let { histo = new_dhisto } as new_dvar = new_dvars.(i) in
          let dsample = dvars.(i).samples.(sample_ix) in
          new_dvar.samples.(new_sample_ix) <- dsample;
          let dcnstr = fdsum_cnstr dsample in
          new_dhisto.(dcnstr) <- new_dhisto.(dcnstr) + 1
        done;
        for i = dvar_ix + 1 to n_dvars_1 do
          let { histo = new_dhisto } as new_dvar = new_dvars.(i - 1) in
          let dsample = dvars.(i).samples.(sample_ix) in
          new_dvar.samples.(new_sample_ix) <- dsample;
          let dcnstr = fdsum_cnstr dsample in
          new_dhisto.(dcnstr) <- new_dhisto.(dcnstr) + 1
        done;
        let new_cvars = split_cvars.(split_cnstr) in
        for i = 0 to n_cvars_1 do
          let { histo = new_chisto } as new_cvar = new_cvars.(i) in
          let csample = cvars.(i).samples.(sample_ix) in
          new_cvar.samples.(new_sample_ix) <- csample;
          let ccnstr = fdsum_cnstr csample in
          new_chisto.(ccnstr) <- new_chisto.(ccnstr) + 1
        done
    | FDStrct (split_cnstr, subs) ->
        let new_sample_ix = split_ixs.(split_cnstr) in
        split_ixs.(split_cnstr) <- new_sample_ix + 1;
        let new_dvars = split_dvars.(split_cnstr) in
        let n_dsubs = Array.length subs in
        for i = 0 to dvar_ix - 1 do
          let { histo = new_dhisto } as new_dvar = new_dvars.(i) in
          let dsample = dvars.(i).samples.(sample_ix) in
          new_dvar.samples.(new_sample_ix) <- dsample;
          let dcnstr = fdsum_cnstr dsample in
          new_dhisto.(dcnstr) <- new_dhisto.(dcnstr) + 1
        done;
        let n_dsubs_1 = n_dsubs - 1 in
        for i = 0 to n_dsubs_1 do
          let { histo = new_dhisto } as new_dvar = new_dvars.(dvar_ix + i) in
          let dsample = subs.(i) in
          new_dvar.samples.(new_sample_ix) <- dsample;
          let dcnstr = fdsum_cnstr dsample in
          new_dhisto.(dcnstr) <- new_dhisto.(dcnstr) + 1
        done;
        for i = dvar_ix + 1 to n_dvars_1 do
          let { histo = new_dhisto } as new_dvar = new_dvars.(i + n_dsubs_1) in
          let dsample = dvars.(i).samples.(sample_ix) in
          new_dvar.samples.(new_sample_ix) <- dsample;
          let dcnstr = fdsum_cnstr dsample in
          new_dhisto.(dcnstr) <- new_dhisto.(dcnstr) + 1
        done;
        let new_cvars = split_cvars.(split_cnstr) in
        for i = 0 to n_cvars_1 do
          let { histo = new_chisto } as new_cvar = new_cvars.(i) in
          let csample = cvars.(i).samples.(sample_ix) in
          new_cvar.samples.(new_sample_ix) <- csample;
          let ccnstr = fdsum_cnstr csample in
          new_chisto.(ccnstr) <- new_chisto.(ccnstr) + 1
        done
  done;
  split_dom_ixs, split_ix_cnt, split_dvars, split_cvars


(* Compute sum-model from optional shave-info *)

let rec calc_sum_mod tp cnstr = function
  | None -> FDAtom cnstr
  | Some (_, ShInfo (pos_infos, vars)) ->
      let sub_tps = cfspec.(tp).(cnstr) in
      let subs = Array.make (Array.length sub_tps) dummy_fdsum in
      let acti sub_ix (var_ix, subcnstr, sub_info) =
        subs.(sub_ix) <- calc_sum_mod vars.(var_ix).tp subcnstr sub_info in
      list_iteri acti pos_infos;
      FDStrct (cnstr, subs)


(* Compute variable model from pos-infos *)

let rec var_mod_of_pos_info_loop cvars var_mods = function
  | sh_ix, cnstr, None -> var_mods.(sh_ix) <- VarFree (FDAtom cnstr)
  | sh_ix, cnstr, (Some (0, _) as sub_sh_info) ->
      let sum_mod = calc_sum_mod cvars.(sh_ix).tp cnstr sub_sh_info in
      var_mods.(sh_ix) <- VarFree sum_mod
  | sh_ix, cnstr, Some (_, ShInfo (sub_infos, sub_vars)) ->
      let sub_var_mods = Array.make (Array.length sub_vars) Var in
      List.iter (var_mod_of_pos_info_loop sub_vars sub_var_mods) sub_infos;
      var_mods.(sh_ix) <- make_strct_var_mods cnstr sub_var_mods

let var_mods_of_pos_infos cvars pos_infos =
  let var_mods = Array.make (Array.length cvars) Var in
  List.iter (var_mod_of_pos_info_loop cvars var_mods) pos_infos;
  var_mods


(* Shave codomain variables with redundant constructors *)

let most_prob_cval cvars = Val (most_prob_csums cvars)
let cnt_some acc = function Some _ -> acc + 1 | _ -> acc

let rec blit_models_loop maybe_models models maybe_models_ix models_ix =
  if models_ix >= 0 then
    let new_maybe_models_ix = maybe_models_ix - 1 in
    match maybe_models.(maybe_models_ix) with
    | Some model ->
        models.(models_ix) <- model;
        blit_models_loop maybe_models models new_maybe_models_ix (models_ix - 1)
    | None -> blit_models_loop maybe_models models new_maybe_models_ix models_ix

let factorized_shave sh_ix_ofs sh_cnstr model def_mod =
  match factorize_models [| model; def_mod |] with
  | FactorNone -> MatchMod (Shave (sh_ix_ofs, sh_cnstr, model, def_mod))
  | FactorVal model -> model
  | FactorLet (models, var_mods) ->
      Let (Shave (sh_ix_ofs, sh_cnstr, models.(0), models.(1)), var_mods)

let rec calc_model dom_ixs ix_cnt dvars cvars =
  let cshave_info, n_sh_cvars = calc_shave_info cfspec cvars in
  match cshave_info with
  | ShInfo ((sh_ix, cnstr, pos_infos) :: rest, cvars) when n_sh_cvars = 0 ->
      let first_fdsum = calc_sum_mod cvars.(sh_ix).tp cnstr pos_infos in
      let fdsums = Array.make (Array.length cvars) first_fdsum in
      let rec loop value_ix = function
        | [] -> Val fdsums
        | (sh_ix, cnstr, sh_info) :: rest ->
            fdsums.(value_ix) <- calc_sum_mod cvars.(sh_ix).tp cnstr sh_info;
            loop (value_ix + 1) rest in
      loop 1 rest
  | ShInfo (pos_infos, cvars) ->
      let dshave_info, n_sh_dvars = calc_shave_info dfspec dvars in
      if n_sh_dvars = 0 then
        let sh_cvars = vars_of_pos_infos n_sh_cvars cvars pos_infos in
        let most_prob_sh_sums = most_prob_csums sh_cvars in
        let var_mods = var_mods_of_pos_infos cvars pos_infos in
        Val (subst_fdsums var_mods most_prob_sh_sums)
      else
        let new_dom_ixs, sh_dvars = dshave dom_ixs dshave_info n_sh_dvars in
        if pos_infos = [] then make_match_mod new_dom_ixs ix_cnt sh_dvars cvars
        else
          let sh_cvars = vars_of_pos_infos n_sh_cvars cvars pos_infos in
          match calc_match_mod new_dom_ixs ix_cnt sh_dvars sh_cvars with
          | Some (Val sums) ->
              Val (subst_fdsums (var_mods_of_pos_infos cvars pos_infos) sums)
          | Some (Let (match_mod, inner_var_mods)) ->
              let outer_var_mods = var_mods_of_pos_infos cvars pos_infos in
              Let (match_mod, subst_var_mods outer_var_mods inner_var_mods)
          | Some (MatchMod match_mod) ->
              Let (match_mod, var_mods_of_pos_infos cvars pos_infos)
          | None -> most_prob_cval cvars

and make_match_mod dom_ixs ix_cnt dvars cvars =
  match calc_match_mod dom_ixs ix_cnt dvars cvars with
  | Some model -> model
  | None -> most_prob_cval cvars

and calc_match_mod dom_ixs ix_cnt dvars cvars =
  match find_split dom_ixs dvars cvars with
  | None as none -> none
  | Some best_ix ->
      match dom_ixs.(best_ix) with
      | FinVar cnv_best_ix ->
          Some (calc_split_mod dom_ixs ix_cnt dvars cvars best_ix cnv_best_ix)
      | ShVar (shaved, last_ix) ->
          let best_ix_1 = best_ix - 1 in
          let best_ix1 = best_ix + 1 in
          let inner_loop ix_cnt sh_ix sh_tp sh_cnstr =
            let left_bnd = shave_lbound ix_cnt sh_ix dom_ixs best_ix_1 in
            let right_bnd = shave_rbound ix_cnt sh_ix dom_ixs best_ix1 in
            let new_ix_cnt = ix_cnt + Array.length dfspec.(sh_tp).(sh_cnstr) in
            let def_mod =
              if split_null_branches then
                calc_def_mod dom_ixs ix_cnt dvars cvars left_bnd right_bnd
              else most_prob_cval cvars in
            new_ix_cnt, def_mod in
          let rec loop ix_cnt ofs = function
            | OneEl (sh_ix, sh_tp, sh_cnstr) ->
                let sh_ix_ofs = sh_ix + ofs in
                let new_ix_cnt, def_mod =
                  inner_loop ix_cnt sh_ix_ofs sh_tp sh_cnstr in
                let split_mod =
                  calc_split_mod
                    dom_ixs new_ix_cnt dvars cvars best_ix (ix_cnt + last_ix) in
                factorized_shave sh_ix_ofs sh_cnstr split_mod def_mod
            | OneCons ((sh_ix, sh_tp, sh_cnstr), rest) ->
                let sh_ix_ofs = sh_ix + ofs in
                let new_ix_cnt, def_mod =
                  inner_loop ix_cnt sh_ix_ofs sh_tp sh_cnstr in
                let model = loop new_ix_cnt ix_cnt rest in
                factorized_shave sh_ix_ofs sh_cnstr model def_mod in
          Some (loop ix_cnt 0 shaved)

and calc_split_mod dom_ixs ix_cnt dvars cvars best_ix cnv_best_ix =
  let is_partial = ref false in
  let split_dom_ixs, split_ix_cnt, split_dvars, split_cvars =
    split dom_ixs ix_cnt dvars cvars best_ix in
  let n_splits = Array.length split_dvars in
  let maybe_models = Array.make n_splits None in
  let n_splits_1 = n_splits - 1 in
  for split_cnstr = 0 to n_splits_1 do
    let new_cvars = split_cvars.(split_cnstr) in
    if array_is_empty new_cvars.(0).samples then is_partial := true
    else
      let new_dvars = split_dvars.(split_cnstr) in
      let new_dom_ixs = split_dom_ixs.(split_cnstr) in
      let new_ix_cnt = split_ix_cnt.(split_cnstr) in
      let model = calc_model new_dom_ixs new_ix_cnt new_dvars new_cvars in
      maybe_models.(split_cnstr) <- Some model
  done;
  if !is_partial then (
    let def_mod =
      if split_null_branches then
        calc_def_mod dom_ixs ix_cnt dvars cvars best_ix (best_ix + 1)
      else most_prob_cval cvars in
    let n_models = Array.fold_left cnt_some 1 maybe_models in
    let models = Array.make n_models def_mod in
    blit_models_loop maybe_models models n_splits_1 (n_models - 2);
    match factorize_models models with
    | FactorNone -> MatchMod (PSplit (cnv_best_ix, maybe_models, def_mod))
    | FactorVal model -> model
    | FactorLet (models, var_mods) ->
        let match_mod =
          let ix_ref = ref 0 in
          let cnv = function
            | None as maybe_model -> maybe_model
            | Some _ ->
                let ix = !ix_ref in
                ix_ref := ix + 1;
                Some models.(ix) in
          let new_maybe_models = Array.map cnv maybe_models in
          PSplit (cnv_best_ix, new_maybe_models, models.(n_splits_1)) in
        Let (match_mod, var_mods))
  else
    let models = unlift_opts maybe_models in
    match factorize_models models with
    | FactorNone -> MatchMod (Split (cnv_best_ix, models))
    | FactorVal model -> model
    | FactorLet (models, var_mods) ->
        Let (Split (cnv_best_ix, models), var_mods)

and calc_def_mod dom_ixs ix_cnt dvars cvars from upto =
  let n_rest = Array.length dvars - upto in
  let n_new_dvars = n_rest + from in
  if n_new_dvars = 0 then most_prob_cval cvars
  else (
    let new_dvars = Array.make n_new_dvars dvars.(0) in
    let new_dom_ixs = Array.make n_new_dvars dom_ixs.(0) in
    let from_1 = max (from - 1) 0 in
    Array.blit dvars 1 new_dvars 1 from_1;
    Array.blit dom_ixs 1 new_dom_ixs 1 from_1;
    Array.blit dvars upto new_dvars from n_rest;
    Array.blit dom_ixs upto new_dom_ixs from n_rest;
    make_match_mod new_dom_ixs ix_cnt new_dvars cvars)


(* Derive a model from domain and codomain variables *)

let derive_model dvars cvars =
  if array_is_empty cvars then failwith "derive_model: no codomain variables";
  if array_is_empty cvars.(0).samples then
    failwith "derive_model: no samples";
  let n_dvars = Array.length dvars in
  calc_model (dom_ixs_iota n_dvars) n_dvars dvars cvars

end
