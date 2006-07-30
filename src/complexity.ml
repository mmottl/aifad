(*  File: complexity.ml

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

(* $Id: complexity.ml,v 1.12 2006/01/17 00:23:37 mottl Exp $ *)

open Utils
open Algdt_types
open Algdt_utils
open Model_types
open Model_utils
open Entropy_utils
open Info_utils

(* Compute model complexity *)

let calc_mod_complexity cfspec cfspec_bits cont_match_mod tps bds = function
  | Val fdsums -> bds /. calc_fdsums_bits cfspec_bits cfspec tps fdsums
  | Let (match_mod, var_mods) ->
      let new_tps = Array.make (count_vars_var_mods var_mods) init_tp in
      let tps_ix_ref = ref 0 in
      let rec acti tpl_tps tpl_ix = function
        | VarStrct (cnstr, var_mods) ->
            Array.iteri (acti cfspec.(tpl_tps.(tpl_ix)).(cnstr)) var_mods
        | VarFree _ -> ()
        | Var ->
            let tps_ix = !tps_ix_ref in
            tps_ix_ref := tps_ix + 1;
            new_tps.(tps_ix) <- tpl_tps.(tpl_ix) in
      Array.iteri (acti tps) var_mods;
      let sub_c = cont_match_mod new_tps bds match_mod in
      sub_c +. bds /. calc_var_mods_bits cfspec_bits cfspec tps var_mods
  | MatchMod match_mod -> cont_match_mod tps bds match_mod


(* Compute binary decisions per predicted bit for some domain value *)

let calc_model_bds_per_bit dfdsums cfspec cinit_tps model =
  let env, add_var = make_env dfdsums in
  let cfspec_bits = calc_fspec_bits cfspec in
  let rec loop_mod tps bds =
    calc_mod_complexity cfspec cfspec_bits loop_match_mod tps bds
  and loop_match_mod tps bds = function
    | Shave (var_ix, cnstr, sh_mod, def_mod) ->
        let new_bds = bds +. 1.0 in
        (match Res.Array.get env var_ix with
        | FDStrct (var_cnstr, subs) when var_cnstr = cnstr ->
            Array.iter add_var subs;
            loop_mod tps new_bds sh_mod
        | FDAtom var_cnstr when var_cnstr = cnstr -> loop_mod tps new_bds sh_mod
        | _ -> loop_mod tps new_bds def_mod)
    | Split (var_ix, models) ->
        let cnstr =
          match Res.Array.get env var_ix with
          | FDStrct (cnstr, subs) -> Array.iter add_var subs; cnstr
          | FDAtom cnstr -> cnstr in
        loop_mod tps (bds +. log2 (float (Array.length models))) models.(cnstr)
    | PSplit (var_ix, maybe_models, def_mod) ->
        let new_bds = bds +. log2 (float (count_some maybe_models + 1)) in
        match Res.Array.get env var_ix with
        | FDStrct (cnstr, subs) ->
            (match maybe_models.(cnstr) with
            | Some model -> Array.iter add_var subs; loop_mod tps new_bds model
            | None -> loop_mod tps new_bds def_mod)
        | FDAtom cnstr ->
            match maybe_models.(cnstr) with
            | Some model -> loop_mod tps new_bds model
            | None -> loop_mod tps new_bds def_mod in
  loop_mod cinit_tps 0.0 model


(* Compute binary decisions per predicted bit for a set of domain values.
   Takes into account the entropy of the domain values. *)

let calc_model_complexity dfspec dvars cfspec cinit_tps model =
  let cfspec_bits = calc_fspec_bits cfspec in
  let n_dvars = Array.length dvars in
  let trans_tbl_ref = ref (Res.Array.init n_dvars id) in
  let wrap_trans_tbl var_ix n_subs f =
    let trans_tbl = !trans_tbl_ref in
    let old_trans = Res.Array.copy trans_tbl in
    let lix = Res.Array.lix trans_tbl in
    let n_subs_1 = n_subs - 1 in
    if n_subs_1 <> 0 then
      for i = 0 to lix do
        let other_var_ix = Res.Array.get trans_tbl i in
        if other_var_ix > var_ix then
        Res.Array.set trans_tbl i (other_var_ix + n_subs_1)
      done;
    for i = 0 to n_subs_1 do Res.Array.add_one trans_tbl (var_ix + i) done;
    let res = f () in
    trans_tbl_ref := old_trans;
    res in
  let rec loop_mod dvars tps bds =
    calc_mod_complexity cfspec cfspec_bits (loop_match_mod dvars) tps bds
  and loop_match_mod dvars tps bds = function
    | Shave (pre_ix, cnstr, sh_mod, def_mod) ->
        let var_ix = Res.Array.get !trans_tbl_ref pre_ix in
        let { histo = histo; tp = tp; samples = samples } = dvars.(var_ix) in
        let n_samples = Array.length samples in
        let bits = calc_entropy histo n_samples in
        let new_bds = bds +. bits in
        let n_subs = Array.length dfspec.(tp).(cnstr) in
        let n_cnstr = histo.(cnstr) in
        if n_cnstr = 0 then
          let new_dvars = atomic_shave_vars dvars var_ix in
          wrap_trans_tbl var_ix n_subs (fun () ->
            loop_mod new_dvars tps new_bds def_mod)
        else
          if n_cnstr = n_samples then
            let new_dvars =
              if n_subs = 0 then atomic_shave_vars dvars var_ix
              else shave_with_sub_vars dfspec dvars var_ix cnstr in
            wrap_trans_tbl var_ix n_subs (fun () ->
              loop_mod new_dvars tps new_bds sh_mod)
          else
            let new_dvars, def_dvars =
              split_cnstr_with_sub_vars dfspec dvars var_ix cnstr in
            let new_c =
              wrap_trans_tbl var_ix n_subs (fun () ->
                loop_mod new_dvars tps new_bds sh_mod) in
            let def_c =
              wrap_trans_tbl var_ix 0 (fun () ->
                loop_mod def_dvars tps new_bds def_mod) in
            let f_samples = float n_samples in
            let scaled_new_c = float n_cnstr *. new_c in
            let scaled_def_c = float (n_samples - n_cnstr) *. def_c in
            (scaled_new_c +. scaled_def_c) /. f_samples
    | Split (pre_ix, models) ->
        let var_ix = Res.Array.get !trans_tbl_ref pre_ix in
        let { samples = samples; tp = tp; histo = histo } = dvars.(var_ix) in
        let n_samples = Array.length samples in
        let sub_tps = dfspec.(tp) in
        let bits = calc_entropy histo n_samples in
        if bits > 0.0 then
          let new_bds = bds +. bits in
          let split_dvars = split_with_sub_vars 0 dfspec dvars var_ix in
          let colli cnstr c model =
            let n_sub_samples = histo.(cnstr) in
            if n_sub_samples = 0 then c
            else
              let new_dvars = split_dvars.(cnstr) in
              let n_subs = Array.length sub_tps.(cnstr) in
              let sub_c =
                wrap_trans_tbl var_ix n_subs (fun () ->
                  loop_mod new_dvars tps new_bds model) in
              c +. float n_sub_samples *. sub_c in
          array_fold_lefti colli 0.0 models /. float n_samples
        else
          let cnstr = array_find_index ((=) n_samples) histo in
          let model = models.(cnstr) in
          let new_dvars = shave_with_sub_vars dfspec dvars var_ix cnstr in
          let n_subs = Array.length sub_tps.(cnstr) in
          wrap_trans_tbl var_ix n_subs (fun () ->
            loop_mod new_dvars tps bds model)
    | PSplit (pre_ix, maybe_models, def_mod) ->
        let var_ix = Res.Array.get !trans_tbl_ref pre_ix in
        let { samples = samples; tp = tp; histo = histo } = dvars.(var_ix) in
        let n_samples = Array.length samples in
        let sub_tps = dfspec.(tp) in
        let bits = calc_entropy histo n_samples in
        if bits > 0.0 then
          let new_bds = bds +. bits in
          let none_ix, split_dvars, def_dvars =
            maybe_split_with_sub_vars dfspec dvars var_ix maybe_models in
          let colli cnstr c = function
            | None -> c
            | Some model ->
                let n_sub_samples = histo.(cnstr) in
                if n_sub_samples = 0 then c
                else
                  let new_dvars = split_dvars.(cnstr) in
                  let n_subs = Array.length sub_tps.(cnstr) in
                  let sub_c =
                    wrap_trans_tbl var_ix n_subs (fun () ->
                      loop_mod new_dvars tps new_bds model) in
                  c +. float n_sub_samples *. sub_c in
          let some_c = array_fold_lefti colli 0.0 maybe_models in
          if none_ix = 0 then some_c /. float n_samples
          else
            let def_c =
              wrap_trans_tbl var_ix 0 (fun () ->
                loop_mod def_dvars tps new_bds def_mod) in
            (float none_ix *. def_c +. some_c) /. float n_samples
        else
          let cnstr = array_find_index ((=) n_samples) histo in
          let model =
            match maybe_models.(cnstr) with
            | Some model -> model
            | None -> def_mod in
          let new_dvars = shave_with_sub_vars dfspec dvars var_ix cnstr in
          let n_subs = Array.length sub_tps.(cnstr) in
          wrap_trans_tbl var_ix n_subs (fun () ->
            loop_mod new_dvars tps bds model) in
  loop_mod dvars cinit_tps 0.0 model
