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
open Model_types
open Deco_intf
module VarSet = IntSet
module VarMap = IntMap

let coll_sub_map ix_cnt ix m sub_tp = VarMap.add (ix_cnt + ix) sub_tp m

let calc_sub_vars sums cnstr ix_cnt =
  let rec loop sub_vars var_ix =
    if var_ix < 0 then sub_vars
    else loop (VarSet.add (var_ix + ix_cnt) sub_vars) (var_ix - 1)
  in
  loop VarSet.empty (Array.length sums.(cnstr) - 1)

module Make (Spec : SPEC) = struct
  open Spec

  let failwith str = failwith ("Deco_impl." ^ str)
  let deco_dsum_of_fdsum, decorate_sums = make_deco_dsum_cnv cfspec cispec

  (* Decorate variable models *)

  let rec deco_var_mods var_tp_lst prod_el var_mods =
    if Array.length var_mods = 1 then
      match prod_el with
      | TpVal tp ->
          let dvar_mod, new_var_tp_lst =
            deco_var_mod var_tp_lst tp var_mods.(0)
          in
          (TpVal dvar_mod, new_var_tp_lst)
      | ProdEls _ -> failwith "deco_var_mods: expected TpVal"
    else deco_var_mods_prod var_tp_lst var_mods prod_el

  and deco_var_mod var_tp_lst tp = function
    | VarFree fdsum -> (DVarFree (deco_dsum_of_fdsum tp fdsum), var_tp_lst)
    | Var -> (DVar tp, tp :: var_tp_lst)
    | VarStrct (cnstr, var_mods) ->
        let dvar_mod, var_tp_lst =
          match cispec.(tp) with
          | Prod _ -> failwith "deco_var_mod: expected Sums"
          | Sums sums -> (
              match sums.(cnstr) with
              | IAtom -> failwith "deco_var_mod: expected IStrct"
              | IStrct prod_el -> deco_var_mods var_tp_lst prod_el var_mods)
        in
        (DVarStrct (tp, cnstr, dvar_mod), var_tp_lst)

  and deco_var_mods_prod var_tp_lst var_mods = function
    | ProdEls prod_els ->
        let dprod_els, new_var_tp_lst, _ =
          deco_vprod_els var_tp_lst prod_els 0 var_mods
        in
        (ProdEls dprod_els, new_var_tp_lst)
    | TpVal tp -> (
        match cispec.(tp) with
        | Sums _ -> failwith "deco_var_mods_prod: expected Prod"
        | Prod prod_el -> deco_var_mods_prod var_tp_lst var_mods prod_el)

  and deco_vprod_els var_tp_lst prod_els ix var_mods =
    let dprod_el, new_var_tp_lst, new_ix =
      deco_vprod_el var_tp_lst ix var_mods prod_els.(0)
    in
    let dprod_els = Array.make (Array.length prod_els) dprod_el in
    let colli i (var_tp_lst, ix) prod_el =
      let dprod_el, new_var_tp_lst, new_ix =
        deco_vprod_el var_tp_lst ix var_mods prod_el
      in
      dprod_els.(i) <- dprod_el;
      (new_var_tp_lst, new_ix)
    in
    let new_var_tp_lst, new_ix =
      array_fold_left1i colli (new_var_tp_lst, new_ix) prod_els
    in
    (dprod_els, new_var_tp_lst, new_ix)

  and deco_vprod_el var_tp_lst ix var_mods = function
    | ProdEls prod_els ->
        let dprod_els, new_var_tp_lst, new_ix =
          deco_vprod_els var_tp_lst prod_els ix var_mods
        in
        (ProdEls dprod_els, new_var_tp_lst, new_ix)
    | TpVal tp -> (
        match cispec.(tp) with
        | Prod sub_prod_el -> deco_vprod_el var_tp_lst ix var_mods sub_prod_el
        | Sums _ ->
            let dvar_mod, new_var_tp_lst =
              deco_var_mod var_tp_lst tp var_mods.(ix)
            in
            (TpVal dvar_mod, new_var_tp_lst, ix + 1))

  (* Decorate model *)

  let prod_el_of_tp tp = TpVal tp

  let rec deco_model ix_cnt var_tps cdoms = function
    | Val sums -> (DVal (decorate_sums cdoms sums), VarSet.empty)
    | Let (match_mod, var_mods) ->
        let dvar_mods, var_tp_rev_lst, _ = deco_vprod_els [] cdoms 0 var_mods in
        let var_tp_ar = array_of_rev_list var_tp_rev_lst in
        let new_cdoms = array_map_of_rev_list prod_el_of_tp var_tp_rev_lst in
        let dmatch_mod, used_vars =
          deco_match_mod ix_cnt var_tps new_cdoms match_mod
        in
        (DLet (dmatch_mod, var_tp_ar, dvar_mods), used_vars)
    | MatchMod match_mod ->
        let dmatch_mod, used_vars =
          deco_match_mod ix_cnt var_tps cdoms match_mod
        in
        (DMatchMod dmatch_mod, used_vars)

  and deco_match_mod ix_cnt var_tps cdoms = function
    | Shave (var_ix, cnstr, sh_model, model) ->
        let tp = VarMap.find var_ix var_tps in
        let sums = dfspec.(tp) in
        let sub_tps = sums.(cnstr) in
        let dsh_model, match_used_vars =
          let new_ix_cnt = ix_cnt + Array.length sub_tps in
          let colli = coll_sub_map ix_cnt in
          let new_var_tps = array_fold_lefti colli var_tps sub_tps in
          deco_model new_ix_cnt new_var_tps cdoms sh_model
        in
        let sub_vars = calc_sub_vars sums cnstr ix_cnt in
        let used_subs_set = VarSet.inter sub_vars match_used_vars in
        let used_subs =
          List.map (fun el -> el - ix_cnt) (VarSet.elements used_subs_set)
        in
        let dmodel, sh_used_vars = deco_model ix_cnt var_tps cdoms model in
        let match_used_no_subs = VarSet.diff match_used_vars used_subs_set in
        let used_vars = VarSet.union match_used_no_subs sh_used_vars in
        let used_vars = VarSet.add var_ix used_vars in
        (DShave (var_ix, tp, cnstr, used_subs, dsh_model, dmodel), used_vars)
    | Split (var_ix, models) ->
        let tp = VarMap.find var_ix var_tps in
        let sums = dfspec.(tp) in
        let used_vars_ref = ref (VarSet.singleton var_ix) in
        let dmodels =
          let colli = coll_sub_map ix_cnt in
          let sub_ix_cnt el = el - ix_cnt in
          Array.mapi
            (fun cnstr model ->
              let sub_tps = sums.(cnstr) in
              let new_ix_cnt = ix_cnt + Array.length sub_tps in
              let new_var_tps = array_fold_lefti colli var_tps sub_tps in
              let dmodel, match_used_vars =
                deco_model new_ix_cnt new_var_tps cdoms model
              in
              let sub_vars = calc_sub_vars sums cnstr ix_cnt in
              let used_subs_set = VarSet.inter sub_vars match_used_vars in
              let used_subs =
                List.map sub_ix_cnt (VarSet.elements used_subs_set)
              in
              let match_used_no_subs =
                VarSet.diff match_used_vars used_subs_set
              in
              used_vars_ref := VarSet.union match_used_no_subs !used_vars_ref;
              (used_subs, dmodel))
            models
        in
        (DSplit (var_ix, tp, dmodels), !used_vars_ref)
    | PSplit (var_ix, maybe_models, model) ->
        let tp = VarMap.find var_ix var_tps in
        let sums = dfspec.(tp) in
        let ddef_mod, def_used_vars = deco_model ix_cnt var_tps cdoms model in
        let used_vars_ref = ref (VarSet.add var_ix def_used_vars) in
        let maybe_dmodels =
          let colli = coll_sub_map ix_cnt in
          let sub_ix_cnt el = el - ix_cnt in
          Array.mapi
            (fun cnstr -> function
              | Some model ->
                  let sub_tps = sums.(cnstr) in
                  let new_ix_cnt = ix_cnt + Array.length sub_tps in
                  let new_var_tps = array_fold_lefti colli var_tps sub_tps in
                  let dmodel, match_used_vars =
                    deco_model new_ix_cnt new_var_tps cdoms model
                  in
                  let sub_vars = calc_sub_vars sums cnstr ix_cnt in
                  let used_subs_set = VarSet.inter sub_vars match_used_vars in
                  let used_subs =
                    List.map sub_ix_cnt (VarSet.elements used_subs_set)
                  in
                  let match_used_no_subs =
                    VarSet.diff match_used_vars used_subs_set
                  in
                  used_vars_ref :=
                    VarSet.union match_used_no_subs !used_vars_ref;
                  Some (used_subs, dmodel)
              | None -> None)
            maybe_models
        in
        (DPSplit (var_ix, tp, maybe_dmodels, ddef_mod), !used_vars_ref)

  (* Rewrite variable indices according to actually used variables *)

  let rewrite_dmodel (dmodel, used_vars) =
    let from_ix_ref = ref 0 in
    let to_ix_ref = ref 0 in
    let var_map_ref = ref VarMap.empty in

    (* Decorate a product element according to used variables *)
    let calc_prod_el_pat var_lst prod_el =
      let ix_ref = ref 0 in
      let var_lst_ref = ref var_lst in
      let rec loop = function
        | ProdEls prod_els ->
            let vars_lst = !var_lst_ref in
            let new_prod_els = Array.map loop prod_els in
            if !var_lst_ref == vars_lst then None
            else Some (`ProdEls new_prod_els)
        | TpVal tp -> (
            match dispec.(tp) with
            | Prod prod_el -> loop prod_el
            | Sums _ -> (
                let from_ix = !from_ix_ref in
                from_ix_ref := from_ix + 1;
                match !var_lst_ref with
                | [] -> None
                | used_var :: rest ->
                    let ix = !ix_ref in
                    ix_ref := ix + 1;
                    if ix < used_var then None
                    else
                      let to_ix = !to_ix_ref in
                      var_map_ref := VarMap.add from_ix to_ix !var_map_ref;
                      to_ix_ref := to_ix + 1;
                      var_lst_ref := rest;
                      Some (`TpVal tp)))
      in
      loop prod_el
    in

    (* Compute pattern from used subvariables *)
    let calc_pat tp cnstr used_subs =
      match dispec.(tp) with
      | Prod _ -> failwith "calc_pat: expected Sums"
      | Sums sums -> (
          match sums.(cnstr) with
          | IAtom -> None
          | IStrct prod_el -> calc_prod_el_pat used_subs prod_el)
    in

    (* Perform rewriting *)
    let rec rwr_dmodel = function
      | DVal _ as dmodel -> dmodel
      | DLet (dmatch_mod, var_tp_ar, dvar_mods) ->
          DLet (rwr_dmatch_mod dmatch_mod, var_tp_ar, dvar_mods)
      | DMatchMod dmatch_mod -> DMatchMod (rwr_dmatch_mod dmatch_mod)
    and rwr_dmatch_mod dmatch_mod =
      let from_ix = !from_ix_ref in
      let to_ix = !to_ix_ref in
      let var_map = !var_map_ref in
      match dmatch_mod with
      | DShave (var_ix, tp, cnstr, used_subs, dsh_mod, dmodel) ->
          let rvar_ix = VarMap.find var_ix var_map in
          let rdmodel = rwr_dmodel dmodel in
          from_ix_ref := from_ix;
          to_ix_ref := to_ix;
          var_map_ref := var_map;
          let pat = calc_pat tp cnstr used_subs in
          let rdsh_mod = rwr_dmodel dsh_mod in
          DShave (rvar_ix, tp, cnstr, pat, rdsh_mod, rdmodel)
      | DSplit (var_ix, tp, dmodels) ->
          let rvar_ix = VarMap.find var_ix var_map in
          let cnvi cnstr (used_subs, dmodel) =
            from_ix_ref := from_ix;
            to_ix_ref := to_ix;
            var_map_ref := var_map;
            let pat = calc_pat tp cnstr used_subs in
            (pat, rwr_dmodel dmodel)
          in
          let rdmodels = Array.mapi cnvi dmodels in
          DSplit (rvar_ix, tp, rdmodels)
      | DPSplit (var_ix, tp, maybe_dmodels, dmodel) ->
          let rvar_ix = VarMap.find var_ix var_map in
          let rdmodel = rwr_dmodel dmodel in
          let cnvi cnstr = function
            | Some (used_subs, dmodel) ->
                from_ix_ref := from_ix;
                to_ix_ref := to_ix;
                var_map_ref := var_map;
                let pat = calc_pat tp cnstr used_subs in
                Some (pat, rwr_dmodel dmodel)
            | None -> None
          in
          let maybe_rdmodels = Array.mapi cnvi maybe_dmodels in
          DPSplit (rvar_ix, tp, maybe_rdmodels, rdmodel)
    in
    let ddpat = calc_prod_el_pat (VarSet.elements used_vars) init_prod_el in
    (rwr_dmodel dmodel, ddpat)

  (* Decorate model for pretty-printing *)

  let decorate_model model =
    let var_tps = array_fold_righti VarMap.add init_tps VarMap.empty in
    let n_init_tps = Array.length init_tps in
    rewrite_dmodel (deco_model n_init_tps var_tps init_prod_els model)
end
