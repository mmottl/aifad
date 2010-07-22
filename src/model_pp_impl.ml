(*  File: model_pp_impl.ml

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

(* $Id: model_pp_impl.ml,v 1.27 2006/01/17 00:23:38 mottl Exp $ *)

open Format
open Utils
open Algdt_pp
open Algdt_types
open Deco_intf
open Model_pp_intf

module TpMap = IntMap
module VarMap = IntMap

module Make (Spec : SPEC) = struct

open Spec

(* Conversion tables from immediate to string encodings *)

let { tp_tbl = dtp_tbl; cnstr_tbl = dcnstr_tbl; _ } = dispec_info
let { tp_tbl = ctp_tbl; cnstr_tbl = ccnstr_tbl; _ } = cispec_info

(* Specialized pretty-printer *)

module CIDataPP = Algdt_pp.Make_IData (struct let cnstr_tbl = ccnstr_tbl end)


(* Variable suffixes *)

let dsuf = "d"
let csuf = "c"


(* Print variable bindings in let-statements *)

let pp_vars ppf tps =
  if Array.length tps > 1 then pp_print_char ppf '(';
  fprintf ppf "@[%s_%s%d" ctp_tbl.(tps.(0)) csuf 1;
  for n = 1 to Array.length tps - 1 do
    fprintf ppf ",@ %s_%s%d" ctp_tbl.(tps.(n)) csuf (n + 1)
  done;
  fprintf ppf "@]";
  if Array.length tps > 1 then pp_print_char ppf ')'


(* Print variable models *)

let rec pp_vprod_el tps n ppf = function
  | TpVal dvar_mod -> pp_dvar_mod tps n ppf dvar_mod
  | ProdEls vprod_els -> pp_vprod_els tps n ppf vprod_els

and pp_vprod_el_arg tps n ppf = function
  | TpVal dvar_mod -> pp_vmod_arg tps n ppf dvar_mod
  | ProdEls vprod_els -> pp_vprod_els tps n ppf vprod_els

and pp_vprod_els tps n ppf vprod_els =
  pp_open_par_box ppf ();
  let new_n = pp_vprod_el tps n ppf vprod_els.(0) in
  let coll n vprod_el = fprintf ppf ",@ "; pp_vprod_el tps n ppf vprod_el in
  let new_n = array_fold_left1 coll new_n vprod_els in
  pp_close_par_box ppf ();
  new_n

and pp_dvar_mod tps n ppf = function
  | DVarStrct (tp, cnstr, vprod_el) ->
      pp_open_box ppf 2;
      pp_print_string ppf ccnstr_tbl.(tp).(cnstr);
      pp_print_space ppf ();
      let new_n = pp_vprod_el_arg tps n ppf vprod_el in
      pp_close_box ppf ();
      new_n
  | DVarFree dsum -> CIDataPP.pp_sum ppf dsum; n
  | DVar tp -> fprintf ppf "%s_%s%d" ctp_tbl.(tp) csuf n; n + 1

and pp_vmod_arg tps n ppf = function
  | DVarStrct (tp, cnstr, vprod_el) ->
      pp_open_par_box ppf ();
      pp_print_string ppf ccnstr_tbl.(tp).(cnstr);
      pp_print_space ppf ();
      let new_n = pp_vprod_el_arg tps n ppf vprod_el in
      pp_close_par_box ppf ();
      new_n
  | DVarFree dsum -> CIDataPP.pp_sum_arg ppf dsum; n
  | DVar tp -> fprintf ppf "%s_%s%d" ctp_tbl.(tp) csuf n; n + 1

let pp_vprod_els_start tps ppf vprod_els =
  if Array.length vprod_els = 1 then
    ignore (pp_vprod_el tps 1 ppf vprod_els.(0))
  else ignore (pp_vprod_els tps 1 ppf vprod_els)


(* Print opening of pattern-matching *)

let pp_open_match ppf var_ix var_map =
  fprintf ppf "@[(match %s with" (VarMap.find var_ix var_map)


(* Print variable bindings *)

let pp_dmodel ppf dmodel ddpat =
  let ix_cnt_ref = ref 0 in
  let var_map_ref = ref VarMap.empty in
  let tp_cnts_ref = ref TpMap.empty in

  (* Print variable bindings in patterns *)
  let pp_pat ppf prod_el =
    let rec loop = function
      | None -> fprintf ppf "_"
      | Some (`TpVal tp) ->
          let ix_cnt = !ix_cnt_ref in
          ix_cnt_ref := ix_cnt + 1;
          let tp_cnts = !tp_cnts_ref in
          let tp_cnt = try TpMap.find tp tp_cnts with Not_found -> 1 in
          tp_cnts_ref := TpMap.add tp (tp_cnt + 1) tp_cnts;
          let var_name = sprintf "%s_%s%d" dtp_tbl.(tp) dsuf tp_cnt in
          var_map_ref := VarMap.add ix_cnt var_name !var_map_ref;
          pp_print_string ppf var_name;
      | Some (`ProdEls prod_els) ->
          pp_open_par_box ppf ();
          loop prod_els.(0);
          let act prod_el = fprintf ppf ",@ "; loop prod_el in
          array_iter1 act prod_els;
          pp_close_par_box ppf () in
    loop prod_el in

  (* Print rule in pattern-matching *)
  let pp_match tp cnstr pat ppf =
    pp_open_box ppf 2;
    pp_print_string ppf dcnstr_tbl.(tp).(cnstr);
    if Array.length dfspec.(tp).(cnstr) > 0 then fprintf ppf "@ %a" pp_pat pat;
    pp_close_box ppf () in

  (* Print models *)
  let rec pp_dmodel ppf = function
    | DVal deco_sums ->
        if Array.length deco_sums = 1 then CIDataPP.pp_data_el ppf deco_sums.(0)
        else CIDataPP.pp_prod_els ppf deco_sums
    | DLet (dmatch_mod, tps, vprod_els) ->
        fprintf ppf "@[<2>@[let@ %a@ =@]@\n@[%a@]@]@ in@\n@[%a@]"
          pp_vars tps
          pp_dmatch_mod dmatch_mod
          (pp_vprod_els_start tps) vprod_els
    | DMatchMod dmatch_mod -> pp_dmatch_mod ppf dmatch_mod

  and pp_branch ppf dmodel =
    pp_print_string ppf " ->";
    (match dmodel with
    | DVal _ -> pp_print_space ppf ()
    | DLet _ | DMatchMod _ -> pp_force_newline ppf ());
    pp_dmodel ppf dmodel

  (* Print match models *)
  and pp_dmatch_mod ppf dmatch_mod =
    let ix_cnt = !ix_cnt_ref in
    let var_map = !var_map_ref in
    let tp_cnts = !tp_cnts_ref in
    (match dmatch_mod with
    | DShave (var_ix, tp, cnstr, pat, dsh_mod, dmodel) ->
        pp_open_match ppf var_ix var_map;
        fprintf ppf "@\n@[<4>| @[%t@]%a@]"
          (pp_match tp cnstr pat) pp_branch dsh_mod;
        fprintf ppf "@\n@[<4>| _%a@]" pp_branch dmodel
    | DSplit (var_ix, tp, dmodels) ->
        let pp_pat_match cnstr pat dmodel =
          ix_cnt_ref := ix_cnt;
          var_map_ref := var_map;
          tp_cnts_ref := tp_cnts;
          fprintf ppf "@\n@[<4>| @[%t@]%a@]"
            (pp_match tp cnstr pat) pp_branch dmodel in
        pp_open_match ppf var_ix var_map;
        let acti cnstr (pat, dmodel) = pp_pat_match cnstr pat dmodel in
        Array.iteri acti dmodels
    | DPSplit (var_ix, tp, maybe_dmodels, dmodel) ->
        pp_open_match ppf var_ix var_map;
        let pp_pat_match cnstr pat dmodel =
          ix_cnt_ref := ix_cnt;
          var_map_ref := var_map;
          tp_cnts_ref := tp_cnts;
          fprintf ppf "@\n@[<4>| @[%t@]%a@]"
            (pp_match tp cnstr pat) pp_branch dmodel in
        let acti cnstr = function
          | Some (pat, dmodel) -> pp_pat_match cnstr pat dmodel
          | None -> () in
        Array.iteri acti maybe_dmodels;
        ix_cnt_ref := ix_cnt;
        var_map_ref := var_map;
        tp_cnts_ref := tp_cnts;
        fprintf ppf "@\n@[<4>| _%a@]" pp_branch dmodel);
    fprintf ppf "@])" in

  fprintf ppf "@[<2>@[let@ model@ @[%a@]@ =@]@\n@[%a@]@]"
    pp_pat ddpat
    pp_dmodel dmodel

end
