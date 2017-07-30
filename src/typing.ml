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
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation,
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
*)

open Printf
open Utils
open Algdt_types
open Algdt_utils


(* Compute immediate representation of types *)

let hash_size = 541

let iprod_of_prod ~fail_tp ~tp_htbl =
  let rec loop = function
    | ProdEls ps -> ProdEls (Array.map loop ps)
    | TpVal tp ->
        try TpVal (Hashtbl.find tp_htbl tp)
        with Not_found -> fail_tp tp in
  loop

let sum_name = function Atom name | Strct (name, _) -> name

let calc_pre_ispec_info defs =
  let tp_htbl = Hashtbl.create hash_size in

  let tp_tbl, cnstr_tbl =
    let cnv_def n (lhs, rhs) =
      if Hashtbl.mem tp_htbl lhs then
        let err = sprintf "type defined more than once: '%s'" lhs in
        failwith err
      else (
        Hashtbl.add tp_htbl lhs n;
        match rhs with
        | Sums ss -> lhs, Array.map sum_name ss
        | Prod _ -> lhs, [||]) in
    array_mapi2 cnv_def defs in

  let ispec =
    let cnstr_htbl = Hashtbl.create hash_size in
    let cnv_def (lhs, rhs) =
      let fail_tp tp =
        let err =
          sprintf "rhs-type '%s' unknown in definition of '%s'" tp lhs in
        failwith err in
      match rhs with
      | Prod pe -> Prod (iprod_of_prod ~fail_tp ~tp_htbl pe)
      | Sums ss ->
          let fail_tag lhs tag =
            let err =
              sprintf
                "tag '%s' used another time in definition of '%s'" tag lhs in
            failwith err in
          let cnv_sum n = function
            | Atom tag ->
                let htbl_el = lhs, tag in
                if Hashtbl.mem cnstr_htbl htbl_el then fail_tag lhs tag
                else (Hashtbl.add cnstr_htbl htbl_el n; IAtom)
            | Strct (tag, pe) ->
                let htbl_el = lhs, tag in
                if Hashtbl.mem cnstr_htbl htbl_el then fail_tag lhs tag
                else (
                  Hashtbl.add cnstr_htbl htbl_el n;
                  IStrct (iprod_of_prod ~fail_tp ~tp_htbl pe)) in
          Sums (Array.mapi cnv_sum ss) in
    Array.map cnv_def defs in
  tp_tbl, cnstr_tbl, tp_htbl, ispec


(* CFG *)

type cfg_prod = CProd | CSum of cnstr

module AlgDtCfgSpec = struct
  type t = unit
  type nt = tp
  type symbol = NT of nt | T of t
  type prod = cfg_prod
  let compare_t = compare
  let compare_nt = compare
  let compare_prod = compare
end

module MyCfg = Cfg.Cfg_impl.Make (AlgDtCfgSpec)

open AlgDtCfgSpec
open MyCfg

let syms_of_prod_el pe =
  let acc_ref = ref [] in
  let rec loop = function
    | TpVal tp -> acc_ref := NT tp :: !acc_ref
    | ProdEls ps -> array_rev_iter loop ps in
  loop pe;
  !acc_ref

let cfg_coll_def nt_n cfg = function
  | Prod pe -> add_prod cfg nt_n CProd (syms_of_prod_el pe)
  | Sums ss ->
      let coll_sum prod_n cfg = function
        | IAtom -> add_prod cfg nt_n (CSum prod_n)  [T ()]
        | IStrct pe -> add_prod cfg nt_n (CSum prod_n) (syms_of_prod_el pe) in
      array_fold_lefti coll_sum cfg ss

let cfg_of_pre_ispec_info (_, _, _, ispec) =
  array_fold_lefti cfg_coll_def MyCfg.empty ispec


(* Compute ispec for some domain *)

let map_prod_el f =
  let rec loop = function
    | TpVal tp -> TpVal (f tp)
    | ProdEls ps -> ProdEls (Array.map loop ps) in
  loop

let calc_ispec_info live_gr dtp (tp_tbl, cnstr_tbl, _, ispec) =
  let gr_dom = grammar_contents (MyCfg.grammar_of_live live_gr) in
  let tp_tbl_len = Array.length tp_tbl in
  let tp_cnv_tbl = Array.make tp_tbl_len 0 in
  let cnv_tp tp = tp_cnv_tbl.(tp) in
  let cnstr_cnv_tbl = Array.make tp_tbl_len [||] in
  let dtp_htbl = Hashtbl.create hash_size in
  let coll_nts nt _ new_nt =
    let the_nt, next_nt =
      if dtp = nt then (Hashtbl.add dtp_htbl tp_tbl.(nt) 0; 0, new_nt)
      else (Hashtbl.add dtp_htbl tp_tbl.(nt) new_nt; new_nt, new_nt + 1) in
    tp_cnv_tbl.(nt) <- the_nt;
    (match ispec.(nt) with
    | Sums sums -> cnstr_cnv_tbl.(nt) <- Array.make (Array.length sums) 0
    | Prod _ -> ());
    next_nt in
  let n_nt = NTMap.fold coll_nts gr_dom 1 in
  let dtp_tbl = Array.make n_nt "" in
  let dcnstr_tbl = Array.make n_nt [||] in
  let dcnstr_htbl = Hashtbl.create hash_size in
  let dispec = Array.make n_nt (Sums [||]) in
  let act_nts nt rhs =
    let new_nt = tp_cnv_tbl.(nt) in
    dtp_tbl.(new_nt) <- tp_tbl.(nt);
    match ispec.(nt) with
    | Prod pe -> dispec.(new_nt) <- Prod (map_prod_el cnv_tp pe)
    | Sums sums ->
        let rhs_lst = ProdSet.elements rhs in
        let sum_cnv_tbl = cnstr_cnv_tbl.(nt) in
        let cnstrs = cnstr_tbl.(nt) in
        let coll_tag new_tag = function
          | CSum tag, _ ->
              sum_cnv_tbl.(tag) <- new_tag;
              Hashtbl.add dcnstr_htbl (new_nt, cnstrs.(tag)) new_tag;
              new_tag + 1
          | CProd, _ -> assert false in
        let n_sums = List.fold_left coll_tag 0 rhs_lst in
        dcnstr_tbl.(new_nt) <- Array.make n_sums "";
        let new_sums = Array.make n_sums IAtom in
        let act_tag new_tag = function
          | CSum tag, _ ->
              dcnstr_tbl.(new_nt).(new_tag) <- cnstrs.(tag);
              (match sums.(tag) with
              | IStrct pe ->
                  new_sums.(new_tag) <- IStrct (map_prod_el cnv_tp pe)
              | IAtom -> ())
          | CProd, _ -> assert false in
        List.iteri act_tag rhs_lst;
        dispec.(new_nt) <- Sums new_sums in
  NTMap.iter act_nts gr_dom;
  {
    tp_tbl = dtp_tbl;
    cnstr_tbl = dcnstr_tbl;
    tp_htbl = dtp_htbl;
    cnstr_htbl = dcnstr_htbl;
    ispec = dispec;
  }


(* Flatten ispec *)

let flatten_ispec ispec =
  let rec flt_prod_el2 acc = function
    | ProdEls ps -> Array.fold_left flt_prod_el2 acc ps
    | TpVal tp ->
        match ispec.(tp) with
        | Sums _ -> tp :: acc
        | Prod pe -> flt_prod_el2 acc pe in
  let rec flt_prod_el1 pss = function
    | ProdEls ps -> flt_prod_el1 (ps :: pss) ps.(0)
    | TpVal tp ->
        match ispec.(tp) with
        | Sums _ -> List.fold_left (array_fold_left1 flt_prod_el2) [tp] pss
        | Prod pe -> flt_prod_el1 pss pe in
  let flt_prod_el pe = array_of_rev_list (flt_prod_el1 [] pe) in
  let flt_sum = function IAtom -> [||] | IStrct pe -> flt_prod_el pe in
  let flt_rhs = function
    | Prod pe -> [| flt_prod_el pe |]
    | Sums ss -> Array.map flt_sum ss in
  let fspec = Array.map flt_rhs ispec in
  fspec, get_init_tps ispec fspec
