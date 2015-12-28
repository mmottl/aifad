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

(* Shave domain variables with redundant constructors *)

type sh_chain = (var_ix * tp * cnstr) one_list
type sh_el = FinVar of var_ix | ShVar of sh_chain * var_ix
type dom_ixs = sh_el array

let dummy_sh_el = FinVar 0
let dom_ixs_iota n = Array.init n (fun n -> FinVar n)

let rec fill_radix dom_ixs radix dom_ix lix ix =
  let new_ix = ix - 1 in
  dom_ixs.(dom_ix + new_ix) <- ShVar (radix, new_ix);
  if new_ix > lix then fill_radix dom_ixs radix dom_ix lix new_ix

let dshave dom_ixs (ShInfo (pos_infos, dvars)) n_sh_dvars =
  let new_dom_ixs = Array.make n_sh_dvars dummy_sh_el in
  let sh_dvars = Array.make n_sh_dvars dummy_var in
  let rec sub_loop rev_radix dvars lix sh_dvar_ix sub_infos =
    let radix = one_list_rev rev_radix in
    let rec inner_loop lix sh_dvar_ix = function
      | [] ->
          let n_dvars = Array.length dvars in
          let n_last = n_dvars - lix in
          if n_last > 0 then (
            Array.blit dvars lix sh_dvars sh_dvar_ix n_last;
            fill_radix new_dom_ixs radix (sh_dvar_ix - lix) lix n_dvars;
            sh_dvar_ix + n_last)
          else sh_dvar_ix
      | (sh_ix, cnstr, sh_info) :: rest ->
          let n_between = sh_ix - lix in
          if n_between > 0 then (
            Array.blit dvars lix sh_dvars sh_dvar_ix n_between;
            fill_radix new_dom_ixs radix (sh_dvar_ix - lix) lix sh_ix);
          let new_lix = sh_ix + 1 in
          let new_sh_dvar_ix =
            let new_sh_dvar_ix = sh_dvar_ix + n_between in
            match sh_info with
            | None | Some (0, _) -> new_sh_dvar_ix
            | Some (_, ShInfo (sub_infos, sub_vars)) ->
                let new_rev_radix =
                  OneCons ((sh_ix, dvars.(sh_ix).tp, cnstr), rev_radix) in
                sub_loop new_rev_radix sub_vars 0 new_sh_dvar_ix sub_infos in
          inner_loop new_lix new_sh_dvar_ix rest in
    inner_loop lix sh_dvar_ix sub_infos in
  let rec loop lix sh_dvar_ix = function
    | [] ->
        let n_last = Array.length dvars - lix in
        if n_last > 0 then (
          Array.blit dvars lix sh_dvars sh_dvar_ix n_last;
          Array.blit dom_ixs lix new_dom_ixs sh_dvar_ix n_last);
        new_dom_ixs, sh_dvars
    | (sh_ix, cnstr, sh_info) :: rest ->
        let n_between = sh_ix - lix in
        if n_between > 0 then (
          Array.blit dvars lix sh_dvars sh_dvar_ix n_between;
          Array.blit dom_ixs lix new_dom_ixs sh_dvar_ix n_between);
        let new_lix = sh_ix + 1 in
        let new_sh_dvar_ix =
          let new_sh_dvar_ix = sh_dvar_ix + n_between in
          match sh_info with
          | None | Some (0, _) -> new_sh_dvar_ix
          | Some (_, ShInfo (sub_infos, sub_vars)) ->
              let rev_radix =
                match dom_ixs.(sh_ix) with
                | FinVar ix -> OneEl (ix, dvars.(sh_ix).tp, cnstr)
                | ShVar (one_lst, last_ix) ->
                    let one_el = last_ix, dvars.(sh_ix).tp, cnstr in
                    OneCons (one_el, one_list_rev one_lst) in
              sub_loop rev_radix sub_vars 0 new_sh_dvar_ix sub_infos in
        loop new_lix new_sh_dvar_ix rest in
  loop 0 0 pos_infos


(* Shave domain indices and find shaved index bounds *)

let rec shave_lbound ix_cnt sh_ix dom_ixs ix =
  if ix >= 0 then
    match dom_ixs.(ix) with
    | FinVar _ -> ix + 1
    | ShVar (OneEl (var_ix, _, _), _)
    | ShVar (OneCons ((var_ix, _, _), _), _) when var_ix <> sh_ix -> ix + 1
    | ShVar (OneEl _, last_ix) ->
        dom_ixs.(ix) <- FinVar (ix_cnt + last_ix);
        shave_lbound ix_cnt sh_ix dom_ixs (ix - 1)
    | ShVar (OneCons (_, radix), last_ix) ->
        let new_radix =
          match radix with
          | OneEl (var_ix, tp, cnstr) -> OneEl (ix_cnt + var_ix, tp, cnstr)
          | OneCons ((var_ix, tp, cnstr), rest) ->
              OneCons ((ix_cnt + var_ix, tp, cnstr), rest) in
        dom_ixs.(ix) <- ShVar (new_radix, last_ix);
        shave_lbound ix_cnt sh_ix dom_ixs (ix - 1)
  else 0

let rec shave_rbound ix_cnt sh_ix dom_ixs ix =
  if ix < Array.length dom_ixs then
    match dom_ixs.(ix) with
    | FinVar _ -> ix
    | ShVar (OneEl (var_ix, _, _), _)
    | ShVar (OneCons ((var_ix, _, _), _), _) when var_ix <> sh_ix -> ix
    | ShVar (OneEl _, last_ix) ->
        dom_ixs.(ix) <- FinVar (ix_cnt + last_ix);
        shave_rbound ix_cnt sh_ix dom_ixs (ix + 1)
    | ShVar (OneCons (_, radix), last_ix) ->
        let new_radix =
          match radix with
          | OneEl (var_ix, tp, cnstr) -> OneEl (ix_cnt + var_ix, tp, cnstr)
          | OneCons ((var_ix, tp, cnstr), rest) ->
              OneCons ((ix_cnt + var_ix, tp, cnstr), rest) in
        dom_ixs.(ix) <- ShVar (new_radix, last_ix);
        shave_rbound ix_cnt sh_ix dom_ixs (ix + 1)
  else ix
