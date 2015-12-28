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

open Utils
open Algdt_types
open Algdt_utils
open Model_types
open Model_utils

(* Factorization of models *)

type factor =
  | FactorNone
  | FactorVal of model
  | FactorLet of models * var_mods

let not_match_mod = function
  | MatchMod _ -> false
  | Val _ | Let _ -> true

let make_var_free fdsum = VarFree fdsum

let get_n_tpl models =
  match models.(0) with
  | Val fdsums -> Array.length fdsums
  | Let (_, var_mods) -> Array.length var_mods
  | MatchMod _ -> assert false (* impossible *)

let get_cnstr_strct models tpl_ix subs_ar =
  match models.(0) with
  | Val fdsums ->
      (match fdsums.(tpl_ix) with
      | FDAtom cnstr -> cnstr, false
      | FDStrct (cnstr, subs) ->
          subs_ar.(0) <- Array.map make_var_free subs;
          cnstr, true)
  | Let (_, var_mods) ->
      (match var_mods.(tpl_ix) with
      | VarStrct (cnstr, subs) -> subs_ar.(0) <- subs; cnstr, true
      | VarFree (FDAtom cnstr) -> cnstr, false
      | VarFree (FDStrct (cnstr, subs)) ->
          subs_ar.(0) <- Array.map make_var_free subs;
          cnstr, true
      | Var -> raise Exit)
  | MatchMod _ -> assert false (* impossible *)

let get_sub_cnstr_strct subs_ar tsubs =
  match tsubs.(0) with
  | VarStrct (cnstr, subs) -> subs_ar.(0) <- subs; cnstr, true
  | VarFree (FDAtom cnstr) -> cnstr, false
  | VarFree (FDStrct (cnstr, subs)) ->
      subs_ar.(0) <- Array.map make_var_free subs;
      cnstr, true
  | Var -> raise Exit

let cpy_subs subs_ar model_ix last_cnstr = function
  | VarStrct (cnstr, subs) when cnstr = last_cnstr -> subs_ar.(model_ix) <- subs
  | VarFree (FDStrct (cnstr, subs)) when cnstr = last_cnstr ->
      subs_ar.(model_ix) <- Array.map make_var_free subs
  | VarStrct _ | VarFree (FDAtom _ | FDStrct _) | Var -> raise Exit

let chk_atom last_cnstr = function
  | VarFree (FDAtom cnstr) when cnstr = last_cnstr -> ()
  | VarStrct _ | VarFree (FDAtom _ | FDStrct _) | Var -> raise Exit

let rec factorize_subs n_models_1 same_ref n_sub_same_ref subs_ar last_cnstr =
  let tsubs = transpose_matrix subs_ar in
  let n_tsubs = Array.length tsubs in
  let factorized_subs = Array.make n_tsubs Var in
  for ix = n_tsubs - 1 downto 0 do
    let tsub = tsubs.(ix) in
    try
      let last_sub_cnstr, is_sub_strct = get_sub_cnstr_strct subs_ar tsub in
      let factorized =
        if is_sub_strct then (
          for model_ix = 1 to n_models_1 do
            cpy_subs subs_ar model_ix last_sub_cnstr tsub.(model_ix)
          done;
          factorize_subs
            n_models_1 same_ref n_sub_same_ref subs_ar last_sub_cnstr)
      else (
        for model_ix = 1 to n_models_1 do
          chk_atom last_sub_cnstr tsub.(model_ix)
        done;
        VarFree (FDAtom last_sub_cnstr)) in
      factorized_subs.(ix) <- factorized
    with Exit ->
      same_ref := Right tsub :: !same_ref;
      incr n_sub_same_ref
  done;
  make_strct_var_mods last_cnstr factorized_subs

let factorize_no_match models =
  let n_models = Array.length models in
  let n_models_1 = n_models - 1 in
  let subs_ar = Array.make n_models [||] in
  let same_ref = ref [] in
  let factorized_ref = ref [] in
  let n_factorized_ref = ref 0 in
  let n_vars_ref = ref 0 in
  let n_sub_same_ref = ref 0 in
  let n_tpl = get_n_tpl models in
  for tpl_ix = n_tpl - 1 downto 0 do
    try
      let last_cnstr, is_strct = get_cnstr_strct models tpl_ix subs_ar in
      let factorized =
        if is_strct then (
          for model_ix = 1 to n_models_1 do
            match models.(model_ix) with
            | Val fdsums ->
                (match fdsums.(tpl_ix) with
                | FDStrct (cnstr, subs) when cnstr = last_cnstr ->
                    subs_ar.(model_ix) <- Array.map make_var_free subs
                | FDAtom _ | FDStrct _ -> raise Exit)
            | Let (_, var_mods) ->
                cpy_subs subs_ar model_ix last_cnstr var_mods.(tpl_ix)
            | MatchMod _ -> assert false (* impossible *)
          done;
          factorize_subs n_models_1 same_ref n_sub_same_ref subs_ar last_cnstr)
        else (
          for model_ix = 1 to n_models_1 do
            match models.(model_ix) with
            | Val fdsums ->
                if fdsum_cnstr fdsums.(tpl_ix) <> last_cnstr then raise Exit
            | Let (_, var_mods) -> chk_atom last_cnstr var_mods.(tpl_ix)
            | MatchMod _ -> assert false (* impossible *)
          done;
          VarFree (FDAtom last_cnstr)) in
      factorized_ref := factorized :: !factorized_ref;
      incr n_factorized_ref
    with Exit ->
      factorized_ref := Var :: !factorized_ref;
      incr n_vars_ref;
      same_ref := Left tpl_ix :: !same_ref
  done;
  let n_factorized = !n_factorized_ref in
  if n_factorized = 0 then FactorNone
  else
    let n_sub_same = !n_sub_same_ref in
    if n_factorized = n_tpl && n_sub_same = 0 then
      let fdsums =
        array_map_of_nlist sum_of_var_mod n_factorized !factorized_ref in
      FactorVal (Val fdsums)
    else
      let same = !same_ref in
      let n_vars = !n_vars_ref in
      let n_new_tpl = n_vars + n_sub_same in
      let cnvi model_ix = function
        | Val fdsums ->
            let new_fdsums = Array.make n_new_tpl dummy_fdsum in
            let acti ix = function
              | Left tpl_ix -> new_fdsums.(ix) <- fdsums.(tpl_ix)
              | Right tsub ->
                  match tsub.(model_ix) with
                  | VarFree fdsum -> new_fdsums.(ix) <- fdsum
                  | VarStrct _ | Var -> assert false (* impossible *) in
            List.iteri acti same;
            Val new_fdsums
        | Let (match_mod, var_mods) ->
            let new_var_mods = Array.make n_new_tpl Var in
            let acti ix = function
              | Left tpl_ix -> new_var_mods.(ix) <- var_mods.(tpl_ix)
              | Right tsub -> new_var_mods.(ix) <- tsub.(model_ix) in
            List.iteri acti same;
            if array_forall is_var new_var_mods then MatchMod match_mod
            else Let (match_mod, new_var_mods)
        | MatchMod _ -> assert false (* impossible *) in
      let new_models = Array.mapi cnvi models in
      let n_new_var_mods = n_factorized + n_vars in
      FactorLet (new_models, array_of_nlist n_new_var_mods !factorized_ref)

let factorize_models models =
  if array_forall not_match_mod models then factorize_no_match models
  else FactorNone
