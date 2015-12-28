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

open Format

open Utils
open Algdt_types
open Algdt_utils
open Model_types
open Typing

(* General utility functions on models *)

let dummy_model = Val dummy_fdsums
let is_var_free = function VarFree _ -> true | VarStrct _ | Var -> false
let is_var var_mod = var_mod = Var

let rec count_vars_var_mod = function
  | VarStrct (_, subs) -> count_vars_var_mods subs
  | VarFree _ -> 0
  | Var -> 1

and coll_count cnt var_mod = cnt + count_vars_var_mod var_mod
and count_vars_var_mods var_mods = Array.fold_left coll_count 0 var_mods

let coll_some cnt el = if el = None then cnt else cnt + 1
let count_some ar = Array.fold_left coll_some 0 ar

let sum_of_var_mod = function
  | VarFree sum_mod -> sum_mod
  | VarStrct _ | Var -> failwith "Model_utils.sum_of_var_mod"

let make_strct_var_mods cnstr var_mods =
  if array_forall is_var_free var_mods then
    VarFree (FDStrct (cnstr, Array.map sum_of_var_mod var_mods))
  else VarStrct (cnstr, var_mods)

let subst_fdsums var_mods fdsums =
  let ix_ref = ref 0 in
  let rec cnv = function
    | VarStrct (cnstr, sub_var_mods) ->
        FDStrct (cnstr, Array.map cnv sub_var_mods)
    | VarFree fdsum -> fdsum
    | Var -> let ix = !ix_ref in ix_ref := ix + 1; fdsums.(ix) in
  Array.map cnv var_mods

let subst_var_mods outer_var_mods inner_var_mods =
  let ix_ref = ref 0 in
  let rec cnv = function
    | VarStrct (cnstr, sub_var_mods) ->
        make_strct_var_mods cnstr (Array.map cnv sub_var_mods)
    | VarFree _ as var_mod -> var_mod
    | Var -> let ix = !ix_ref in ix_ref := ix + 1; inner_var_mods.(ix) in
  Array.map cnv outer_var_mods

let make_env init_vars =
  let env = Res.Array.sempty (1.5, 0.5, 4 * Array.length init_vars) in
  let add_var init_var = Res.Array.add_one env init_var in
  Array.iter add_var init_vars;
  env, add_var


(* Apply a model to data *)

let apply_model model fdsums =
  let env, add_var = make_env fdsums in
  let rec loop_mod = function
    | Val fdsums -> fdsums
    | Let (match_mod, var_mods) ->
        let tpl = loop_match_mod match_mod in
        let ix_ref = ref 0 in
        let rec loop = function
          | VarStrct (cnstr, var_mods) ->
              FDStrct (cnstr, Array.map loop var_mods)
          | VarFree fdsum -> fdsum
          | Var -> let ix = !ix_ref in incr ix_ref; tpl.(ix) in
        Array.map loop var_mods
    | MatchMod match_mod -> loop_match_mod match_mod
  and loop_match_mod = function
    | Shave (var_ix, cnstr, sh_mod, def_mod) ->
        (match Res.Array.get env var_ix with
        | FDStrct (var_cnstr, subs) when var_cnstr = cnstr ->
            Array.iter add_var subs;
            loop_mod sh_mod
        | FDAtom var_cnstr when var_cnstr = cnstr -> loop_mod sh_mod
        | FDStrct _ | FDAtom _ -> loop_mod def_mod)
    | Split (var_ix, models) ->
        let cnstr =
          match Res.Array.get env var_ix with
          | FDStrct (cnstr, subs) -> Array.iter add_var subs; cnstr
          | FDAtom cnstr -> cnstr in
        loop_mod models.(cnstr)
    | PSplit (var_ix, maybe_models, def_mod) ->
        match Res.Array.get env var_ix with
        | FDStrct (cnstr, subs) ->
            (match maybe_models.(cnstr) with
            | Some model -> Array.iter add_var subs; loop_mod model
            | None -> loop_mod def_mod)
        | FDAtom cnstr ->
            match maybe_models.(cnstr) with
            | Some model -> loop_mod model
            | None -> loop_mod def_mod in
  loop_mod model


(* Pretty-print model *)

let coll_n_cnstrs n_cnstrs cnstrs = n_cnstrs + Array.length cnstrs

let pp_model ppf t_prefix c_prefix dispec_info cispec_info model =
  let add_t_prefix = (^) t_prefix in
  let map_add_c_prefix = Array.map ((^) c_prefix) in
  let prefix_ispec_info ispec_info =
    {
      ispec_info with
      tp_tbl = Array.map add_t_prefix ispec_info.tp_tbl;
      cnstr_tbl = Array.map map_add_c_prefix ispec_info.cnstr_tbl;
    } in
  let module Spec = struct
    let dispec = dispec_info.ispec
    let cispec = cispec_info.ispec
    let dfspec, init_tps =
      if Array.fold_left coll_n_cnstrs 0 dispec_info.cnstr_tbl = 0 then
        empty_fspec, dummy_init_tps
      else flatten_ispec dispec_info.ispec
    let cfspec, _ = flatten_ispec cispec
  end in
  let module Deco = Deco_impl.Make (Spec) in
  let module DecoSpec = struct
    include Spec
    let dispec_info = prefix_ispec_info dispec_info
    let cispec_info = prefix_ispec_info cispec_info
  end in
  let module Model_pp = Model_pp_impl.Make (DecoSpec) in
  let dmodel, ddpat = Deco.decorate_model model in
  Model_pp.pp_dmodel ppf dmodel ddpat


(* Print human-readable model to stdout *)

let print_model t_prefix v_prefix dispec_info cispec_info model =
  set_margin 10000;
  set_max_indent 1000;
  pp_model std_formatter t_prefix v_prefix dispec_info cispec_info model;
  print_newline ()
