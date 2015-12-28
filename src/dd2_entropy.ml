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
open Entropy_utils

type split_info = var array

let rec vars_entropy fspec vars =
  let var = vars.(0) in
  let n_samples = Array.length var.samples in
  if n_samples = 0 then 0.0
  else
    let ShInfo (pos_infos, _), n_sh_vars = calc_shave_info fspec vars in
    if n_sh_vars = 0 then 0.0
    else (
      let sh_vars = vars_of_pos_infos n_sh_vars vars pos_infos in
      let split_vars = split_with_sub_vars 0 fspec sh_vars 0 in
      let f_samples = float n_samples in
      let histo = sh_vars.(0).histo in
      let colli cnstr e new_vars =
        if array_is_empty new_vars then e
        else
          let f_sub_samples = float histo.(cnstr) in
          e +. f_sub_samples /. f_samples *. vars_entropy fspec new_vars in
      let e = calc_entropy histo n_samples in
      array_fold_lefti colli e split_vars)

let var_entropy fspec var = vars_entropy fspec [| var |]

let calc_split_infos _ dvar cfspec cvars = split_vars dvar cfspec cvars
let calc_split_info_entropy = vars_entropy
