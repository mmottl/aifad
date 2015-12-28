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
open Entropy_utils

type split_info = histo array

let var_entropy _ var = calc_entropy var.histo (Array.length var.samples)

let vars_entropy _ vars =
  if array_is_empty vars then invalid_arg "var_entropies: no variables";
  let n_samples = Array.length vars.(0).samples in
  Array.fold_left (fun e var -> e +. calc_entropy var.histo n_samples) 0.0 vars

let calc_split_infos _ dvar _ cvars = calc_cntg_tbls dvar cvars

let sum_histo histo = Array.fold_left (+) 0 histo
let coll_entropy e histo = e +. calc_entropy histo (sum_histo histo)
let calc_split_info_entropy _ histos = Array.fold_left coll_entropy 0.0 histos
