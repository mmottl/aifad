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
open Model_types
open Entropy_utils

let calc_tp_bits tp = log2 (float (Array.length tp))
let calc_fspec_bits fspec = Array.map calc_tp_bits fspec

(* Information contents of fdsums *)

let rec calc_fdsum_bits fspec_bits fspec tp = function
  | FDAtom _ -> fspec_bits.(tp)
  | FDStrct (cnstr, subs) ->
      fspec_bits.(tp) +.
        calc_fdsums_bits fspec_bits fspec fspec.(tp).(cnstr) subs

and calc_fdsums_bits fspec_bits fspec tps fdsums =
  let colli ix bits fdsum =
    bits +. calc_fdsum_bits fspec_bits fspec tps.(ix) fdsum in
  array_fold_lefti colli 0.0 fdsums


(* Information contents of var_mods *)

let rec calc_var_mod_bits fspec_bits fspec tp = function
  | VarStrct (cnstr, subs) ->
      fspec_bits.(tp) +.
        calc_var_mods_bits fspec_bits fspec fspec.(tp).(cnstr) subs
  | VarFree fdsum -> calc_fdsum_bits fspec_bits fspec tp fdsum
  | Var -> 0.0

and calc_var_mods_bits fspec_bits fspec tps var_mods =
  let colli ix bits var_mod =
    bits +. calc_var_mod_bits fspec_bits fspec tps.(ix) var_mod in
  array_fold_lefti colli 0.0 var_mods


(* Common bits between fdsums *)

let rec calc_cmn_fdsum_bits fspec_bits fspec tp fdsum1 fdsum2 =
  match fdsum1, fdsum2 with
  | FDStrct _, FDAtom _ | FDAtom _, FDStrct _ -> 0.0
  | FDAtom cnstr1, FDAtom cnstr2
  | FDStrct (cnstr1, _), FDStrct (cnstr2, _) when cnstr1 <> cnstr2 -> 0.0
  | FDAtom _, FDAtom _ -> fspec_bits.(tp)
  | FDStrct (cnstr, subs1), FDStrct (_, subs2) ->
      fspec_bits.(tp) +.
        calc_cmn_fdsums_bits fspec_bits fspec fspec.(tp).(cnstr) subs1 subs2

and calc_cmn_fdsums_bits fspec_bits fspec tps fdsums1 fdsums2 =
  let colli ix bits fdsum1 =
    bits +. calc_cmn_fdsum_bits fspec_bits fspec tps.(ix) fdsum1 fdsums2.(ix) in
  array_fold_lefti colli 0.0 fdsums1
