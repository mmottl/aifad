(*  File: rand_gen.ml

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

(* $Id: rand_gen.ml,v 1.10 2006/01/17 00:23:38 mottl Exp $ *)

open Algdt_types
open Algdt_utils
open Model_data

(* Random data generation *)

let rand_deco_data_gen ispec =
  let rec loop tp =
    match ispec.(tp) with
    | Prod prod_el -> cnv_prod_el prod_el
    | Sums sums ->
        let cnstr = Random.int (Array.length sums) in
        match sums.(cnstr) with
        | IAtom -> TpVal (DAtom (tp, cnstr))
        | IStrct prod_el -> TpVal (DStrct ((tp, cnstr), cnv_prod_el prod_el))
  and cnv_prod_el = function
    | TpVal tp -> loop tp
    | ProdEls prod_els -> ProdEls (Array.map cnv_prod_el prod_els) in
  loop init_tp

let write_rand_samples ppf dispec_info cispec_info n with_target =
  let { ispec = dispec; cnstr_tbl = dcnstr_tbl; _ } = dispec_info in
  let { ispec = cispec; cnstr_tbl = ccnstr_tbl; _ } = cispec_info in
  let module IData2PP =
    Algdt_pp.Make_IData2
      (struct let cnstr_tbl = dcnstr_tbl end)
      (struct let cnstr_tbl = ccnstr_tbl end) in
  if with_target then
    for i = 1 to n do
      IData2PP.pp_sample
        ppf (rand_deco_data_gen dispec, rand_deco_data_gen cispec);
      Format.pp_force_newline ppf ()
    done
  else
    for i = 1 to n do
      IData2PP.D.pp_data ppf (rand_deco_data_gen dispec);
      Format.pp_force_newline ppf ()
    done

let write_rand_c45_data_ignore oc dcnstr_tbl =
  let dcnstrs1 = dcnstr_tbl.(1) in
  output_string oc dcnstrs1.(Random.int (Array.length dcnstrs1));
  for tp = 2 to Array.length dcnstr_tbl - 1 do
    output_char oc ',';
    let dcnstrs = dcnstr_tbl.(tp) in
    output_string oc dcnstrs.(Random.int (Array.length dcnstrs))
  done

let write_rand_c45_data_mprob p oc dcnstr_tbl =
  if Random.float 1.0 <= p then (
    output_string oc "?";
    for tp = 2 to Array.length dcnstr_tbl - 2 do output_string oc ",?"; done)
  else (
    let dcnstrs1 = dcnstr_tbl.(1) in
    output_string oc dcnstrs1.(Random.int (Array.length dcnstrs1));
    for tp = 2 to Array.length dcnstr_tbl - 2 do
      output_char oc ',';
      let dcnstrs = dcnstr_tbl.(tp) in
      output_string oc dcnstrs.(Random.int (Array.length dcnstrs))
    done)

let write_rand_c45_data_flat p oc dcnstr_tbl =
  if Random.float 1.0 <= p then output_string oc "?"
  else (
    let dcnstrs1 = dcnstr_tbl.(1) in
    output_string oc dcnstrs1.(Random.int (Array.length dcnstrs1 - 1) + 1);
    for tp = 2 to Array.length dcnstr_tbl - 1 do
      if Random.float 1.0 <= p then output_string oc ",?"
      else (
        output_char oc ',';
        let dcnstrs = dcnstr_tbl.(tp) in
        output_string oc dcnstrs.(Random.int (Array.length dcnstrs - 1) + 1));
    done)

let write_rand_c45_data_lift_all p oc dcnstr_tbl =
  if Random.float 1.0 <= p then output_string oc "?"
  else (
    let dcnstrs1 = dcnstr_tbl.(1) in
    output_string oc dcnstrs1.(Random.int (Array.length dcnstrs1)));
  for tp = 2 to (Array.length dcnstr_tbl - 1) / 2 do
    if Random.float 1.0 <= p then output_string oc ",?"
    else (
      output_char oc ',';
      let dcnstrs = dcnstr_tbl.(tp) in
      output_string oc dcnstrs.(Random.int (Array.length dcnstrs)))
  done

let write_rand_c45_target oc ccnstr_tbl =
  let ccnstr_tbl1 = ccnstr_tbl.(1) in
  output_string oc ccnstr_tbl1.(Random.int (Array.length ccnstr_tbl1))

let write_rand_c45_samples oc dispec_info cispec_info n with_target mv p =
  let dcnstr_tbl = dispec_info.cnstr_tbl in
  let ccnstr_tbl = cispec_info.cnstr_tbl in
  if Array.length dcnstr_tbl <= 1 then
    if with_target then
      for i = 1 to n do
        write_rand_c45_target oc ccnstr_tbl;
        output_char oc '\n'
      done
    else failwith "write_rand_c45_samples: no input variables and no target"
  else
    match with_target, mv with
    | true, Ignore ->
        for i = 1 to n do
          write_rand_c45_data_ignore oc dcnstr_tbl;
          output_char oc ',';
          write_rand_c45_target oc ccnstr_tbl;
          output_char oc '\n'
        done
    | false, Ignore ->
        for i = 1 to n do
          write_rand_c45_data_ignore oc dcnstr_tbl;
          output_char oc '\n'
        done
    | true, MProb ->
        for i = 1 to n do
          write_rand_c45_data_mprob p oc dcnstr_tbl;
          output_char oc ',';
          write_rand_c45_target oc ccnstr_tbl;
          output_char oc '\n'
        done
    | false, MProb ->
        for i = 1 to n do
          write_rand_c45_data_mprob p oc dcnstr_tbl;
          output_char oc '\n';
        done
    | true, Flat ->
        for i = 1 to n do
          write_rand_c45_data_flat p oc dcnstr_tbl;
          output_char oc ',';
          write_rand_c45_target oc ccnstr_tbl;
          output_char oc '\n'
        done
    | false, Flat ->
        for i = 1 to n do
          write_rand_c45_data_flat p oc dcnstr_tbl;
          output_char oc '\n';
        done
    | true, LiftAll ->
        for i = 1 to n do
          write_rand_c45_data_lift_all p oc dcnstr_tbl;
          output_char oc ',';
          write_rand_c45_target oc ccnstr_tbl;
          output_char oc '\n'
        done
    | false, LiftAll ->
        for i = 1 to n do
          write_rand_c45_data_lift_all p oc dcnstr_tbl;
          output_char oc '\n';
        done
