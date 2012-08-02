(*  File: c45_io.ml

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

open Printf
open Pcre
open Utils
open Algdt_types
open Algdt_utils
open Model_data
open Data_io

module StrSet = Set.Make (struct type t = string let compare = compare end)

let hash_size = 541

let flags = [`DOTALL]  (* makes matching faster *)

let comma_rex = regexp ~flags "\\s*(?:,|\\.)\\s*"
let def_rex = regexp ~flags "^(\\w+):\\s*(\\w+(?:\\s*,\\s*\\w+)*)\\s*\\.\\s*$"

let read_c45_spec names =
  let failwith msg = failwith (sprintf "read_c45_spec:%s: %s" names msg) in
  let var_set = ref StrSet.empty in
  let var_lst = ref [] in
  let n_lines_ref = ref 1 in
  let rev_pat_lst = ref [] in
  let target_attr_lst =
    do_open_in names (fun names_ic ->
      let target_line = input_line names_ic in
      foreach_line ~ic:names_ic
        (fun line ->
          incr n_lines_ref;
          match extract ~full_match:false ~rex:def_rex line with
          | [| name; "continuous" |] ->
              eprintf
                "read_c45_spec: \
                 ignoring variable %s in line (data column): %s:%d\n"
                name
                names
                !n_lines_ref;
              flush stderr;
              rev_pat_lst := "[^,]+" :: !rev_pat_lst
          | [| name; rhs |] ->
              if StrSet.mem name !var_set then
                failwith ("variable appears twice: " ^ name);
              var_set := StrSet.add name !var_set;
              let attrs = split ~rex:comma_rex rhs in
              var_lst := (name, Array.of_list attrs) :: !var_lst;
              let pat = ["("; String.concat "|" attrs; "|\\?)"] in
              rev_pat_lst := String.concat "" pat :: !rev_pat_lst
          | _ -> assert false (* impossible *));
        let lix = String.length target_line - 1 in
        let target_line = String.sub target_line 0 lix in
        let target_attr_lst = split ~rex:comma_rex target_line in
        if target_attr_lst = [] then failwith "no target attributes"
        else target_attr_lst) in
  let input_pat = String.concat "," (List.rev !rev_pat_lst) in
  let target_attrs = Array.of_list target_attr_lst in
  let target_pat_lst = ["("; String.concat "|" target_attr_lst; "|\\?)"] in
  let target_pat = String.concat "" target_pat_lst in
  let data_pat =
    if input_pat = "" then target_pat
    else String.concat "," [ input_pat; target_pat ] in
  let tp_names = Array.of_list (List.rev !var_lst) in
  tp_names, target_attrs, data_pat, input_pat

let make_start_prod_el n = TpVal (n + 1)

let target_attr_name = "__target"
let missing = "Missing__"
let value = "Value__"
let __maybe_sample = "__maybe_sample"
let __sample = "__sample"
let maybe_ = "maybe_"

let calc_dispec_info mv (tp_names, _, _, _) =
  let n_vars = Array.length tp_names in
  match mv with
  | Ignore ->
      let n_vars1 = n_vars + 1 in
      let dtp_tbl = Array.make n_vars1 "" in
      let dcnstr_tbl = Array.make n_vars1 [||] in
      let dtp_htbl = Hashtbl.create hash_size in
      let dcnstr_htbl = Hashtbl.create hash_size in
      let dtp = ProdEls (Array.init n_vars make_start_prod_el) in
      let dispec = Array.make n_vars1 (Prod dtp) in
      let acti_defs ix (tp_name, attrs) =
        let tp = ix + 1 in
        dtp_tbl.(tp) <- tp_name;
        dcnstr_tbl.(tp) <- attrs;
        Hashtbl.add dtp_htbl tp_name tp;
        let acti_cnstrs cnstr cnstr_name =
          Hashtbl.add dcnstr_htbl (tp, cnstr_name) cnstr in
        Array.iteri acti_cnstrs attrs;
        dispec.(tp) <- Sums (Array.make (Array.length attrs) IAtom) in
      Array.iteri acti_defs tp_names;
      {
        tp_tbl = dtp_tbl;
        cnstr_tbl = dcnstr_tbl;
        tp_htbl = dtp_htbl;
        cnstr_htbl = dcnstr_htbl;
        ispec = dispec;
      }
  | MProb ->
      let n_vars1 = n_vars + 1 in
      let n_vars2 = n_vars1 + 1 in
      let dtp_tbl = Array.make n_vars2 "" in
      dtp_tbl.(n_vars1) <- __maybe_sample;
      let dcnstr_tbl = Array.make n_vars2 [||] in
      dcnstr_tbl.(n_vars1) <- [| missing; value; |];
      let dtp_htbl = Hashtbl.create hash_size in
      Hashtbl.add dtp_htbl __maybe_sample n_vars1;
      let dcnstr_htbl = Hashtbl.create hash_size in
      Hashtbl.add dcnstr_htbl (n_vars1, missing) 0;
      Hashtbl.add dcnstr_htbl (n_vars1, value) 1;
      let dispec = Array.make n_vars2 (Prod (TpVal n_vars1)) in
      let attr_prod = Array.init n_vars make_start_prod_el in
      dispec.(n_vars1) <- Sums [| IAtom; IStrct (ProdEls attr_prod); |];
      let acti_defs ix (tp_name, attrs) =
        let tp = ix + 1 in
        dtp_tbl.(tp) <- tp_name;
        dcnstr_tbl.(tp) <- attrs;
        Hashtbl.add dtp_htbl tp_name tp;
        let acti_cnstrs cnstr cnstr_name =
          Hashtbl.add dcnstr_htbl (tp, cnstr_name) cnstr in
        Array.iteri acti_cnstrs attrs;
        dispec.(tp) <- Sums (Array.make (Array.length attrs) IAtom) in
      Array.iteri acti_defs tp_names;
      {
        tp_tbl = dtp_tbl;
        cnstr_tbl = dcnstr_tbl;
        tp_htbl = dtp_htbl;
        cnstr_htbl = dcnstr_htbl;
        ispec = dispec;
      }
  | Flat ->
      let n_vars1 = n_vars + 1 in
      let dtp_tbl = Array.make n_vars1 "" in
      dtp_tbl.(n_vars) <- __sample;
      let dcnstr_tbl = Array.make n_vars1 [||] in
      let dtp_htbl = Hashtbl.create hash_size in
      Hashtbl.add dtp_htbl __sample n_vars;
      let dcnstr_htbl = Hashtbl.create hash_size in
      let dtp = ProdEls (Array.init n_vars make_start_prod_el) in
      let dispec = Array.make n_vars1 (Prod dtp) in
      let attr_prod = Array.init n_vars make_start_prod_el in
      dispec.(n_vars) <- Prod (ProdEls attr_prod);
      let acti_defs ix (tp_name, attrs) =
        let tp = ix + 1 in
        dtp_tbl.(tp) <- tp_name;
        dcnstr_tbl.(tp) <- Array.append [| missing |] attrs;
        Hashtbl.add dtp_htbl tp_name tp;
        let acti_cnstrs cnstr cnstr_name =
          Hashtbl.add dcnstr_htbl (tp, cnstr_name) (cnstr + 1) in
        Array.iteri acti_cnstrs attrs;
        Hashtbl.add dcnstr_htbl (tp, missing) 0;
        dispec.(tp) <- Sums (Array.make (Array.length attrs + 1) IAtom) in
      Array.iteri acti_defs tp_names;
      {
        tp_tbl = dtp_tbl;
        cnstr_tbl = dcnstr_tbl;
        tp_htbl = dtp_htbl;
        cnstr_htbl = dcnstr_htbl;
        ispec = dispec;
      }
  | LiftAll ->
      let n_vars2 = n_vars * 2 in
      let n_vars21 = n_vars2 + 1 in
      let n_vars1 = n_vars + 1 in
      let dtp_tbl = Array.make n_vars21 "" in
      let dcnstr_tbl = Array.make n_vars21 [| missing; value; |] in
      dcnstr_tbl.(0) <- [||];
      let dtp_htbl = Hashtbl.create hash_size in
      let dcnstr_htbl = Hashtbl.create hash_size in
      for tp = n_vars1 to n_vars2 do
        Hashtbl.add dcnstr_htbl (tp, missing) 0;
        Hashtbl.add dcnstr_htbl (tp, value) 1;
      done;
      let dtp = ProdEls (Array.init n_vars (fun n -> TpVal (n + n_vars1))) in
      let dispec = Array.make n_vars21 (Prod dtp) in
      let acti_defs ix (tp_name, attrs) =
        let tp = ix + 1 in
        let sample_tp = tp + n_vars in
        dtp_tbl.(tp) <- tp_name;
        let sample_tp_name = maybe_ ^ tp_name in
        dtp_tbl.(sample_tp) <- sample_tp_name;
        dcnstr_tbl.(tp) <- attrs;
        Hashtbl.add dtp_htbl tp_name tp;
        Hashtbl.add dtp_htbl sample_tp_name sample_tp;
        let acti_cnstrs cnstr cnstr_name =
          Hashtbl.add dcnstr_htbl (tp, cnstr_name) cnstr in
        Array.iteri acti_cnstrs attrs;
        dispec.(tp) <- Sums (Array.make (Array.length attrs) IAtom);
        dispec.(sample_tp) <- Sums [| IAtom; IStrct (TpVal tp); |] in
      Array.iteri acti_defs tp_names;
      {
        tp_tbl = dtp_tbl;
        cnstr_tbl = dcnstr_tbl;
        tp_htbl = dtp_htbl;
        cnstr_htbl = dcnstr_htbl;
        ispec = dispec;
      }

let calc_cispec_info (_, target_attrs, _, _) =
  let ctp_htbl = Hashtbl.create 1 in
  Hashtbl.add ctp_htbl target_attr_name 1;
  let ccnstr_htbl = Hashtbl.create 13 in
  let acti cnstr attr = Hashtbl.add ccnstr_htbl (1, attr) cnstr in
  Array.iteri acti target_attrs;
  let cispec =
    [| Prod (TpVal 1); Sums (Array.make (Array.length target_attrs) IAtom) |] in
  {
    tp_tbl = [| ""; target_attr_name |];
    cnstr_tbl = [| [||]; target_attrs |];
    tp_htbl = ctp_htbl;
    cnstr_htbl = ccnstr_htbl;
    ispec = cispec;
  }

let calc_vars n_samples n_vars dcnstr_tbl target_attrs rows =
  let dvars =
    Array.init n_vars (fun ix ->
      let tp = ix + 1 in
      {
        tp = tp;
        samples = Array.make n_samples dummy_fdsum;
        histo = Array.make (Array.length dcnstr_tbl.(tp)) 0;
      }) in
  let csamples = Array.make n_samples dummy_fdsum in
  let chisto = Array.make (Array.length target_attrs) 0 in
  let cvars = [| { tp = 1; samples = csamples; histo = chisto; } |] in
  let n_samples_1 = n_samples - 1 in
  List.iteri
    (fun ix (sample, ccnstr) ->
      let sample_ix = n_samples_1 - ix in
      csamples.(sample_ix) <- FDAtom ccnstr;
      chisto.(ccnstr) <- chisto.(ccnstr) + 1;
      let acti var_ix { samples = dsamples; histo = dhisto } =
        let dcnstr = sample.(var_ix) in
        dhisto.(dcnstr) <- dhisto.(dcnstr) + 1;
        dsamples.(sample_ix) <- FDAtom dcnstr in
      Array.iteri acti dvars)
    rows;
  dvars, cvars

let read_c45_data (tp_names, target_attrs, pat, _ as c45_spec) mv ic =
  let rex = regexp ~flags pat in
  let { cnstr_tbl = dcnstr_tbl; cnstr_htbl = dcnstr_htbl } as dispec_info =
    calc_dispec_info mv c45_spec in
  let { cnstr_htbl = ccnstr_htbl } as cispec_info =
    calc_cispec_info c45_spec in

  let n_lines_ref = ref 1 in
  let n_samples_ref = ref 0 in
  let n_vars = Array.length tp_names in
  let n_vars_1 = n_vars - 1 in

  match mv with
  | Ignore ->
      let rows_ref = ref [] in
      foreach_line ~ic (fun line ->
        try
          let string_row = extract ~full_match:false ~rex line in
          let sample = Array.make n_vars 0 in  (* MV = 0! *)
          for ix = 0 to n_vars_1 do
            sample.(ix) <- Hashtbl.find dcnstr_htbl (ix + 1, string_row.(ix))
          done;
          let target_attr = Hashtbl.find ccnstr_htbl (1, string_row.(n_vars)) in
          rows_ref := (sample, target_attr) :: !rows_ref;
          incr n_lines_ref;
          incr n_samples_ref;
        with Not_found ->
          eprintf "invalid data line (maybe missing value?): %d\n" !n_lines_ref;
          incr n_lines_ref);
      close_in ic;
      flush stderr;
      let dvars, cvars =
        calc_vars !n_samples_ref n_vars dcnstr_tbl target_attrs !rows_ref in
      dispec_info, dvars, cispec_info, cvars

  | MProb ->
      let rows_ref = ref [] in
      foreach_line ~ic (fun line ->
        try
          let string_row = extract ~full_match:false ~rex line in
          let sample = Array.make n_vars dummy_fdsum in
          let target_attr = Hashtbl.find ccnstr_htbl (1, string_row.(n_vars)) in
          (try
            for ix = 0 to n_vars_1 do
              let attr = string_row.(ix) in
              if attr = "?" then raise Exit
              else
                sample.(ix) <- FDAtom (Hashtbl.find dcnstr_htbl (ix + 1, attr))
            done;
            rows_ref := (Some sample, target_attr) :: !rows_ref;
          with Exit -> rows_ref := (None, target_attr) :: !rows_ref);
          incr n_lines_ref;
          incr n_samples_ref;
        with Not_found ->
          eprintf
            "invalid data line (maybe missing value in target?): %d\n"
            !n_lines_ref;
          incr n_lines_ref);
      close_in ic;
      flush stderr;

      let n_samples = !n_samples_ref in
      let n_samples_1 = n_samples - 1 in
      let { samples = dsamples; histo = dhisto } as dvar =
        {
          tp = n_vars + 1;
          samples = Array.make n_samples dummy_fdsum;  (* dummy_fdsum = MV! *)
          histo = [| 0; 0; |]
        } in
      let dvars = [| dvar |] in
      let csamples = Array.make n_samples dummy_fdsum in
      let chisto = Array.make (Array.length target_attrs) 0 in
      let cvars = [| { tp = 1; samples = csamples; histo = chisto; } |] in

      List.iteri
        (fun ix (maybe_sample, ccnstr) ->
          let sample_ix = n_samples_1 - ix in
          csamples.(sample_ix) <- FDAtom ccnstr;
          chisto.(ccnstr) <- chisto.(ccnstr) + 1;
          match maybe_sample with
          | None -> dhisto.(0) <- dhisto.(0) + 1
          | Some sample ->
              dhisto.(1) <- dhisto.(1) + 1;
              dsamples.(sample_ix) <- FDStrct (1, sample))
        !rows_ref;

      dispec_info, dvars, cispec_info, cvars

  | Flat ->
      let rows_ref = ref [] in
      foreach_line ~ic (fun line ->
        try
          let string_row = extract ~full_match:false ~rex line in
          let sample = Array.make n_vars 0 in  (* MV = 0! *)
          for ix = 0 to n_vars_1 do
            let attr = string_row.(ix) in
            if attr <> "?" then
              sample.(ix) <- Hashtbl.find dcnstr_htbl (ix + 1, attr)
          done;
          let target_attr = Hashtbl.find ccnstr_htbl (1, string_row.(n_vars)) in
          rows_ref := (sample, target_attr) :: !rows_ref;
          incr n_lines_ref;
          incr n_samples_ref;
        with Not_found ->
          eprintf
            "invalid data line (maybe missing value in target?): %d\n"
            !n_lines_ref;
          incr n_lines_ref);
      close_in ic;
      flush stderr;
      let dvars, cvars =
        calc_vars !n_samples_ref n_vars dcnstr_tbl target_attrs !rows_ref in
      dispec_info, dvars, cispec_info, cvars

  | LiftAll ->
      let rows_ref = ref [] in
      foreach_line ~ic (fun line ->
        try
          let string_row = extract ~full_match:false ~rex line in
          let sample =
            Array.make n_vars dummy_fdsum in  (* dummy_fdsum = MV! *)
          let target_attr = Hashtbl.find ccnstr_htbl (1, string_row.(n_vars)) in
          for ix = 0 to n_vars_1 do
            let attr = string_row.(ix) in
            if attr <> "?" then
              let data = FDAtom (Hashtbl.find dcnstr_htbl (ix + 1, attr)) in
              sample.(ix) <- FDStrct (1, [| data |])
          done;
          rows_ref := (sample, target_attr) :: !rows_ref;
          incr n_lines_ref;
          incr n_samples_ref;
        with Not_found ->
          eprintf
            "invalid data line (maybe missing value in target?): %d\n"
            !n_lines_ref;
          incr n_lines_ref);
      close_in ic;
      flush stderr;

      let n_samples = !n_samples_ref in
      let n_samples_1 = n_samples - 1 in
      let dvars =
        let n_vars1 = n_vars + 1 in
        Array.init n_vars (fun ix ->
          {
            tp = ix + n_vars1;
            samples = Array.make n_samples dummy_fdsum;
            histo = [| 0; 0 |];
          }) in
      let csamples = Array.make n_samples dummy_fdsum in
      let chisto = Array.make (Array.length target_attrs) 0 in
      let cvars = [| { tp = 1; samples = csamples; histo = chisto; } |] in

      List.iteri
        (fun ix (sample, ccnstr) ->
          let sample_ix = n_samples_1 - ix in
          csamples.(sample_ix) <- FDAtom ccnstr;
          chisto.(ccnstr) <- chisto.(ccnstr) + 1;
          let acti var_ix { samples = dsamples; histo = dhisto } =
            let fdsum = sample.(var_ix) in
            let dcnstr = fdsum_cnstr fdsum in
            dhisto.(dcnstr) <- dhisto.(dcnstr) + 1;
            dsamples.(sample_ix) <- fdsum in
          Array.iteri acti dvars)
        !rows_ref;

      dispec_info, dvars, cispec_info, cvars


(* Read C4.5-rhs-data *)

let target_rex = regexp ~flags:[`DOTALL] "(?:^|.*,)(\\w+)\\s*$"

let read_rhs ic cnstr_htbl =
  try
    let strs = extract ~full_match:false ~rex:target_rex (input_line ic) in
    Some [| FDAtom (Hashtbl.find cnstr_htbl (1, strs.(0))) |]
  with End_of_file -> None
