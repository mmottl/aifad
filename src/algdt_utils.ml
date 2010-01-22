(*  File: algdt_utils.ml

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

(* $Id: algdt_utils.ml,v 1.48 2006/01/17 00:23:37 mottl Exp $ *)

open Algdt_types
open Utils

(* Miscellaneous general definitions *)

let init_tp = 0
let init_prod_el = TpVal init_tp
let init_prod_els = [| init_prod_el |]
let dummy_init_tps = [| init_tp |]

let dummy_fdsum = FDAtom 0
let dummy_fdsums = [| dummy_fdsum |]
let dummy_tp = 0

let dummy_var =
  {
    samples = [||];
    tp = dummy_tp;
    histo = [||];
  }

let empty_fspec = [|[|[||]|]|]

let get_n_fdsums ispec fspec =
  match ispec.(init_tp) with
  | Prod _ -> Array.length fspec.(init_tp).(0)
  | Sums _ -> 1

let get_init_tps ispec fspec =
  match ispec.(init_tp) with
  | Prod _ -> fspec.(init_tp).(0)
  | Sums _ -> dummy_init_tps

(* XXX implementation dependent! *)
(* let fdsum_cnstr = function FDAtom cnstr | FDStrct (cnstr, _) -> cnstr *)
external fdsum_cnstr : fdsum -> cnstr = "%field0"

let histo_is_redundant histo n_samples =
  let rec loop cnstr =
    let freq = histo.(cnstr) in
    freq = n_samples || freq = 0 && cnstr > 0 && loop (cnstr - 1) in
  loop (Array.length histo - 1)

let many_fdsums_of_vars vars =
  let var_samples = Array.map (fun var -> var.samples) vars in
  let n_vars = Array.length var_samples in
  if n_vars = 0 then [||]
  else
    let var_samples0 = var_samples.(0) in
    let n_vars_1 = n_vars - 1 in
    let acti sample_ix fdsum =
      let fdsums = Array.make n_vars fdsum in
      for var_ix = 1 to n_vars_1 do
        fdsums.(var_ix) <- var_samples.(var_ix).(sample_ix)
      done;
      fdsums in
    Array.mapi acti var_samples0


(* Conversion: undecorated -> decorated *)

let make_deco_dsum_cnv fspec ispec =
  let rec cnv_fdsum tp = function
    | FDAtom cnstr -> DAtom (tp, cnstr)
    | FDStrct (cnstr, fdsums) -> cnv_fdstrct tp cnstr fdsums
  and cnv_fdstrct tp cnstr fdsums =
    let dprod_els =
      let len = Array.length fdsums in
      if len = 1 then
        let sub_tp = fspec.(tp).(cnstr).(0) in
        match fdsums.(0) with
        | FDAtom sub_cnstr -> TpVal (DAtom (sub_tp, sub_cnstr))
        | FDStrct (sub_cnstr, sub_fdsums) ->
            TpVal (cnv_fdstrct sub_tp sub_cnstr sub_fdsums)
      else
        match ispec.(tp) with
        | Prod _ -> failwith "cnv_fdstrct: expected Sums"
        | Sums sums ->
            match sums.(cnstr) with
            | IAtom -> failwith "cnv_fdstrct: expected IStrct"
            | IStrct (ProdEls prod_els) ->
                ProdEls (fst (cnv_prod_els prod_els 0 fdsums))
            | IStrct (TpVal sub_tp) ->
                match ispec.(sub_tp) with
                | Prod prod_el -> fst (cnv_prod_el_loop 0 fdsums prod_el)
                | Sums _ -> failwith "cnv_fdstrct: expected Prod" in
    DStrct ((tp, cnstr), dprod_els)
  and cnv_prod_els prod_els ix fdsums =
    let dprod_el, new_ix = cnv_prod_el_loop ix fdsums prod_els.(0) in
    let dprod_els = Array.make (Array.length prod_els) dprod_el in
    let colli i ix prod_el =
      let dprod_el, new_ix = cnv_prod_el_loop ix fdsums prod_el in
      dprod_els.(i) <- dprod_el;
      new_ix in
    dprod_els, array_fold_left1i colli new_ix prod_els
  and cnv_prod_el_loop ix fdsums = function
    | ProdEls prod_els ->
        let dprod_els, new_ix = cnv_prod_els prod_els ix fdsums in
        ProdEls dprod_els, new_ix
    | TpVal tp ->
        match ispec.(tp) with
        | Prod sub_prod_el -> cnv_prod_el_loop ix fdsums sub_prod_el
        | Sums _ -> TpVal (cnv_fdsum tp fdsums.(ix)), ix + 1 in
  cnv_fdsum, fun prod_els fdsums -> fst (cnv_prod_els prod_els 0 fdsums)


(* Calculation of histograms and contingency tables *)

let make_histo var = Array.make (Array.length var.histo) 0

let calc_cntg_tbls { histo = dhisto; samples = dsamples } cvars =
  let make_cntg_tbl _ = Array.map make_histo cvars in
  let cntg_tbls = Array.init (Array.length dhisto) make_cntg_tbl in
  let fill_cntg_tbls sample_ix dsample =
    let dcnstr = fdsum_cnstr dsample in
    let cntg_tbl = cntg_tbls.(dcnstr) in
    let cnt_var ix var =
      let ccnstr = fdsum_cnstr var.samples.(sample_ix) in
      let ccnstr_cnts = cntg_tbl.(ix) in
      ccnstr_cnts.(ccnstr) <- ccnstr_cnts.(ccnstr) + 1 in
    Array.iteri cnt_var cvars in
  Array.iteri fill_cntg_tbls dsamples;
  cntg_tbls

let sample_histo fspec tp samples =
  let histo = Array.make (Array.length fspec.(tp)) 0 in
  let act sample =
    let cnstr = fdsum_cnstr sample in
    histo.(cnstr) <- histo.(cnstr) + 1 in
  Array.iter act samples;
  histo

let make_var fspec tp samples =
  { samples = samples; tp = tp; histo = sample_histo fspec tp samples }

let make_vars fspec tps many_samples =
  Array.mapi (fun i -> make_var fspec tps.(i)) many_samples


(* Split variables on some given variable without adding subvariables. *)
let split_vars { histo = histo; samples = samples } fspec vars =
  let cnv_histo freq =
    let cnv_vars { tp = tp } =
      {
        samples = Array.make freq dummy_fdsum;
        tp = tp;
        histo = Array.make (Array.length fspec.(tp)) 0;
      } in
    Array.map cnv_vars vars in
  let many_vars = Array.map cnv_histo histo in
  let dst_ixs = Array.make (Array.length histo) 0 in
  let n_vars_1 = Array.length vars - 1 in
  let acti_samples src_ix src_sample =
    let src_cnstr = fdsum_cnstr src_sample in
    let dst_ix = dst_ixs.(src_cnstr) in
    dst_ixs.(src_cnstr) <- dst_ix + 1;
    let dst_vars = many_vars.(src_cnstr) in
    for var_ix = 0 to n_vars_1 do
      let dst_var = dst_vars.(var_ix) in
      let dst_sample = vars.(var_ix).samples.(src_ix) in
      dst_var.samples.(dst_ix) <- dst_sample;
      let dst_histo = dst_var.histo in
      let dst_cnstr = fdsum_cnstr dst_sample in
      dst_histo.(dst_cnstr) <- dst_histo.(dst_cnstr) + 1
    done in
  Array.iteri acti_samples samples;
  many_vars


(* Split variables on some variable and add possible subvariables *)
(* (left, X (x, y), right) -> (left, x, y, right) *)
(* Ignore branches with less than or equal [min_freq] samples *)

let split_with_sub_vars min_freq fspec vars var_ix =
  let n_vars_1 = Array.length vars - 1 in
  let { samples = samples; tp = tp; histo = histo } = vars.(var_ix) in
  let var_ix_1 = var_ix - 1 in
  let var_ix1 = var_ix + 1 in
  let subs_tps = fspec.(tp) in

  let n_splits = Array.length histo in
  let split_vars = Array.make n_splits [||] in

  for split_cnstr = 0 to n_splits - 1 do
    let freq = histo.(split_cnstr) in
    let sub_tps = subs_tps.(split_cnstr) in
    let n_subs = Array.length sub_tps in
    let n_new_vars = n_vars_1 + n_subs in
    let new_vars = Array.make n_new_vars dummy_var in
    for i = 0 to var_ix_1 do
      let cur_var = vars.(i) in
      new_vars.(i) <-
        {
          samples = Array.make freq dummy_fdsum;
          tp = cur_var.tp;
          histo = Array.make (Array.length cur_var.histo) 0;
        }
    done;
    let n_subs_1 = n_subs - 1 in
    for i = 0 to n_subs_1 do
      let tp = sub_tps.(i) in
      new_vars.(var_ix + i) <-
        {
          samples = Array.make freq dummy_fdsum;
          tp = tp;
          histo = Array.make (Array.length fspec.(tp)) 0;
        }
    done;
    for i = var_ix1 to n_vars_1 do
      let cur_var = vars.(i) in
      new_vars.(i + n_subs_1) <-
        {
          samples = Array.make freq dummy_fdsum;
          tp = cur_var.tp;
          histo = Array.make (Array.length cur_var.histo) 0;
        }
    done;
    split_vars.(split_cnstr) <- new_vars
  done;

  let split_ixs = Array.make n_splits 0 in

  for sample_ix = 0 to Array.length samples - 1 do
    match samples.(sample_ix) with
    | FDAtom split_cnstr when histo.(split_cnstr) > min_freq ->
        let new_sample_ix = split_ixs.(split_cnstr) in
        split_ixs.(split_cnstr) <- new_sample_ix + 1;
        let new_vars = split_vars.(split_cnstr) in
        for i = 0 to var_ix_1 do
          let { histo = new_histo } as new_var = new_vars.(i) in
          let sample = vars.(i).samples.(sample_ix) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done;
        for i = var_ix1 to n_vars_1 do
          let { histo = new_histo } as new_var = new_vars.(i - 1) in
          let sample = vars.(i).samples.(sample_ix) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done
    | FDStrct (split_cnstr, subs) when histo.(split_cnstr) > min_freq ->
        let new_sample_ix = split_ixs.(split_cnstr) in
        split_ixs.(split_cnstr) <- new_sample_ix + 1;
        let new_vars = split_vars.(split_cnstr) in
        let n_subs = Array.length subs in
        for i = 0 to var_ix_1 do
          let { histo = new_histo } as new_var = new_vars.(i) in
          let sample = vars.(i).samples.(sample_ix) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done;
        let n_subs_1 = n_subs - 1 in
        for i = 0 to n_subs_1 do
          let { histo = new_histo } as new_var = new_vars.(var_ix + i) in
          let sample = subs.(i) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done;
        for i = var_ix1 to n_vars_1 do
          let { histo = new_histo } as new_var = new_vars.(i + n_subs_1) in
          let sample = vars.(i).samples.(sample_ix) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done
    | FDAtom _ | FDStrct _ -> ()
  done;
  split_vars


(* Split variables on some variable and a constructor into two sets and
   add possible subvariables to the constructor case *)
(* (left, X (x, y), right) -> (left, x, y, right) *)

let split_cnstr_with_sub_vars fspec vars var_ix cnstr =
  let n_vars_1 = Array.length vars - 1 in
  let { samples = samples; tp = tp; histo = histo } = vars.(var_ix) in
  let n_samples = Array.length samples in
  let var_ix1 = var_ix + 1 in
  let var_ix_1 = var_ix - 1 in

  let freq = histo.(cnstr) in
  let other_freq = n_samples - freq in
  let sub_tps = fspec.(tp).(cnstr) in
  let n_subs = Array.length sub_tps in
  let n_new_vars = n_vars_1 + n_subs in
  let new_vars = Array.make n_new_vars dummy_var in
  let other_vars = Array.make n_new_vars dummy_var in

  for i = 0 to var_ix_1 do
    let { tp = cur_tp; histo = cur_histo } = vars.(i) in
    let n_cur_histo = Array.length cur_histo in
    new_vars.(i) <-
      {
        samples = Array.make freq dummy_fdsum;
        tp = cur_tp;
        histo = Array.make n_cur_histo 0;
      };
    other_vars.(i) <-
      {
        samples = Array.make other_freq dummy_fdsum;
        tp = cur_tp;
        histo = Array.make n_cur_histo 0;
      }
  done;
  let n_subs_1 = n_subs - 1 in
  for i = var_ix1 to n_vars_1 do
    let { tp = cur_tp; histo = cur_histo } = vars.(i) in
    let n_cur_histo = Array.length cur_histo in
    new_vars.(i + n_subs_1) <-
      {
        samples = Array.make freq dummy_fdsum;
        tp = cur_tp;
        histo = Array.make n_cur_histo 0;
      };
    other_vars.(i - 1) <-
      {
        samples = Array.make other_freq dummy_fdsum;
        tp = cur_tp;
        histo = Array.make n_cur_histo 0;
      }
  done;

  let new_sample_ix_ref = ref 0 in
  let other_sample_ix_ref = ref 0 in

  if n_subs > 0 then (
    for i = 0 to n_subs_1 do
      let tp = sub_tps.(i) in
      new_vars.(var_ix + i) <-
        {
          samples = Array.make freq dummy_fdsum;
          tp = tp;
          histo = Array.make (Array.length fspec.(tp)) 0;
        }
    done;
    for sample_ix = 0 to n_samples - 1 do
      match samples.(sample_ix) with
      | FDStrct (split_cnstr, subs) when split_cnstr = cnstr ->
          let new_sample_ix = !new_sample_ix_ref in
          new_sample_ix_ref := new_sample_ix + 1;
          for i = 0 to var_ix_1 do
            let { histo = new_histo } as new_var = new_vars.(i) in
            let sample = vars.(i).samples.(sample_ix) in
            new_var.samples.(new_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            new_histo.(cnstr) <- new_histo.(cnstr) + 1
          done;
          for i = 0 to n_subs_1 do
            let { histo = new_histo } as new_var = new_vars.(var_ix + i) in
            let sample = subs.(i) in
            new_var.samples.(new_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            new_histo.(cnstr) <- new_histo.(cnstr) + 1
          done;
          for i = var_ix1 to n_vars_1 do
            let { histo = new_histo } as new_var = new_vars.(i + n_subs_1) in
            let sample = vars.(i).samples.(sample_ix) in
            new_var.samples.(new_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            new_histo.(cnstr) <- new_histo.(cnstr) + 1
          done
      | FDAtom _ | FDStrct _ ->
          let other_sample_ix = !other_sample_ix_ref in
          other_sample_ix_ref := other_sample_ix + 1;
          for i = 0 to var_ix_1 do
            let { histo = other_histo } as other_var = other_vars.(i) in
            let sample = vars.(i).samples.(sample_ix) in
            other_var.samples.(other_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            other_histo.(cnstr) <- other_histo.(cnstr) + 1
          done;
          for i = var_ix1 to n_vars_1 do
            let { histo = other_histo } as other_var = other_vars.(i - 1) in
            let sample = vars.(i).samples.(sample_ix) in
            other_var.samples.(other_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            other_histo.(cnstr) <- other_histo.(cnstr) + 1
          done
    done)
  else
    for sample_ix = 0 to n_samples - 1 do
      match samples.(sample_ix) with
      | FDAtom split_cnstr when split_cnstr = cnstr ->
          let new_sample_ix = !new_sample_ix_ref in
          new_sample_ix_ref := new_sample_ix + 1;
          for i = 0 to var_ix_1 do
            let { histo = new_histo } as new_var = new_vars.(i) in
            let sample = vars.(i).samples.(sample_ix) in
            new_var.samples.(new_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            new_histo.(cnstr) <- new_histo.(cnstr) + 1
          done;
          for i = var_ix1 to n_vars_1 do
            let { histo = new_histo } as new_var = new_vars.(i - 1) in
            let sample = vars.(i).samples.(sample_ix) in
            new_var.samples.(new_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            new_histo.(cnstr) <- new_histo.(cnstr) + 1
          done
      | FDAtom _ | FDStrct _ ->
          let other_sample_ix = !other_sample_ix_ref in
          other_sample_ix_ref := other_sample_ix + 1;
          for i = 0 to var_ix_1 do
            let { histo = other_histo } as other_var = other_vars.(i) in
            let sample = vars.(i).samples.(sample_ix) in
            other_var.samples.(other_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            other_histo.(cnstr) <- other_histo.(cnstr) + 1
          done;
          for i = var_ix1 to n_vars_1 do
            let { histo = other_histo } as other_var = other_vars.(i - 1) in
            let sample = vars.(i).samples.(sample_ix) in
            other_var.samples.(other_sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            other_histo.(cnstr) <- other_histo.(cnstr) + 1
          done
    done;

  new_vars, other_vars


(* Shave variable with a given constructor and add possible subvariables *)
(* (left, X (x, y), right) -> (left, x, y, right) *)

let shave_with_sub_vars fspec vars var_ix cnstr =
  let n_vars_1 = Array.length vars - 1 in
  let { samples = samples; tp = tp; histo = histo } = vars.(var_ix) in
  let freq = histo.(cnstr) in
  let sub_tps = fspec.(tp).(cnstr) in
  let n_subs = Array.length sub_tps in
  let n_new_vars = n_vars_1 + n_subs in
  let new_vars = Array.make n_new_vars dummy_var in
  Array.blit vars 0 new_vars 0 var_ix;
  Array.blit vars (var_ix + 1) new_vars (var_ix + n_subs) (n_vars_1 - var_ix);
  if n_subs > 0 then (
    let n_subs_1 = n_subs - 1 in
    for i = 0 to n_subs_1 do
      let tp = sub_tps.(i) in
      new_vars.(var_ix + i) <-
        {
          samples = Array.make freq dummy_fdsum;
          tp = tp;
          histo = Array.make (Array.length fspec.(tp)) 0;
        }
    done;
    for sample_ix = 0 to Array.length samples - 1 do
      match samples.(sample_ix) with
      | FDStrct (split_cnstr, subs) when split_cnstr = cnstr ->
          for i = 0 to n_subs_1 do
            let { histo = new_histo } as new_var = new_vars.(var_ix + i) in
            let sample = subs.(i) in
            new_var.samples.(sample_ix) <- sample;
            let cnstr = fdsum_cnstr sample in
            new_histo.(cnstr) <- new_histo.(cnstr) + 1
          done
      | FDAtom _ | FDStrct _ ->
          failwith "shave_with_sub_vars: constructor not unique"
    done);
  new_vars


(* Shave variable without adding subvariables *)

let atomic_shave_vars vars var_ix =
  let n_vars_1 = Array.length vars - 1 in
  let new_vars = Array.make n_vars_1 dummy_var in
  Array.blit vars 0 new_vars 0 var_ix;
  Array.blit vars (var_ix + 1) new_vars var_ix (n_vars_1 - var_ix);
  new_vars


(* Split variable on maybe constructors, adding subvariables as required *)

let maybe_split_with_sub_vars fspec vars var_ix maybes =
  let n_vars_1 = Array.length vars - 1 in
  let { samples = samples; tp = tp; histo = histo } = vars.(var_ix) in
  let var_ix_1 = var_ix - 1 in
  let var_ix1 = var_ix + 1 in
  let subs_tps = fspec.(tp) in

  let n_splits = Array.length histo in
  let split_vars = Array.make n_splits [||] in
  let freq_none_ref = ref 0 in

  for split_cnstr = 0 to n_splits - 1 do
    let freq = histo.(split_cnstr) in
    match maybes.(split_cnstr) with
    | None -> freq_none_ref := !freq_none_ref + freq
    | Some _ ->
        let sub_tps = subs_tps.(split_cnstr) in
        let n_subs = Array.length sub_tps in
        let n_new_vars = n_vars_1 + n_subs in
        let new_vars = Array.make n_new_vars dummy_var in
        for i = 0 to var_ix_1 do
          let cur_var = vars.(i) in
          new_vars.(i) <-
            {
              samples = Array.make freq dummy_fdsum;
              tp = cur_var.tp;
              histo = Array.make (Array.length cur_var.histo) 0;
            }
        done;
        let n_subs_1 = n_subs - 1 in
        for i = 0 to n_subs_1 do
          let tp = sub_tps.(i) in
          new_vars.(var_ix + i) <-
            {
              samples = Array.make freq dummy_fdsum;
              tp = tp;
              histo = Array.make (Array.length fspec.(tp)) 0;
            }
        done;
        for i = var_ix1 to n_vars_1 do
          let cur_var = vars.(i) in
          new_vars.(i + n_subs_1) <-
            {
              samples = Array.make freq dummy_fdsum;
              tp = cur_var.tp;
              histo = Array.make (Array.length cur_var.histo) 0;
            }
        done;
        split_vars.(split_cnstr) <- new_vars
  done;

  let freq_none = !freq_none_ref in
  let none_vars = Array.make n_vars_1 dummy_var in
  for i = 0 to var_ix_1 do
    let cur_var = vars.(i) in
    none_vars.(i) <-
      {
        samples = Array.make freq_none dummy_fdsum;
        tp = cur_var.tp;
        histo = Array.make (Array.length cur_var.histo) 0;
      }
  done;
  for i = var_ix1 to n_vars_1 do
    let cur_var = vars.(i) in
    none_vars.(i - 1) <-
      {
        samples = Array.make freq_none dummy_fdsum;
        tp = cur_var.tp;
        histo = Array.make (Array.length cur_var.histo) 0;
      }
  done;

  let split_ixs = Array.make n_splits 0 in
  let none_ix_ref = ref 0 in

  for sample_ix = 0 to Array.length samples - 1 do
    match samples.(sample_ix) with
    | FDAtom split_cnstr
    | FDStrct (split_cnstr, _) when maybes.(split_cnstr) = None ->
        let none_sample_ix = !none_ix_ref in
        none_ix_ref := none_sample_ix + 1;
        for i = 0 to var_ix_1 do
          let { histo = none_histo } as none_var = none_vars.(i) in
          let sample = vars.(i).samples.(sample_ix) in
          none_var.samples.(none_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          none_histo.(cnstr) <- none_histo.(cnstr) + 1
        done;
        for i = var_ix1 to n_vars_1 do
          let { histo = none_histo } as none_var = none_vars.(i - 1) in
          let sample = vars.(i).samples.(sample_ix) in
          none_var.samples.(none_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          none_histo.(cnstr) <- none_histo.(cnstr) + 1
        done
    | FDAtom split_cnstr ->
        let new_sample_ix = split_ixs.(split_cnstr) in
        split_ixs.(split_cnstr) <- new_sample_ix + 1;
        let new_vars = split_vars.(split_cnstr) in
        for i = 0 to var_ix_1 do
          let { histo = new_histo } as new_var = new_vars.(i) in
          let sample = vars.(i).samples.(sample_ix) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done;
        for i = var_ix1 to n_vars_1 do
          let { histo = new_histo } as new_var = new_vars.(i - 1) in
          let sample = vars.(i).samples.(sample_ix) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done
    | FDStrct (split_cnstr, subs) ->
        let new_sample_ix = split_ixs.(split_cnstr) in
        split_ixs.(split_cnstr) <- new_sample_ix + 1;
        let new_vars = split_vars.(split_cnstr) in
        let n_subs = Array.length subs in
        for i = 0 to var_ix_1 do
          let { histo = new_histo } as new_var = new_vars.(i) in
          let sample = vars.(i).samples.(sample_ix) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done;
        let n_subs_1 = n_subs - 1 in
        for i = 0 to n_subs_1 do
          let { histo = new_histo } as new_var = new_vars.(var_ix + i) in
          let sample = subs.(i) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done;
        for i = var_ix1 to n_vars_1 do
          let { histo = new_histo } as new_var = new_vars.(i + n_subs_1) in
          let sample = vars.(i).samples.(sample_ix) in
          new_var.samples.(new_sample_ix) <- sample;
          let cnstr = fdsum_cnstr sample in
          new_histo.(cnstr) <- new_histo.(cnstr) + 1
        done
  done;
  !none_ix_ref, split_vars, none_vars


(* Maybe find unique constructor *)

let maybe_get_unique histo n =
  let rec loop cnstr =
    let freq = histo.(cnstr) in
    if freq > 0 then
      if freq < n then None
      else Some cnstr
    else
      if cnstr > 0 then loop (cnstr - 1)
      else None in
  loop (Array.length histo - 1)


(* Calculation of shave information for domains with redundant constructors *)

type shave_info = ShInfo of pos_infos * vars
and pos_infos = (var_ix * cnstr * subpos_info) list
and subpos_info = (int * shave_info) option

let calc_shave_info fspec vars =
  let n_vars = Array.length vars in
  if n_vars = 0 then ShInfo ([], vars), 0
  else
    let n_samples = Array.length vars.(0).samples in
    let rec loop_var (ShInfo (pos_infos, vars) as sh_info) n_sh_vars var_ix =
      if var_ix < 0 then sh_info, n_sh_vars
      else
        let var = vars.(var_ix) in
        match maybe_get_unique var.histo n_samples with
        | None -> loop_var sh_info (n_sh_vars + 1) (var_ix - 1)
        | Some cnstr ->
            let sub_tps = fspec.(var.tp).(cnstr) in
            let n_subs = Array.length sub_tps in
            if n_subs = 0 then
              let new_sh_info =
                ShInfo ((var_ix, cnstr, None) :: pos_infos, vars) in
              loop_var new_sh_info n_sh_vars (var_ix - 1)
            else (
              let fst_tp = sub_tps.(0) in
              let fst_var =
                {
                  samples = Array.make n_samples dummy_fdsum;
                  tp = fst_tp;
                  histo = Array.make (Array.length fspec.(fst_tp)) 0;
                } in
              let sub_vars = Array.make n_subs fst_var in
              let n_subs_1 = n_subs - 1 in
              for i = 1 to n_subs_1 do
                sub_vars.(i) <-
                  let tp = sub_tps.(i) in
                  {
                    samples = Array.make n_samples dummy_fdsum;
                    tp = tp;
                    histo = Array.make (Array.length fspec.(tp)) 0;
                  }
              done;
              let samples = var.samples in
              for sample_ix = 0 to n_samples - 1 do
                match samples.(sample_ix) with
                | FDStrct (_, subs) ->
                    for i = 0 to n_subs_1 do
                      let sub_var = sub_vars.(i) in
                      let sub = subs.(i) in
                      sub_var.samples.(sample_ix) <- sub;
                      let subhisto = sub_var.histo in
                      let subcnstr = fdsum_cnstr sub in
                      subhisto.(subcnstr) <- subhisto.(subcnstr) + 1
                    done
                | FDAtom _ -> assert false (* impossible *)
              done;
              let new_sh_info, n_sh_sub_vars =
                loop_var (ShInfo ([], sub_vars)) 0 n_subs_1 in
              let new_pos_info =
                var_ix, cnstr, Some (n_sh_sub_vars, new_sh_info) in
              let new_sh_info = ShInfo (new_pos_info :: pos_infos, vars) in
              loop_var new_sh_info (n_sh_vars + n_sh_sub_vars) (var_ix - 1)) in
    loop_var (ShInfo ([], vars)) 0 (n_vars - 1)


(* Compute variables from pos-infos *)

let vars_of_pos_infos n_sh_vars vars pos_infos =
  if pos_infos = [] then vars
  else (
    let sh_vars = Array.make n_sh_vars dummy_var in
    let rec loop vars lix sh_var_ix = function
      | [] ->
          let n_last = Array.length vars - lix in
          Array.blit vars lix sh_vars sh_var_ix n_last;
          sh_var_ix + n_last
      | (sh_ix, _, sub_sh_info) :: rest ->
          let n_between = sh_ix - lix in
          Array.blit vars lix sh_vars sh_var_ix n_between;
          let next_sh_var_ix = sh_var_ix + n_between in
          let next_lix = sh_ix + 1 in
          match sub_sh_info with
          | None | Some (0, _) -> loop vars next_lix next_sh_var_ix rest
          | Some (_, ShInfo (sub_infos, sub_vars)) ->
              let next_sh_var_ix = loop sub_vars 0 next_sh_var_ix sub_infos in
              loop vars next_lix next_sh_var_ix rest in
    ignore (loop vars 0 0 pos_infos);
    sh_vars)
