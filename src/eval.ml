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

open Format

open Utils
open Algdt_types
open Typing
open Info_utils

(* Report result of evaluation *)

(* Evaluation loop *)

let report (cfspec, ctps) read_eval read =
  let cfspec_bits = calc_fspec_bits cfspec in
  let rec loop all_bits cmn_bits avg_sum n_samples =
    let maybe_eval_sample = read_eval () in
    let maybe_sample = read () in
    match maybe_eval_sample, maybe_sample with
    | Some eval_fdsums, Some fdsums ->
        let cmn =
          calc_cmn_fdsums_bits cfspec_bits cfspec ctps eval_fdsums fdsums in
        let bits = calc_fdsums_bits cfspec_bits cfspec ctps eval_fdsums in
        let new_avg_sum = avg_sum +. cmn /. bits in
        loop (all_bits +. bits) (cmn_bits +. cmn) new_avg_sum (n_samples + 1)
    | None, None -> all_bits, cmn_bits, avg_sum, n_samples
    | _ -> failwith "report: datasets have different size!" in
  let all_bits, cmn_bits, avg_sum, n_samples = loop 0.0 0.0 0.0 0 in
  let f_samples = float n_samples in
  printf
    "\
      Samples:              %d@\n\
      Bits:                 %f@\n\
      Average bits:         %f@\n\
      Common bits:          %f@\n\
      Average common bits:  %f@\n\
      Bit Accuracy:         %f@\n\
      Sample accuracy:      %f@\n"
    n_samples
    all_bits
    (all_bits /. f_samples)
    cmn_bits
    (cmn_bits /. f_samples)
    (cmn_bits /. all_bits)
    (avg_sum /. f_samples)


(* Evaluate AIFAD-data *)

let eval cispec_info eval_name =
  let cispec = cispec_info.ispec in
  let cfspec, _ as flattened = flatten_ispec cispec in
  let cnv_crhs = Data_io.make_cnv_data cispec_info cfspec in
  do_open_in eval_name (fun eval_ic ->
    let eval_lexbuf = Lexing.from_channel eval_ic in
    let lexbuf = Lexing.from_channel stdin in
    let read_eval () = Data_io.read_rhs eval_lexbuf cispec cfspec cnv_crhs in
    let read () = Data_io.read_rhs lexbuf cispec cfspec cnv_crhs in
    report flattened read_eval read)


(* Evaluate C4.5-data *)

let eval_c45 { cnstr_htbl; ispec } eval_name =
  do_open_in eval_name (fun eval_ic ->
  let read_eval () = C45_io.read_rhs eval_ic cnstr_htbl in
  let read () = C45_io.read_rhs stdin cnstr_htbl in
  report (flatten_ispec ispec) read_eval read)
