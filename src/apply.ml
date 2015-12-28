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

open Pcre

open Utils
open Algdt_types
open Algdt_utils
open Algdt_pp
open Model_data
open Data_io
open Typing

(* Apply model to data *)

let handle_model pred_oc = function
  | `Model (
        dispec_info, { ispec = cispec; cnstr_tbl = ccnstr_tbl }, model) ->
      let dispec = dispec_info.ispec in
      let dfspec, _ = flatten_ispec dispec_info.ispec in
      let cfspec, _ = flatten_ispec cispec in
      let ppf = formatter_of_out_channel pred_oc in
      let lexbuf = Lexing.from_channel stdin in
      let cnv_drhs = make_cnv_data dispec_info dfspec in
      let cnv_many_prod_el = snd (make_deco_dsum_cnv cfspec cispec) in
      let module CIDataPP =
        Make_IData (struct let cnstr_tbl = ccnstr_tbl end) in
      let rec loop () =
        match read_lhs lexbuf dispec dfspec cnv_drhs with
        | Some fdsums ->
            let res = Model_utils.apply_model model fdsums in
            CIDataPP.pp_data ppf (cnv_many_prod_el init_prod_els res).(0);
            pp_force_newline ppf ();
            loop ()
        | None -> pp_print_flush ppf () in
      loop ()
  | `C45Model ((_, _, _, pat), dispec_info, cispec_info, mv, model) ->
      let rex = regexp ~flags:[`DOTALL] pat in
      let dcnstr_htbl = dispec_info.cnstr_htbl in
      let ccnstr_tbl = cispec_info.cnstr_tbl in
      let n_lines_ref = ref 1 in
      let find_cnstr ix str = FDAtom (Hashtbl.find dcnstr_htbl (ix + 1, str)) in
      let print_app fdsums =
        match (Model_utils.apply_model model fdsums).(0) with
        | FDAtom cnstr ->
            output_string pred_oc ccnstr_tbl.(1).(cnstr);
            output_char pred_oc '\n'
        | FDStrct _ -> assert false (* impossible *) in
      let full_match = false in
      match mv with
      | Ignore ->
          foreach_line (fun line ->
            try
              print_app (Array.mapi find_cnstr (extract ~full_match ~rex line));
              incr n_lines_ref;
            with Not_found ->
              let msg =
                sprintf
                  "invalid input line (maybe missing values?): %d\n"
                  !n_lines_ref in
              failwith msg)
      | MProb ->
          let cnv_cnstr ix str =
            if str = "?" then raise Exit
            else find_cnstr ix str in
          foreach_line (fun line ->
            try
              let fdsum =
                try
                  let subs =
                    Array.mapi cnv_cnstr (extract ~full_match ~rex line) in
                  FDStrct (1, subs)
                with Exit -> dummy_fdsum  (* dummy_fdsum = MV! *) in
              print_app [| fdsum |];
              incr n_lines_ref
            with Not_found ->
              failwith (sprintf "invalid input line: %d\n" !n_lines_ref))
      | Flat ->
          let cnv_cnstr ix str =
            if str = "?" then dummy_fdsum  (* dummy_fdsum = MV! *)
            else find_cnstr ix str in
          foreach_line (fun line ->
            try
              print_app (Array.mapi cnv_cnstr (extract ~full_match ~rex line));
              incr n_lines_ref
            with Not_found ->
              failwith (sprintf "invalid input line: %d\n" !n_lines_ref))
      | LiftAll ->
          let cnv_cnstr ix str =
            if str = "?" then dummy_fdsum  (* dummy_fdsum = MV! *)
            else FDStrct (1, [| find_cnstr ix str |]) in
          foreach_line (fun line ->
            try
              print_app (Array.mapi cnv_cnstr (extract ~full_match ~rex line));
              incr n_lines_ref
            with Not_found ->
              failwith (sprintf "invalid input line: %d\n" !n_lines_ref))

let apply model_name maybe_pred_name =
  let pred_oc, close_pred_oc =
    match maybe_pred_name with
    | Some pred_name -> let oc = open_out pred_name in oc, close_out
    | None -> stdout, flush in
  let mdat = open_mdat model_name in
  unwind_protect (handle_model pred_oc) mdat close_pred_oc pred_oc
