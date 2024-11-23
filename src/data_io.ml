(* AIFAD - Automated Induction of Functions over Algebraic Datatypes

   Copyright © 2002 Austrian Research Institute for Artificial Intelligence
   Copyright © 2003- Markus Mottl <markus.mottl@gmail.com>

   This library is free software; you can redistribute it and/or modify it under
   the terms of the GNU Lesser General Public License as published by the Free
   Software Foundation; either version 2.1 of the License, or (at your option)
   any later version.

   This library is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
   FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
   details.

   You should have received a copy of the GNU Lesser General Public License
   along with this library; if not, write to the Free Software Foundation, Inc.,
   51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA *)

open Utils
open Algdt_types
open Algdt_utils
open Typing
open MyCfg

let failwith str = failwith ("Data_io." ^ str)

(* Read specification of algebraic datatypes from a lexbuf *)

let get_domain_tp name (_, _, tp_htbl, _) =
  try Hashtbl.find tp_htbl name
  with Not_found ->
    failwith (Printf.sprintf "get_domain_tp: no valid domain type '%s'" name)

let make_ispec_info dom pre_ispec_info cfg =
  let dtp = get_domain_tp dom pre_ispec_info in
  calc_ispec_info (make_sane_live cfg dtp) dtp pre_ispec_info

let read_spec lexbuf =
  let type_defs = Algdt_parser.spec lexbuf in
  let pre_ispec_info = calc_pre_ispec_info type_defs in
  let cfg = cfg_of_pre_ispec_info pre_ispec_info in
  ( make_ispec_info "domain" pre_ispec_info cfg,
    make_ispec_info "codomain" pre_ispec_info cfg )

(* Convert data to immediate representation *)

let store_node node = function
  | (i, ar) :: rest ->
      ar.(i) <- node;
      let i' = i + 1 in
      if i' = Array.length ar then rest else (i', ar) :: rest
  | [] -> assert false

let make_cnv_data { cnstr_htbl; ispec; tp_tbl } fspec =
  let find_cnstr tp cnstr =
    try Hashtbl.find cnstr_htbl (tp, cnstr)
    with Not_found ->
      let err =
        Printf.sprintf "find_cnstr: unknown constructor %s of type %s" cnstr
          tp_tbl.(tp)
      in
      failwith err
  in
  let rec cnv_data_rhs ar_lst tp dpe =
    match ispec.(tp) with
    | Prod pe -> cnv_data ar_lst pe dpe
    | Sums isums -> (
        match dpe with
        | ProdEls _ -> failwith "cnv_data_rhs: expected constructor"
        | TpVal (DAtom cnstr_name) -> (
            let cnstr = find_cnstr tp cnstr_name in
            match isums.(cnstr) with
            | IAtom -> store_node (FDAtom cnstr) ar_lst
            | IStrct _ ->
                failwith "cnv_data_rhs: constructor has too few arguments")
        | TpVal (DStrct (cnstr_name, next_dpe)) -> (
            let cnstr = find_cnstr tp cnstr_name in
            match isums.(cnstr) with
            | IStrct next_pe ->
                let new_ar_len = Array.length fspec.(tp).(cnstr) in
                let new_ar = Array.make new_ar_len dummy_fdsum in
                let new_node = FDStrct (cnstr, new_ar) in
                let new_ar_lst = (0, new_ar) :: store_node new_node ar_lst in
                cnv_data new_ar_lst next_pe next_dpe
            | IAtom ->
                failwith "cnv_data_rhs: constructor has too many arguments"))
  and cnv_data ar_lst pe dpe =
    match (pe, dpe) with
    | TpVal tp, _ -> cnv_data_rhs ar_lst tp dpe
    | ProdEls iprods, ProdEls dprods ->
        if Array.length iprods <> Array.length dprods then
          failwith "cnv_data: expected product of different dimension"
        else
          let coll i ar_lst ipe = cnv_data ar_lst ipe dprods.(i) in
          array_fold_lefti coll ar_lst iprods
    | ProdEls _, TpVal _ -> failwith "cnv_data: expected product"
  in
  cnv_data_rhs

(* Builds sample in immediate representation *)

let make_fdsums ispec fspec cnv_rhs data =
  let fdsums = Array.make (get_n_fdsums ispec fspec) dummy_fdsum in
  ignore (cnv_rhs [ (0, fdsums) ] 0 data);
  fdsums

(* Read a single sample *)

let read_sample lexbuf dispec dfspec cispec cfspec cnv_drhs cnv_crhs =
  match Algdt_parser.data_sample lexbuf with
  | Some (ddata, cdata) ->
      let dfdsums = make_fdsums dispec dfspec cnv_drhs ddata in
      let cfdsums = make_fdsums cispec cfspec cnv_crhs cdata in
      Some (dfdsums, cfdsums)
  | None -> None

(* Read an array of samples *)

let read_samples lexbuf dispec_info dfspec cispec_info cfspec =
  let cnv_drhs = make_cnv_data dispec_info dfspec in
  let cnv_crhs = make_cnv_data cispec_info cfspec in
  let dispec = dispec_info.ispec in
  let cispec = cispec_info.ispec in
  let n_dfdsums = get_n_fdsums dispec dfspec in
  let n_cfdsums = get_n_fdsums cispec cfspec in
  let dfdsums_lix = n_dfdsums - 1 in
  let cfdsums_lix = n_cfdsums - 1 in
  let rec loop n_samples smpl_lst =
    match read_sample lexbuf dispec dfspec cispec cfspec cnv_drhs cnv_crhs with
    | Some pair -> loop (n_samples + 1) (pair :: smpl_lst)
    | None ->
        let dvars = Array.make_matrix n_dfdsums n_samples dummy_fdsum in
        let cvars = Array.make_matrix n_cfdsums n_samples dummy_fdsum in
        let n_samples_1 = n_samples - 1 in
        let act n (dsmpl, csmpl) =
          for i = 0 to dfdsums_lix do
            dvars.(i).(n_samples_1 - n) <- dsmpl.(i)
          done;
          for i = 0 to cfdsums_lix do
            cvars.(i).(n_samples_1 - n) <- csmpl.(i)
          done
        in
        List.iteri act smpl_lst;
        (dvars, cvars)
  in
  loop 0 []

(* Read a single element of data *)

let read_data parse lexbuf ispec fspec cnv_rhs =
  match parse lexbuf with
  | Some data -> Some (make_fdsums ispec fspec cnv_rhs data)
  | None -> None

let read_lhs lexbuf dispec dfspec cnv_drhs =
  read_data Algdt_parser.lhs_data lexbuf dispec dfspec cnv_drhs

let read_rhs lexbuf cispec cfspec cnv_crhs =
  read_data Algdt_parser.rhs_data lexbuf cispec cfspec cnv_crhs
