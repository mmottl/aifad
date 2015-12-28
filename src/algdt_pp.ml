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

(* Utility functions *)

let pp_open_par_box ppf () = pp_print_char ppf '('; pp_open_box ppf 0
let pp_close_par_box ppf () = pp_close_box ppf (); pp_print_char ppf ')'

let pp_par_left ppf is_arg = if is_arg then pp_print_char ppf '('
let pp_par_right ppf is_arg = if is_arg then pp_print_char ppf ')'


(* Specification pretty-printer *)

let rec pp_prod ppf p = fprintf ppf "@ *@ %a" pp_prod_el p
and pp_prods ppf ps = array_iter1 (pp_prod ppf) ps

and pp_prod_el ppf = function
  | TpVal tp_name -> pp_print_string ppf tp_name
  | ProdEls ps -> fprintf ppf "(@[%a%a@])" pp_prod_el ps.(0) pp_prods ps

let pp_sum_el ppf = function
  | Atom tag -> pp_print_string ppf tag
  | Strct (tag, prod_el) -> fprintf ppf "%s@ @[%a@]" tag pp_prod_el prod_el

let pp_sum ppf sum_el = fprintf ppf "@ @[|@ %a@]" pp_sum_el sum_el
let pp_sums ppf sums = array_iter1 (pp_sum ppf) sums

let pp_type_def ppf (lhs, rhs) =
  match rhs with
  | Prod prod_el -> fprintf ppf "@\n%s = @[%a@]." lhs pp_prod_el prod_el
  | Sums ss -> fprintf ppf "@\n%s @[= %a%a@]." lhs pp_sum_el ss.(0) pp_sums ss

let pp_type_defs ppf type_defs = Array.iter (pp_type_def ppf) type_defs


(* Generic data pretty-printer *)

module type DATA_PP_SPEC = sig
  type tag
  val pp_tag : formatter -> tag -> unit
end

module type DATA_PP = sig
  type tag

  val pp_data_el : formatter -> tag data -> unit
  val pp_data : formatter -> tag data -> unit
  val pp_sum : formatter -> tag dsum -> unit
  val pp_sum_arg : formatter -> tag dsum -> unit
  val pp_prod_els : formatter -> tag dsum prod_els -> unit
end

module Make_Data (Spec : DATA_PP_SPEC) = struct
  type tag = Spec.tag

  let rec pp_dprod ppf d = fprintf ppf ",@ %a" pp_data_el d

  and pp_dprods ppf prod_els = array_iter1 (pp_dprod ppf) prod_els

  and pp_prod_els ppf prod_els =
    fprintf ppf "(@[%a%a@])" pp_data_el prod_els.(0) pp_dprods prod_els

  and pp_data_el ppf = pp_data_el_ false ppf
  and pp_arg ppf = pp_data_el_ true ppf
  and pp_sum ppf = pp_sum_ false ppf
  and pp_sum_arg ppf = pp_sum_ true ppf

  and pp_sum_ is_arg ppf = function
    | DAtom tag -> Spec.pp_tag ppf tag
    | DStrct (tag, arg) ->
        fprintf ppf "%a@[<2>%a@ %a@]%a"
          pp_par_left is_arg Spec.pp_tag tag pp_arg arg pp_par_right is_arg

  and pp_data_el_ is_arg ppf = function
    | TpVal sum -> pp_sum_ is_arg ppf sum
    | ProdEls prod_els -> pp_prod_els ppf prod_els

  let pp_data ppf d = fprintf ppf "@[<2>%a@]." pp_data_el d
end

let pp_sample dpp cpp ppf (d, c) = fprintf ppf "@[<2>%a ->@ %a@]." dpp d cpp c


(* Data pretty-printer *)

module Data_Spec = struct
  type tag = cnstr_name
  let pp_tag = pp_print_string
end

module Data = struct
  include Make_Data (Data_Spec)

  let pp_sample = pp_sample pp_data_el pp_data_el
end


(* Generic data pretty-printer (immediate representation) *)

module type IDATA_META_SPEC = sig val cnstr_tbl : cnstr_tbl end

module Make_IData_Spec (Spec : IDATA_META_SPEC) = struct
  type tag = tp * cnstr
  let pp_tag ppf (tp, cnstr) = pp_print_string ppf Spec.cnstr_tbl.(tp).(cnstr)
end

module Make_IData (IData_Meta_Spec : IDATA_META_SPEC) =
  Make_Data (Make_IData_Spec (IData_Meta_Spec))

module Make_IData2
  (DIData_Meta_Spec : IDATA_META_SPEC)
  (CIData_Meta_Spec : IDATA_META_SPEC) = struct
  module D = Make_Data (Make_IData_Spec (DIData_Meta_Spec))
  module C = Make_Data (Make_IData_Spec (CIData_Meta_Spec))

  let pp_sample = pp_sample D.pp_data_el C.pp_data_el
end
