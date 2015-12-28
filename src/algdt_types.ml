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

(** Specification of widely used types *)

(** {6 Algebraic types} *)

type 'tp_val prod_els = 'tp_val prod_el array
and 'tp_val prod_el = TpVal of 'tp_val | ProdEls of 'tp_val prod_els

type ('tp_val, 'cnstr) sum = Atom of 'cnstr | Strct of 'cnstr * 'tp_val prod_el
type ('prod, 'sums) rhs = Prod of 'prod | Sums of 'sums


(** {6 Data specifications (= type definitions)} *)

type ('tp_val, 'cnstr) type_def =
  'tp_val * ('tp_val prod_el, ('tp_val, 'cnstr) sum array) rhs

type ('tp_val, 'cnstr) type_defs = ('cnstr, 'tp_val) type_def array


(** {6 Immediate representation of types using integer encodings} *)

type tp = int
type tps = tp array

type isum = IAtom | IStrct of tp prod_el
type irhs = (tp prod_el, isum array) rhs


(** {6 Data representation} *)

type 'tag dsum = DAtom of 'tag | DStrct of 'tag * 'tag dsum prod_el
type 'tag data = 'tag dsum prod_el
type 'tag data_sample = 'tag data * 'tag data


(** {6 Flat data representation (nested tuples are flattened)} *)

type cnstr = int
type fspec = tps array array

type fdsum = FDAtom of cnstr | FDStrct of cnstr * fdsums
and fdsums = fdsum array


(** {6 Useful abbreviations for internal representation of data
       specifications} *)

type tp_name = string
type tp_tbl = tp_name array
type cnstr_name = string
type cnstr_names = cnstr_name array
type cnstr_tbl = cnstr_names array
type tp_htbl = (tp_name, tp) Hashtbl.t
type cnstr_htbl = (tp * cnstr_name, cnstr) Hashtbl.t
type ispec = irhs array

type pre_ispec_info = tp_name array * cnstr_tbl * tp_htbl * ispec

type ispec_info =
  {
    tp_tbl : tp_tbl;
    cnstr_tbl : cnstr_tbl;
    tp_htbl : tp_htbl;
    cnstr_htbl : cnstr_htbl;
    ispec : ispec;
  }


(** {6 Types about variables} *)

type samples = fdsums
type histo = int array
type cntg_tbl = histo array
type cntg_tbls = cntg_tbl array

type var =
  {
    samples : samples;
    tp : tp;
    histo : histo;
  }

type vars = var array
type var_ix = int
type ix_cnt = var_ix
