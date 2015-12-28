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

open Algdt_types

(** Utility functions and definitions *)

(** {6 Initial, dummy and empty values} *)

val init_tp : tp
(** [init_tp] is the initial type from which values are derived in a
    (co-)domain. *)

val init_prod_el : tp prod_el
(** [init_prod_el] is the initial product element from which values are
    derived in a (co-)domain. *)

val init_prod_els : tp prod_els
(** [init_prod_els] is the initial pseudo-product containing only one
    element from which values are derived in a (co-)domain. *)

val dummy_init_tps : tps
(** [dummy_init_tps] is a dummy value for initial types. *)

val dummy_fdsum : fdsum
(** [dummy_fdsum] is a dummy data value in flat representation. *)

val dummy_fdsums : fdsums
(** [dummy_fdsums] is an array containing one dummy data value in flat
    representation. *)

val dummy_var : var
(** [dummy_var] is a dummy variable containing nothing. *)

val empty_fspec : fspec
(** [empty_fspec] an empty specification of flattened data. *)


(** TODO *)

val get_n_fdsums : ispec -> fspec -> int
(** [get_n_fdsums ispec fspec] @return number of variables (= elements of
    topmost flattened product) derived from [fspec] given [ispec]. *)

val get_init_tps : ispec -> fspec -> tps
(** [get_init_tps fspec ispec] @return initial types of variables (=
    elements of topmost flattened product) derived from [fspec] given
    [ispec]. *)

val fdsum_cnstr : fdsum -> cnstr
(** [fdsum_cnstr fdsum] @return constructor tag of [fdsum]. *)

val histo_is_redundant : histo -> int -> bool
(** [histo_is_redundant histo n_samples] checks whether histogram
    [histo] contains only one constructor. [n_samples] must be the number
    of samples counted in [histo]! *)

val many_fdsums_of_vars : vars -> fdsums array
(** [many_fdsums_of_vars vars] @return data elements in [vars] row-wise. *)

val make_deco_dsum_cnv :
  fspec -> ispec ->
  (tp -> fdsum -> (tp * cnstr) dsum) *
  (tp prod_els -> fdsums -> (tp * cnstr) dsum prod_els)
(** [make_deco_dsum_cnv fspec ispec] @return the tuple of functions
    [(cnv_fdsum, cnv_many_prod_els)], which convert from undecorated,
    flattened data to decorated (= type annotated), unflattened data. *)

val calc_cntg_tbls : var -> vars -> cntg_tbls
(** [calc_cntg_tbls dvar cvars] @return contingency table for each
    split of [cvars] on [dvar] as array of contingency tables. *)

val make_vars : fspec -> tps -> samples array -> vars
(** [make_vars fspec init_tps many_samples] @return variables with each
    containing the samples from the corresponding element in
    [many_samples]. *)

val split_vars : var -> fspec -> vars -> vars array
(** [split_vars dvar cfspec cvars] split [cvars] on [dvar] given
    [cfspec]. *)

val split_with_sub_vars : int -> fspec -> vars -> int -> vars array
(** [split_with_sub_vars min_freq fspec vars var_ix] splits [vars] on
    the variable denoted by [var_ix] adding possible subvariables:
    (left, X (x, y), right) -> (left, x, y, right).
    Ignore branches with less than or equal [min_freq] samples. *)

val split_cnstr_with_sub_vars : fspec -> vars -> int -> cnstr -> vars * vars
(** [split_cnstr_with_sub_vars fspec vars var_ix cnstr] splits [vars]
    the variable denoted by [var_ix] into two variable sets [(cnstr_vars,
    others)], where [cnstr_vars] contains the variables with samples
    where [cnstr] matched, including possible subvariables, and [others]
    contains the other cases.
    (left, X (x, y), right) -> (left, x, y, right) *)

val shave_with_sub_vars : fspec -> vars -> int -> cnstr -> vars
(** [shave_with_sub_vars fspec vars var_ix cnstr] shaves variable with
    a given constructor and adds possible subvariables.
    (left, X (x, y), right) -> (left, x, y, right) *)

val atomic_shave_vars : vars -> int -> vars
(** [atomic_shave_vars vars var_ix] shaves variables without adding
    subvariables. *)

val maybe_split_with_sub_vars :
  fspec -> vars -> int -> 'a option array -> int * vars array * vars
(** [maybe_split_with_sub_vars fspec vars var_ix maybes] splits variables
    on maybe constructors, adding subvariables as required. *)


(** {6 Shave information and functions} *)

type shave_info = ShInfo of pos_infos * vars
and pos_infos = (var_ix * cnstr * subpos_info) list
and subpos_info = (int * shave_info) option

val calc_shave_info : fspec -> vars -> shave_info * int

val vars_of_pos_infos : int -> vars -> pos_infos -> vars
