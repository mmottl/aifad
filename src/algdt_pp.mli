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

open Format
open Algdt_types

(** Pretty-printing functions for specifications and data *)

(** {6 Utility functions} *)

val pp_open_par_box : formatter -> unit -> unit
(** [pp_open_par_box ppf ()] prints a left parenthesis and opens a box without
    indentation immediately afterwards using formatter [ppf]. *)

val pp_close_par_box : formatter -> unit -> unit
(** [pp_close_par_box ppf ()] closes a box followed by a right parenthesis using
    formatter [ppf]. *)

(** {6 Pretty-printer for data specifications} *)

val pp_type_def : formatter -> (tp_name, cnstr_name) type_def -> unit
(** [pp_type_def ppf type_def] pretty-print [type_def] using formatter [ppf]. *)

val pp_type_defs : formatter -> (tp_name, cnstr_name) type_defs -> unit
(** [pp_type_defs ppf type_defs] pretty-print [type_defs] using formatter [ppf]. *)

(** {6 Data pretty-printer} *)

module type DATA_PP = sig
  type tag
  (** Type of constructor tags *)

  val pp_data_el : formatter -> tag data -> unit
  (** [pp_data_el ppf data] pretty-print [data]. *)

  val pp_data : formatter -> tag data -> unit
  (** [pp_data ppf data] pretty-print [data] terminated by '.' using formatter
      [ppf]. *)

  val pp_sum : formatter -> tag dsum -> unit
  (** [pp_sum ppf dsum] pretty-print [dsum] using formatter [ppf]. *)

  val pp_sum_arg : formatter -> tag dsum -> unit
  (** [pp_sum_arg ppf dsum] pretty-print [dsum] as constructor argument using
      formatter [ppf]. *)

  val pp_prod_els : formatter -> tag dsum prod_els -> unit
  (** [pp_prod_els ppf prod_els] pretty-print [prod_els] using formatter [ppf]. *)
end

module Data : sig
  include DATA_PP with type tag = cnstr_name

  val pp_sample : formatter -> tag data * tag data -> unit
  (** [pp_sample ppf sample] pretty-print [sample] (= [(lhs, rhs)]) using
      formatter [ppf]. *)
end

(** {6 Generic data pretty-printer (immediate representation)} *)

module type IDATA_META_SPEC = sig
  val cnstr_tbl : cnstr_tbl
end

module Make_IData : functor (Spec : IDATA_META_SPEC) ->
  DATA_PP with type tag = tp * cnstr

module Make_IData2 : functor
  (DSpec : IDATA_META_SPEC)
  (CSpec : IDATA_META_SPEC)
  -> sig
  module D : DATA_PP with type tag = tp * cnstr
  module C : DATA_PP with type tag = D.tag

  val pp_sample : formatter -> D.tag data * C.tag data -> unit
  (** [pp_sample ppf sample] pretty-print [sample] (= [(lhs, rhs)]) using
      formatter [ppf]. *)
end
