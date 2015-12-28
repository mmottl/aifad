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

open Lexing
open Algdt_types

(** Parsers for AIFAD-input *)

(** {6 Parser for data specifications} *)

val spec : lexbuf -> (tp_name, cnstr_name) type_defs
(** [spec lexbuf] reads a data specification from [lexbuf]. @return type
    definitions in specification. *)


(** {6 Data parser} *)

val data : lexbuf -> cnstr_name data option
(** [data lexbuf] reads data from [lexbuf].  Data samples will not be
    accepted.  @return [Some data], or [None] on [EOF]. *)

val data_sample : lexbuf -> cnstr_name data_sample option
(** [data_sample lexbuf] reads data samples (left- and right hand sides)
    from [lexbuf].  @return [Some (lhs, rhs))], or [None] on [EOF]. *)

val lhs_data : lexbuf -> cnstr_name data option
(** [lhs_data lexbuf] reads data and data samples from [lexbuf].  @return
    [Some data] or [Some lhs] of data samples, or [None] on [EOF]. *)

val rhs_data : lexbuf -> cnstr_name data option
(** [rhs_data lexbuf] reads data and data samples from [lexbuf].  @return
    [Some data] or [Some rhs] of data samples, or [None] on [EOF]. *)
