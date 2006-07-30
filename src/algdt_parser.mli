(*  File: algdt_parser.mli

    AIFAD - Automated Induction of Functions over Algebraic Datatypes

    Author: Markus Mottl
    email:  markus.mottl@gmail.com
    WWW:    http://www.ocaml.info

    Copyright (C) 2004- Markus Mottl

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

(* $Id: algdt_parser.mli,v 1.3 2006/01/17 00:23:37 mottl Exp $ *)

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
