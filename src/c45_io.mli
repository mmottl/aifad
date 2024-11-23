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

open Algdt_types
open Model_data

(** Functions for reading and handling C4.5 specifications and data. *)

val read_c45_spec : string -> c45_spec
(** [read_c45_spec file] reads a C4.5-specification (= *.names-file) from
    [file]. *)

val calc_dispec_info : missing_value -> c45_spec -> ispec_info
(** [calc_dispec_info mv c45_spec] computes the immediate representation of the
    specification of the domain type definitions in [c45_spec] given the
    strategy for handling missing values [mv]. *)

val calc_cispec_info : c45_spec -> ispec_info
(** [calc_cispec_info c45_spec] computes the immediate representation of the
    specification of the codomain using [c45_spec]. *)

val read_c45_data :
  c45_spec ->
  missing_value ->
  in_channel ->
  ispec_info * vars * ispec_info * vars
(** [read_c45_data c45_spec mv ic] reads in C4.5-data from channel [ic] using
    specification [c45_spec] and missing-value strategy [mv].
    @return the tuple [(dispec_info, dvars, cispec_info, cvars)]. *)

val read_rhs : in_channel -> cnstr_htbl -> fdsums option
(** [read_rhs ic cnstr_htbl] reads in one element of right hand side C4.5-data
    from channel [ic] using the constructor hashtable [cnstr_htbl]. *)
