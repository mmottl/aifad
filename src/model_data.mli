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
open Model_types

type missing_value = Ignore | MProb | Flat | LiftAll
type c45_spec = (tp_name * cnstr_names) array * cnstr_names * string * string
type c45_model_data = c45_spec * ispec_info * ispec_info * missing_value * model
type model_data = ispec_info * ispec_info * model
type mdat = [ `Model of model_data | `C45Model of c45_model_data ]

val open_mdat : string -> [> mdat ]
val maybe_save_mdat : mdat -> string option -> unit
val load_model_data : string -> model_data
val load_c45_model_data : string -> c45_model_data
