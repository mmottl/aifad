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

module type SPEC = sig
  val dispec : ispec
  val cispec : ispec
  val dfspec : fspec
  val cfspec : fspec
  val init_tps : tps
end

type dvar_mod =
  | DVarStrct of tp * cnstr * dvar_mod prod_el
  | DVarFree of (tp * cnstr) dsum
  | DVar of tp

type 'pat dmodel =
  | DVal of (tp * cnstr) data array
  | DLet of 'pat dmatch_mod * tps * dvar_mod prod_els
  | DMatchMod of 'pat dmatch_mod

and 'pat dmatch_mod =
  | DShave of var_ix * tp * cnstr * 'pat * 'pat dmodel * 'pat dmodel
  | DSplit of var_ix * tp * ('pat * 'pat dmodel) array
  | DPSplit of var_ix * tp * ('pat * 'pat dmodel) option array * 'pat dmodel
