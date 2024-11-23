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

type cfg_prod = CProd | CSum of cnstr

module AlgDtCfgSpec : sig
  type t = unit
  and nt = tp
  and symbol = NT of nt | T of t
  and prod = cfg_prod

  val compare_t : t -> t -> int
  val compare_nt : nt -> nt -> int
  val compare_prod : prod -> prod -> int
end

module MyCfg : Cfg.Cfg_intf.CFG with module Spec = AlgDtCfgSpec

val calc_pre_ispec_info : (cnstr_name, tp_name) type_defs -> pre_ispec_info
val cfg_of_pre_ispec_info : pre_ispec_info -> MyCfg.grammar
val calc_ispec_info : MyCfg.live_grammar -> tp -> pre_ispec_info -> ispec_info
val flatten_ispec : ispec -> fspec * tps
