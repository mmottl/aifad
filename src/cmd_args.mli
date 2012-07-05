(*  File: cmd_args.mli

    AIFAD - Automated Induction of Functions over Algebraic Datatypes

    Author: Markus Mottl
    email:  markus.mottl@gmail.com
    WWW:    http://www.ocaml.info

    Copyright (C) 2002  Austrian Research Institute for Artificial Intelligence
    Copyright (C) 2003- Markus Mottl

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

(** Command line interface *)

val version : string
(** [version] version of AIFAD. *)

val start_time : float
(** [start_time] time in Epoche-seconds when program started. *)

(** Type of action to be performed by AIFAD. *)
type action =
  | Print           (** Print model *)
  | Learn           (** Learn from data *)
  | Eval of string  (** [Eval file] compare data from [stdin] and [file] *)
  | Apply           (** Apply model to data on [stdin] *)
  | RandGen of int  (** [RandGen n] randomly generates [n] samples *)

val usage : string
(** [usage] usage message. *)

val args : (Arg.key * Arg.spec * Arg.doc) list
(** [args] arguments accepted by AIFAD. *)

val action : action
(** [action] action to be performed by AIFAD. *)

val maybe_model_name : string option
(** [maybe_model_name] optional name of model file to use. *)

val is_algdt : bool
(** [is_algdt] tells whether data is in AIFAD-format (or C4.5 if not). *)

val maybe_spec : string option
(** [maybe_spec] optional name of specification file to use. *)

val maybe_data_name : string option
(** [maybe_data_name] optional name of data file to use. *)

val maybe_pred_name : string option
(** [maybe_pred_name] optional name of prediction file to use. *)

val split_null_branches : bool
(** [split_null_branches] split null branches. *)

val gain_c45 : bool
(** [gain_c45] use Ross Quinlan's (bad) way of computing the gain ratio. *)

val with_min_gr : bool
(** [with_min_gr] use minimum gain ratio heuristics to preprune trees. *)

val indep_entropy : bool
(** [indep_entropy] compute entropy assuming independence between variables. *)

val shallow_entropy : bool
(** [shallow_entropy] compute entropy without considering substructures. *)

val indep_most_prob : bool
(** [indep_most_prob] compute most probable value assuming independence. *)

val n_rand_gain : int
(** [n_rand_gain] maximum number of generated models using the random gain
    heuristics . *)

val t_rand_gain : float
(** [t_rand_gain] maximum time allowed for generating models using the
    random gain heuristics . *)

val print_hmod : bool
(** [print_hmod] print human-readable model to stdout. *)

val mv : Model_data.missing_value
(** [mv] strategy of handling missing values to use. *)

val factorize : bool
(** [factorize] factorize models. *)

val rand_mv_prob : float
(** [rand_mv_prob] probability of creating missing values in random data. *)

val with_target : bool
(** [with_target] also create target value in random data. *)
