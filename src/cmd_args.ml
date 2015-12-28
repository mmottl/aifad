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

let start_time = Unix.gettimeofday ()

open Model_data

type action = Print | Learn | Eval of string | Apply | RandGen of int

let action_ref = ref Print
let maybe_model_name_ref = ref None
let is_algdt_ref = ref true
let maybe_spec_ref = ref None
let maybe_data_name_ref = ref None
let maybe_pred_name_ref = ref None
let split_null_branches_ref = ref false
let gain_c45_ref = ref false
let with_min_gr_ref = ref false
let indep_entropy_ref = ref false
let shallow_entropy_ref = ref false
let indep_most_prob_ref = ref false
let n_rand_gain_ref = ref 0
let t_rand_gain_ref = ref 10.0
let print_hmod_ref = ref true
let mv_ref = ref LiftAll
let factorize_ref = ref true
let rand_mv_prob_ref = ref 0.01
let with_target_ref = ref true

let usage = Printf.sprintf ("Usage: %s [OPTION]...\n") Sys.argv.(0)

let rec args =
  [
    (
      "-learn", Arg.Unit (fun () -> action_ref := Learn),
      "\t\tlearn model"
    );(
      "-apply", Arg.Unit (fun () -> action_ref := Apply),
      "\t\tapply a given model to data on stdin"
    );(
      "-eval", Arg.String (fun name -> action_ref := Eval name),
     "\t\tcompare data on stdin to the reference in the given file\n"
    );(
      "-c45", Arg.Clear is_algdt_ref, "\t\t\tchange to C4.5-processing\n"
    );(
      "-spec", Arg.String (fun spec -> maybe_spec_ref := Some spec),
      "\t\tfilename of data specification"
    );(
      "-stem",
      Arg.String (fun stem ->
        let ssfx, dsfx =
          if !is_algdt_ref then ".ads", ".add"
          else ".names", ".data" in
        maybe_spec_ref := Some (stem ^ ssfx);
        maybe_data_name_ref := Some (stem ^ dsfx)),
      "\t\tfilestem of specification + data\n"
    );(
      "-model", Arg.String (fun str -> maybe_model_name_ref := Some str),
      "\t\tfilename of machine-readable model file"
    );(
      "-pred", Arg.String (fun str -> maybe_pred_name_ref := Some str),
      "\t\tfilename of predictions (stdout)\n"
    );(
      "-no-hmod", Arg.Clear print_hmod_ref,
      "\t\tsuppress printing of human-readable model\n"
    );(
      "-gain-c45", Arg.Set gain_c45_ref,
      "\t\tuse gain ratio as implemented by Quinlan in C4.5"
    );(
      "-with-min-gr", Arg.Set with_min_gr_ref,
      "\t\tpre-prune using minimum gain ratio criterion\n"
    );(
      "-indep-entropy", Arg.Set indep_entropy_ref,
      "\tassume independence of tuples during\n\
       \t\t\tentropy computation"
    );(
      "-shallow-entropy", Arg.Set shallow_entropy_ref,
      "\tperform shallow computation of entropy"
    );(
      "-indep-most-prob", Arg.Set indep_most_prob_ref,
      "\tassume independence among tuple values in codomain\n\
       \t\t\tduring computation of most probable value"
     );(
      "-split-null", Arg.Set split_null_branches_ref,
      "\t\tsplit null branches\n"
    );(
      "-n-rand-gain", Arg.Set_int n_rand_gain_ref,
      "\t\tgenerate additional random models dependent\n\
       \t\t\ton gain ratio"
    );(
      "-t-rand-gain", Arg.Set_float t_rand_gain_ref,
      "\t\ttime limit in seconds for generation of\n\
       \t\t\trandom models (default: 10)\n"
    );(
      "-mv-ignore", Arg.Unit (fun () -> mv_ref := Ignore),
      "\t\tignore C4.5-samples with missing-values"
    );(
      "-mv-mprob", Arg.Unit (fun () -> mv_ref := MProb),
      "\t\tlift complete left-hand-side of C4.5-samples\n\
       \t\t\timplies most probable class for samples with\n\
       \t\t\tmissing values"
    );(
      "-mv-flat", Arg.Unit (fun () -> mv_ref := Flat),
      "\t\tadd constructor for missing values"
    );(
      "-mv-lift-all", Arg.Unit (fun () -> mv_ref := LiftAll),
      "\t\tlift each input variable of C4.5-samples (default)\n"
    );(
      "-no-factor", Arg.Clear factorize_ref,
      "\t\tdo not factorize values out of matches\n"
    );(
      "-rand-gen", Arg.Int (fun n -> action_ref := RandGen n),
      "\t\tgenerate n random samples on stdout"
    );(
      "-rand-gen-no-target",
      Arg.Int (fun n -> action_ref := RandGen n; with_target_ref := false),
      "\tgenerate n random samples without target on stdout"
    );(
      "-rand-mv-prob", Arg.Set_float rand_mv_prob_ref,
      "\tset probability of random missing values (0.01)"
    );(
      "-rand-init", Arg.Int Random.init,
      "\t\tInitialize random number generator with given seed"
    );(
      "-rand-self-init", Arg.Unit Random.self_init,
      "\tInitialize random number generator (system-dependent)\n"
    );(
      "-v", Arg.Unit (fun () -> print_endline Version.version; exit 0),
      "\t\t\tshow version number, then exit\n"
    );(
      "-help", Arg.Unit (fun () -> Arg.usage args usage; exit 0),
      "\t\tdisplay this list of options, then exit"
    );(
      "--help", Arg.Unit (fun () -> Arg.usage args usage; exit 0),
      "\t\tdisplay this list of options, then exit"
    )
  ]

let anon_arg _ = raise (Arg.Bad "no free arguments allowed")

let _ = Arg.parse args anon_arg usage

let action = !action_ref
let maybe_model_name = !maybe_model_name_ref
let is_algdt = !is_algdt_ref
let maybe_spec = !maybe_spec_ref
let maybe_data_name = !maybe_data_name_ref
let maybe_pred_name = !maybe_pred_name_ref
let split_null_branches = !split_null_branches_ref
let gain_c45 = !gain_c45_ref
let with_min_gr = !with_min_gr_ref
let indep_entropy = !indep_entropy_ref
let shallow_entropy = !shallow_entropy_ref
let indep_most_prob = !indep_most_prob_ref
let n_rand_gain = !n_rand_gain_ref
let t_rand_gain = !t_rand_gain_ref
let print_hmod = !print_hmod_ref
let mv = !mv_ref
let factorize = !factorize_ref
let rand_mv_prob = !rand_mv_prob_ref
let with_target = !with_target_ref
