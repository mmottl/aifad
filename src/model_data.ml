open Utils
open Algdt_types
open Model_types

type missing_value = Ignore | MProb | Flat | LiftAll
type c45_spec = (tp_name * cnstr_names) array * cnstr_names * string * string
type c45_model_data = c45_spec * ispec_info * ispec_info * missing_value * model
type model_data = ispec_info * ispec_info * model
type mdat = [ `Model of model_data | `C45Model of c45_model_data ]

(* Load and save model files *)

let handle_mdat = function
  | `Model (dispec_info, cispec_info, model) -> dispec_info, cispec_info, model
  | _ -> failwith "handle_mdat: not a valid model!"

let open_mdat model_name = do_open_in model_name Marshal.from_channel
let load_model_data model_name = handle_mdat (open_mdat model_name)

let maybe_save_mdat mdat = function
  | Some model_name ->
      do_open_out model_name (fun model_oc ->
        Marshal.to_channel model_oc mdat [Marshal.No_sharing]);
  | None -> ()

let handle_mdat_c45 = function
  | `C45Model (spec, dispec_info, cispec_info, mv, model) ->
      spec, dispec_info, cispec_info, mv, model
  | _ -> failwith "handle_mdat_c45: not a valid C4.5-model"

let load_c45_model_data model_name = handle_mdat_c45 (open_mdat model_name)
