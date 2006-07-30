open Algdt_types
open Model_types

type missing_value = Ignore | MProb | Flat | LiftAll
type c45_spec = (tp_name * cnstr_names) array * cnstr_names * string * string
type c45_model_data = c45_spec * ispec_info * ispec_info * missing_value * model
type model_data = ispec_info * ispec_info * model
type mdat = [ `Model of model_data | `C45Model of c45_model_data ]

val open_mdat : string -> [> mdat]
val maybe_save_mdat : mdat -> string option -> unit

val load_model_data : string -> model_data
val load_c45_model_data : string -> c45_model_data
