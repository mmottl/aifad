SOURCES := \
  utils.ml \
  \
  algdt_types.mli \
  model_types.mli \
  \
  model_data.mli \
  model_data.ml \
  \
  algdt_parser_y.mly \
  algdt_lexer.mll \
  algdt_parser.mli \
  algdt_parser.ml \
  \
  algdt_pp.mli \
  algdt_pp.ml \
  \
  algdt_utils.mli \
  algdt_utils.ml \
  \
  typing.mli \
  typing.ml \
  \
  data_io.ml \
  \
  entropy_utils.ml \
  info_utils.ml \
  \
  deco_intf.mli \
  deco_impl.ml \
  \
  model_pp_intf.mli \
  model_pp_impl.ml \
  \
  model_utils.ml \
  factor.ml \
  \
  most_prob.ml \
  \
  dshave.ml \
  split_intf.mli \
  split_impl.mli \
  split_impl.ml \
  \
  is_entropy.ml \
  id_entropy.ml \
  ds_entropy.ml \
  ds2_entropy.ml \
  dd2_entropy.ml \
  \
  gain_intf.mli \
  gain_impl.ml \
  \
  c45_io.mli \
  c45_io.ml \
  \
  cmd_args.mli \
  cmd_args.ml \
  \
  complexity.mli \
  complexity.ml \
  \
  learn.ml \
  apply.mli \
  apply.ml \
  eval.ml \
  rand_gen.ml \
  \
  main.ml

RESULT := aifad
OCAMLFLAGS = -warn-error A
PACKS := cfg res pcre
THREADS := y

all: dc

export OCAMLMAKEFILE=../OCamlMakefile
-include $(OCAMLMAKEFILE)
