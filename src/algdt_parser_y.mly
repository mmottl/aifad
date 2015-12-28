%{
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
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Utils
open Algdt_types

let make_prod_els prods prod = ProdEls (array_of_rev_list (prod :: prods))
%}

%token EQUAL
%token BAR
%token STAR
%token LPAREN
%token RPAREN
%token COMMA
%token DOT
%token MINUSGREATER
%token EOF
%token <string> UIDENT
%token <string> LIDENT

%start spec
%type <(Algdt_types.tp_name, Algdt_types.cnstr_name) Algdt_types.type_defs> spec

%start data
%type <Algdt_types.cnstr_name Algdt_types.data option> data

%start data_sample
%type <Algdt_types.cnstr_name Algdt_types.data_sample option> data_sample

%start lhs_data
%type <Algdt_types.cnstr_name Algdt_types.data option> lhs_data

%start rhs_data
%type <Algdt_types.cnstr_name Algdt_types.data option> rhs_data

%%

/* Specifications parser */

spec : type_defs EOF { array_of_rev_list $1 }

type_defs :
  | type_def { [$1] }
  | type_defs type_def { $2 :: $1 }

type_def : LIDENT EQUAL rhs DOT { $1, $3 }

rhs :
  | prod_el { Prod $1 }
  | sum { Sums (array_of_rev_list $1) }

prod_el :
  | LIDENT { TpVal $1 }
  | LPAREN prods STAR prod_el RPAREN { make_prod_els $2 $4 }

prods :
  | prod_el { [$1] }
  | prods STAR prod_el { $3 :: $1 }

sum :
  | sums { $1 }
  | BAR sums { $2 }

sums :
  | sum_el { [$1] }
  | sums BAR sum_el { $3 :: $1 }

sum_el :
  | UIDENT { Atom $1 }
  | UIDENT prod_el { Strct ($1, $2) }


/* Data parser */

data :
  | data_el DOT { Some $1 }
  | EOF { None }

data_sample :
  | data_el MINUSGREATER data_el DOT { Some ($1, $3) }
  | EOF { None }

lhs_data :
  | data { $1 }
  | data_el MINUSGREATER data_el DOT { Some $1 }

rhs_data :
  | data { $1 }
  | data_el MINUSGREATER data_el DOT { Some $3 }

data_part :
  | LPAREN data_prods COMMA data_el RPAREN { make_prod_els $2 $4 }
  | UIDENT { TpVal (DAtom $1) }

data_el :
  | UIDENT data_arg { TpVal (DStrct ($1, $2)) }
  | data_part { $1 }

data_arg :
  | LPAREN UIDENT data_arg RPAREN { TpVal (DStrct ($2, $3)) }
  | data_part { $1 }

data_prods :
  | data_prods COMMA data_el { $3 :: $1 }
  | data_el { [$1] }
