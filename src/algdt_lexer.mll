(*  File: algdt_lexer.mll

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

{
open Lexing
open Algdt_parser_y

let ill_char c = failwith ("illegal character: '" ^ c ^ "'")
}

let ws = [' ' '\010' '\013' '\009' '\012']
let lc = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uc = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let idc =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']

(* Lexer for data specifications *)
rule spec = parse
  | ws+ { spec lexbuf }
  | '#' [^ '\n']* '\n' { spec lexbuf }
  | "=" { EQUAL }
  | "|" { BAR }
  | "*" { STAR }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "." { DOT }
  | uc idc* as id { UIDENT id }
  | lc idc* as id { LIDENT id }
  | eof { EOF }
  | _ { ill_char (lexeme lexbuf) }

(* Data lexer *)
and data = parse
  | ws+ { data lexbuf }
  | '#' [^ '\n']* '\n' { data lexbuf }
  | "," { COMMA }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "." { DOT }
  | "->" { MINUSGREATER }
  | uc idc* as id { UIDENT id }
  | eof { EOF }
  | _ { ill_char (lexeme lexbuf) }
