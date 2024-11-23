(* AIFAD - Automated Induction of Functions over Algebraic Datatypes

   Copyright © 2002 Austrian Research Institute for Artificial Intelligence
   Copyright © 2003- Markus Mottl <markus.mottl@gmail.com>

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
   Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA *)

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
