(*  File: algdt_parser.ml

    AIFAD - Automated Induction of Functions over Algebraic Datatypes

    Author: Markus Mottl
    email:  markus.mottl@gmail.com
    WWW:    http://www.ocaml.info

    Copyright (C) 2004- Markus Mottl

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

(* $Id: algdt_parser.ml,v 1.2 2006/01/17 00:23:37 mottl Exp $ *)

module P = Algdt_parser_y
module L = Algdt_lexer

let spec = P.spec L.spec
let data = P.data L.data
let data_sample = P.data_sample L.data
let lhs_data = P.lhs_data L.data
let rhs_data = P.rhs_data L.data
