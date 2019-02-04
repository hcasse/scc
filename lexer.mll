(*	
 *	This file is part of SCC.
 *
 *	SCC is free software: you can redistribute it and/or modify
 *	it under the terms of the GNU General Public License as published by
 *	the Free Software Foundation, either version 3 of the License, or
 *	(at your option) any later version.
 *
 *	SCC is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *	GNU General Public License for more details.
 *
 *	You should have received a copy of the GNU General Public License
 *	along with SCC.  If not, see <http://www.gnu.org/licenses/>.
 *)
{
open Parser

}

let space = [' ' '\t' '\n']+
let int = ['0'-'9']+
let frac = '.' int
let exp = ['e' 'E'] ['+' '-']? int
let flt = frac | int frac | int exp | frac exp | int frac exp
let id = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule scan =
parse	";"			{ SEMI }
|		","			{ COM }
|		":"			{ COL }
|		"("			{ LPAR }
|		")"			{ RPAR }
|		"{"			{ LBRACE }
|		"}"			{ RBRACE }

|		"="			{ EQ }
|		"!"			{ EXCLAM }
|		"+"			{ PLUS }
|		"-"			{ MINUS }
|		"*"			{ STAR }
|		"/"			{ SLASH }
|		"%"			{ PERCENT }
|		"=="		{ EQ_EQ }
|		"!="		{ EXCLAM_EQ }
|		"<"			{ LT }
|		"<="		{ LT_EQ }
|		">"			{ GT }
|		">="		{ GT_EQ }
|		"&"			{ AND }
|		"&&"		{ AND_AND }
|		"||"		{ PIPE_PIPE }
|		"++"		{ PLUS_PLUS }
|		"--"		{ MINUS_MINUS }
|		"<<"		{ LT_LT }
|		">>"		{ GT_GT }

|		"case"		{ CASE }
|		"char"		{ TYPE Common.CHAR }
|		"default"	{ DEFAULT }
|		"do"		{ DO }
|		"else"		{ ELSE }
|		"float"		{ TYPE Common.FLOAT }
|		"for"		{ FOR }
|		"if"		{ IF }
|		"int"		{ TYPE Common.INT }
|		"return"	{ RETURN }
|		"switch"	{ SWITCH }
|		"void"		{ TYPE Common.VOID }
|		"while"		{ WHILE }

|		id as s		{ ID s }
|		int	as s	{ INT (int_of_string s) }
|		flt as s	{ FLOAT (float_of_string s) }
|		'\'' ([^ '\\'] as c) '\''
					{ INT (Char.code c) }

|		space		{ scan lexbuf }
|		_ as c		{ raise (Common.LexerError (fun out -> Printf.fprintf out "unknown character '%c'" c)) }
|		eof			{ EOF }

