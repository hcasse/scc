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

(** Main program of the compiler. *)

open Common 

let opts = [
	("-stop-after-typing", Arg.Set stop_after_typing, "Stop compiling after typing pass.");
	("-stop-after-syntax", Arg.Set stop_after_syntax, "Stop compiling after syntactic analysis.");
	("-stop-after-sem",    Arg.Set stop_after_sem,    "Stop compiling after semantic analysis.");
	("-output-c", Arg.Set output_c, "Output re-synthetized C instead of AST")
]
let  doc = "SIAME C Compiler"

let compile path =
	try
		Parser.prog Lexer.scan (open_source path);
		print_string "Success!\n"
	with
	|	Parsing.Parse_error				-> fatal (current_loc ()) (prints "syntax error")
	|	Common.LexerError msg 			-> fatal (current_loc ()) msg
	|	Common.SyntaxError msg 			-> print_fatal msg

let _ = 
	try
		Arg.parse opts compile doc
	with Exit ->
		exit 1
