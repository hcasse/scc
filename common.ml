(*	
 * This file is part of SCC.
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

open Printf

(** This modules contains common types and functions used throughout
	the SCC project. They concern mainly types description, error
	management and source line management. *)

(* internals *)
let current_source = ref ""
let current_lexbuf = ref (Lexing.from_string "")

(** Thrown by the lexer when an unused character is found. *)
exception LexerError of (out_channel -> unit)


(** Thrown by the parser in case of syntax error. *)
exception SyntaxError of (out_channel -> unit)


(** Thrown when there is a semantic error. *)
exception SemanticError of (out_channel -> unit)


(** Transitional error to let ELINE/SLINE add line information. *)
exception PreError of (out_channel -> unit)


(** Raise a PreError with f as a message.
	@param f	Function displaying the error message. *)
let pre_error f =
	raise (PreError (fun out -> f out))


(** Build a lexbuf for the given file.
	@param path		Path to the source file.
	@return			Corresponding lexbuf. *)
let open_source path =
	let file = open_in path in
	let lexbuf = Lexing.from_channel file in
	current_source := path;
	current_lexbuf := lexbuf;
	lexbuf


(** Delayed display for error messages.
	@param s	String to display. *)
let prints s out =
	output_string out s


(** Delayed display for error messages.
	@param i	Integer message. *)
let printi i out =
	output_value out i


(** Delayed display for error messages.
	@param l	List of delayed functions. *)
let printm l out =
	List.iter (fun f -> f out) l


(** Display display of new line. *)
let println out =
	output_string out "\n"


(** Human-readable position of an error as
	(source line, source file, line number, first column number, last column number) *)
type pos_t = string * string * int * int *int


(** Description of a location in source file as
	(source file, first offset, last offset) *)
type loc_t = string * int * int


(** Null location. *)
let null_loc = ("", -1, -1)


(** Any location. *)
let any_loc = ("", min_int, max_int)


(** Output a location.
	@param out		Output channel.
	@param l		Location to display. *)
let output_loc out l =
	if l = null_loc then output_string out "<no loc>" else
	let (s, f, l) = l in
	Printf.fprintf out "%s:%d:%d" s f l


(** Join two locations if they can be joined (same source file).
	Else return null_loc. null_loc can be joined with any other
	location.
	@param l1	First location.
	@param l2	Second location. *)
	
let join_loc l1 l2 =
	if l1 = null_loc then l2 else
	if l2 = null_loc then l1 else
	let (s1, f1, l1) = l1 in
	let (s2, f2, l2) = l2 in
	if s1 <> s2 then any_loc else
	(s1, min f1 f2, max l1 l2)


(** Tranform a location to a position.
	@param loc		Error location
	@param pos		Position in a human readable form. *)
let loc_to_pos loc =
	let (source, first, last) = loc in
	let file = open_in source in
	let rec next n sum =
		let line = input_line file in
		let size = String.length line + 1 in
		let first_col = first - sum in
		let last_col = if sum + size < last then sum + size else last - sum in
		if sum + size > first then (line ^ "\n", source, n, first_col, last_col)
		else next (n + 1) (sum + size) in
	next 1 0


(** Print a line of source and underline between the given
	fcol and lcol.
	@param text		Source line.
	@param fcol		First column to underline.
	@param lcol		Last column to underline. *)
let print_source_line text fcol lcol out =
	output_string out (String.sub text 0 fcol);
	output_string out "\x1B[4m";
	output_string out (String.sub text fcol (lcol - fcol));
	output_string out "\x1B[24m";
	output_string out (String.sub text lcol ((String.length text) - lcol))


(** Print a message refering to a soure line position.
	@param loc	Location of the message.
	@param pref	Message prefix.
	@param msg	Function to display the message.
	@param out	Output channel to print to. *)
let print_source_msg loc pref msg out =
	if loc = null_loc then
		printm [prints pref; prints ": "; msg; println] out
	else
		let (text, source, line, fcol, lcol) = loc_to_pos loc in
		let print_pos out = Printf.fprintf out "%s: %s:%d:%d: " pref source line fcol in
		printm [print_source_line text fcol lcol; print_pos; msg; println] out


(** Delayed display of a located error.
	@param loc	Current location.
	@param msg	Message to display. *)
let error loc msg =
	print_source_msg loc "ERROR" msg stderr


(** Delayed display of a located error and stop the application.
	@param loc	Current location.
	@param msg	Message to display. *)
let fatal loc msg =
	print_source_msg loc "ERROR" msg stderr;
	exit 1


(** Display a warning message.
	@param loc	Location.
	@param msg	Message to display. *)
let warn loc msg =
	print_source_msg loc "WARNING" msg stderr


(** Create the location corresponding to the current position
	in the of the lexer.
	@return	Current location. *)
let current_loc _ =
	(!current_source, Lexing.lexeme_start !current_lexbuf, Lexing.lexeme_end !current_lexbuf)


(** Print an error for the current position.
	@param msg		Error message. *)
let print_error msg =
	msg stderr


(** Print an error and stop the parsing.
	@param msg		Message to display. *)
let print_fatal msg =
	print_error msg;
	raise Exit

(** Type to represen types. *)
type type_t =
	|	VOID
	|	INT
	|	CHAR
	|	FLOAT
	|	PTR of type_t
	|	FUN of type_t * (type_t * string) list

(** Output a type to the given channel.
	@param out	Output channel.
	@param t	Type to output. *)
let rec output_type out t =
	let outs = output_string out in
	
	let rec output_params l d =
		match l with
		| [] -> ()
		| (t, i)::l ->
			begin
				if not d then outs ", ";
				output_type out t;
				outs " ";
				outs i;
				output_params l true
			end in
	
	match t with
	| VOID ->
		outs "void"
	| INT ->
		outs "int"
	| CHAR ->
		outs "char"
	| FLOAT ->
		outs "float"
	| PTR t ->
		outs "ptr(";
		output_type out t;
		outs ")"
	| FUN (r, ps) ->
		outs "fun(";
		output_type out r;
		outs ",[";
		output_params ps false;
		outs "])"

(** Print to standard  output the given type. *)
let print_type = output_type stdout

let printt t = fun out -> output_type out t


(** Manage the support of a PreError raise when f is called. In this case,
 * display the error message prefixed with the given location. Else just
 * return the result of f.
 * @param l		Current location.
 * @param f		Function to evaluate.
 * @return		Result of f. *)
let handle_error l f =
	try
		f ()
	with PreError m ->
		error l m;
		exit 1


(** Return the power of n if n is a power of 2.
	@param n	Number to look.
	@return		m if n = 2^m, -1 else. *)
let pow_of_2 n =
	let rec f n =
		if n = 0 then -1 else
		if n land 1 = 1 then
			if n = 1 then 0 else -1
		else
			let p = f (n lsr 1) in
			if p = -1 then -1 else p + 1 in
	f n


(* easy printf output *)
let outt = output_type
let outl = output_loc


(** If true, make the compiler to stop and dump after typing pass. *)
let stop_after_typing = ref false

(** If true, stop the compiler just after syntactic analysis. *)
let stop_after_syntax = ref false


(** If true, stop the compiler just after semantic analysis. *)
let stop_after_sem = ref false

(** Output re-synthesized to C. *)
let output_c = ref false

(** Stop the compilation after translation. *)
let stop_after_trans = ref false

(** Stop the compilation after the instruction selection. *)
let stop_after_select = ref false

(** Stop the compilation after the CFG building. *)
let stop_after_cfg = ref false
