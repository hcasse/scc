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

(** This modules describes the AST and provides basic functions to
	manage them. *)

open Common
open List

let display_all = false

type unop =
	| NOT
	| NEG
	| INV

type binop =
	| ADD
	| SUB
	| MUL
	| DIV
	| MOD
	| EQ
	| NE
	| LT
	| LE
	| GT
	| GE
	| LOG_AND
	| LOG_OR
	| BIT_AND
	| BIT_OR
	| XOR
	| SHL
	| SHR

type stmt =
	| NOP
	| DECLARE of type_t * string * stmt
	| SET of type_t * refr * expr
	| SEQ of stmt * stmt
	| IF of expr * stmt *stmt
	| WHILE of expr * stmt
	| DOWHILE of stmt * expr
	| CASE of expr
	| DEFAULT
	| SWITCH of expr * stmt
	| SLINE of loc_t * stmt
	| BLOCK of stmt
	| CALL of expr * expr list
	| RETURN of type_t * expr
	
and expr =
	| NONE
	| CST of type_t * const
	| REF of type_t * refr
	| UNOP of type_t * unop * expr
	| BINOP of type_t * binop * expr * expr
	| ADDR of type_t *refr
	| ELINE of type_t * loc_t * expr
	| CAST of type_t * expr
	| ECALL of type_t * expr * expr list

and const =
	| NULL
	| INTV of int
	| FLOATV of float

and refr =
	| NOREF
	| ID of type_t * string
	| AT of type_t * expr
	| RLINE of type_t * loc_t * refr

type decl =
	| NODECL
	| VARDECL of loc_t * type_t * string * expr
	| FUNDECL of loc_t * type_t * string * stmt


let outs = output_string
let print = Printf.fprintf

let rec output_refr out r =
	match r with
	| NOREF ->
		outs out "NO_REF"
	| ID (t, i) ->
		outs out "ID(";
		output_type out t;
		print out ", \"%s\")" i
	| AT (t, e) ->
		output_string out "AT(";
		output_type out t;
		output_string out ", ";
		output_expr out e;
		output_string out ")"
	| RLINE (_, _, r) ->
		output_refr out r

and output_const out c =
	match c with
	| NULL		-> output_string out "null"
	| INTV i	-> print out "%d" i
	| FLOATV x	-> print out "%f" x

and output_expr out e =
	match e with
	| NONE ->
		outs out "NONE"
	| CST (t, c) ->
		outs out "CST(";
		output_type out t;
		outs out ", ";
		output_const out c;
		outs out ")" 
	| REF (t, r) ->
		outs out "REF(";
		output_type out t;
		outs out ", ";
		output_refr out r;
		outs out ")"
	| UNOP (t, op, e) ->
		outs out "UNOP(";
		output_type out t;
		outs out ", ";
		output_unop out op;
		outs out ", ";
		output_expr out e;
		outs out ")"
	| BINOP (t, op, e1, e2) ->
		outs out "BINOP(";
		output_type out t;
		outs out ", ";
		output_binop out op;
		outs out ", ";
		output_expr out e1;
		outs out ", ";
		output_expr out e2;
		outs out ")"
	| ADDR (t, e) ->
		outs out "ADDR(";
		output_type out t;
		outs out ", ";
		output_refr out e;
		outs out ")"
	| ELINE (_, l, e) ->
		if display_all then (outs out "ELINE("; output_loc out l; outs out ", ");
		output_expr out e;
		if display_all then outs out ")"
	| CAST(t, e) ->
		outs out "CAST(";
		output_type out t;
		outs out ", ";
		output_expr out e;
		outs out ")"
	| ECALL (t, e, es) ->
		outs out "ECALL(";
		output_type out t;
		outs out ", ";
		output_expr out e;
		outs out ", [";
		ignore (List.fold_left (fun s e -> outs out s; output_expr out e; ", ") "" es);
		outs out "])"
	
and output_unop out op =
	outs out
		(match op with
		| NOT -> "NOT"
		| NEG -> "NEG"
		| INV -> "INV")

and output_binop out op =
	outs out
		(match op with
		| ADD		-> "ADD"
		| SUB 		-> "SUB"
		| MUL		-> "MUL"
		| DIV		-> "DIV"
		| MOD		-> "MOD"
		| EQ		-> "EQ"
		| NE		-> "NE"
		| LT		-> "LT"
		| LE		-> "LE"
		| GT		-> "GT"
		| GE		-> "GE"
		| LOG_AND	-> "LOG_AND"
		| LOG_OR	-> "LOG_OR"
		| BIT_AND	-> "BIT_AND"
		| BIT_OR	-> "BIT_OR"
		| XOR		-> "XOR"
		| SHL		-> "SHL"
		| SHR		-> "SHR")

and output_stmt out s = output_s out 0 s

and output_s out t s =
	let rec printt n =
		if n = 0 then () else begin outs out "  "; printt (n - 1) end in
	let p = outs out in
	let pi = printt t; p in
	let pe = printt (t + 1); output_expr out in
	let ps = output_s out (t + 1) in
	
	match s with
	| NOP ->
		pi "NOP"
	| DECLARE (tt, i, s) ->
		pi "DECLARE(";
		output_type out tt;
		print out ", \"%s\",\n" i;
		output_s out t s;
		p ")"
	| SET (t, r, e) ->
		pi "SET(";
		output_type out t;
		print out ", ";
		output_refr out r;
		print out ", ";
		output_expr out e;
		print out ")"
	| SEQ (s1, s2) ->
		pi "SEQ(\n";
		ps s1;
		p ",\n";
		ps s2;
		p ")"
	| IF (e, s1, s2) ->
		pi "IF(\n";
		pi "";
		pe e;
		p ",\n";
		ps s1;
		p ",\n";
		ps s2;
		p ")"
	| WHILE (e, s) ->
		pi "WHILE(\n";
		pi "";
		pe e;
		p ",\n";
		ps s;
		p ")"
	| DOWHILE (s, e) ->
		pi "DOWHILE(\n";
		ps s;
		p ",\n";
		pi "";
		pe e;
		p ")"
	| CASE e ->
		pi "CASE(";
		pe e;
		p ")"
	| DEFAULT ->
		pi "DEFAULT"
	| SWITCH (e, s) ->
		pi "SWITCH(\n";
		pe e;
		p ",\n";
		ps s;
		p ")"
	| SLINE (l, s) ->
		if display_all then (pi "SLINE("; output_loc out l; p ",\n");
		output_s out t s;
		if display_all then p ")"
	| BLOCK s ->
		pi "BLOCK(\n";
		output_s out t s;
		p ")"
	| CALL (e, es) ->
		outs out "CALL(";
		output_expr out e;
		outs out ", [";
		ignore (List.fold_left (fun s e -> outs out s; output_expr out e; ", ") "" es);
		outs out "])"
	| RETURN (t, e) ->
		outs out "RETURN(";
		output_type out t;
		outs out ", ";
		output_expr out e;
		outs out ")"
	
	
let output_decl out d =
	let p = outs out in
	match d with
	| NODECL ->
		p "NULL"
	| VARDECL(_, t, n, e) ->
		p "VARDECL(";
		output_type out t;
		p ", ";
		p n;
		p ", ";
		output_expr out e;
		p ")\n\n"
	| FUNDECL (_, t, n, s) ->
		p "FUNDECL(";
		output_type out t;
		p ", ";
		p n;
		p ",\n";
		output_stmt out s;
		p ")\n\n"
		
let print_stmt = output_stmt stdout
let print_expr = output_expr stdout
let print_refr = output_refr stdout
let print_decl = output_decl stdout


(** Build an expression line from the current position in lexbuf.
	@param e	Expression corresponding to the line. *)
(*let expr_line e =
	ELINE (VOID, ("", Lexing.lexeme_start !lexbuf, Lexing.lexeme_end !lexbuf), e)*)


(** Find the location corresponding to the given expression.
	@param e	Expression to look location for.
	@return		Corresponding location or null_loc. *)
let rec expr_loc e =
	match e with
	| NONE
	| CST _
	| REF _
	| ADDR _
	| ECALL _ ->
		null_loc
	| UNOP (_, _, e) 
	| CAST(_, e) ->
		expr_loc e
	| BINOP (_, _, e1, e2) ->
		let (s, f, _) = expr_loc e1 in 
		let (_, _, l) = expr_loc e2 in 
		(s, f, l)
	| ELINE (_, l, _) ->
		l

(** Find the location information for a reference.
	@param r	Reference to look in.
	@return		Corresponding location or null_loc. *)
let rec refr_loc r =
	match r with
	| NOREF
	| AT _
	| ID _				-> null_loc
	| RLINE (_, l, _)	-> l


(** Find the location information for a statement.
	@param s	Statement to look in.
	@return		Corresponding location or null_loc. *)
let rec stmt_loc s =
	match s with
	| NOP				-> null_loc
	| DECLARE (_, _, s)	-> stmt_loc s
	| SET _				-> null_loc
	| SEQ (s1, s2)		-> join_loc (stmt_loc s1) (stmt_loc s2)
	| IF (e, s1, s2)	-> join_loc (expr_loc e) (join_loc (stmt_loc s1) (stmt_loc s2))
	| WHILE (e, s)		-> join_loc (expr_loc e) (stmt_loc s)
	| DOWHILE (s, e)	-> join_loc (stmt_loc s) (expr_loc e)
	| CASE e			-> expr_loc e
	| DEFAULT			-> null_loc
	| SWITCH (e, s)		-> join_loc (expr_loc e) (stmt_loc s)
	| SLINE (l, _)		-> l
	| BLOCK s			-> stmt_loc s
	| CALL (e, es)		-> fold_left (fun l e -> join_loc l (expr_loc e)) (expr_loc e) es
	| RETURN _			-> null_loc


(** Generate error message including error information.
	@param e	Expression wher the error applies.
	@param msg	Message of the error. *)
let expr_error e msg out =
	error (expr_loc e) msg


(** Get the type of an expression.
	@param e	Looked expression.
	@return		Expression type. *)
let expr_type e =
	match e with
	| NONE -> VOID
	| CST (t, _)
	| REF (t, _)
	| UNOP (t, _, _)
	| BINOP (t, _, _, _)
	| ADDR (t, _)
	| ELINE (t, _, _)
	| ECALL (t, _, _)
	| CAST (t, _) -> t

(** Get the type of a reference.
	@param r	Looked reference.
	@return		Reference type. *)
let refr_type r =
	match r with
	| NOREF				-> VOID
	| ID (t, _)
	| AT (t, _)
	| RLINE (t, _, _)	-> t
	
(* deprecated *)
let prints s = fun out -> output_stmt out s
let printe e = fun out -> output_expr out e
let printr r = fun out -> output_refr out r

(* easy %a printf output *)
let outs = output_stmt
let oute = output_expr
let outr = output_refr

