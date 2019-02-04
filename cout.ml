(* Module providing support to display back AST into C. *)

open Ast
open Common
open List
open Printf

(** Return precedence of an expression.
	@param e	Expression to get precedence of.
	@return		Expression precedence. *)
let rec prec e =
	match e with
	| NONE
	| REF _
	| CST _						-> 0
	| ECALL _					-> 1
	| CAST _
	| UNOP _
	| ADDR _ 					-> 2
	| BINOP (_, MUL, _, _)
	| BINOP (_, DIV, _, _)
	| BINOP (_, MOD, _, _)		-> 3
	| BINOP (_, ADD, _, _)
	| BINOP (_, SUB, _, _)		-> 4
	| BINOP (_, SHL, _, _)
	| BINOP (_, SHR, _, _)		-> 5
	| BINOP (_, LT, _, _)
	| BINOP (_, LE, _, _)
	| BINOP (_, GT, _, _)
	| BINOP (_, GE, _, _)		-> 6
	| BINOP (_, EQ, _, _)
	| BINOP (_, NE, _, _)		-> 7
	| BINOP (_, BIT_AND, _, _)	-> 8
	| BINOP (_, XOR, _, _)		-> 9
	| BINOP (_, BIT_OR, _, _)	-> 10
	| BINOP (_, LOG_AND, _, _)	-> 11
	| BINOP (_, LOG_OR, _, _)	-> 12
	| ELINE (_, _, e)			-> prec e

let worst_prec = 16

let op1 out op =
	output_string out
		(match op with
		| NEG	-> "- "
		| NOT	-> "!"
		| INV	-> "~")

let op2 out op =
	output_string out
		(match op with
		| ADD		-> " + "
		| SUB		-> " - "
		| MUL		-> " * "
		| DIV		-> " / "
		| MOD		-> " % "
		| EQ		-> " == "
		| NE		-> " != "
		| LT		-> " < "
		| LE		-> " <= "
		| GT		-> " > "
		| GE		-> " >= "
		| LOG_AND	-> " && "
		| LOG_OR	-> " || "
		| BIT_AND	-> " & "
		| BIT_OR	-> " | "
		| XOR		-> " ^ "
		| SHL		-> " << "
		| SHR		-> " >> ")

let clean n =
	let rec look i =
		if (String.get n i) = '$'
		then look (i - 1)
		else String.sub n 0 (i + 1) in
	look ((String.length n) - 1)

let rec type_ out t =
	match t with
	| VOID ->
		output_string out "void"
	| INT ->
		output_string out "int"
	| CHAR ->
		output_string out "char"
	| FLOAT ->
		output_string out "float"
	| PTR t ->
		type_ out t;
		output_string out " *"
	| FUN (rt, ps) ->
		type_ out rt;
		output_string out " ()(";
		ignore (fold_left (fun c (t, _) ->
			if c then output_string out ", ";
			type_ out t;
			true
		) false ps);
		output_char out ')'


let rec refr out r =
	match r with
	| NOREF				-> failwith "Cout: bad ref"
	| ID (_, i)			-> output_string out (clean i) 
	| AT (_, e)			-> output_char out '*'; expr out e 2
	| RLINE (_, _, r)	-> refr out r

and expr out e ap =
	let p = prec e in
	if ap < p then output_char out '(';
	(match e with
	| NONE
	| CST (_, NULL) ->
		failwith "Cout: bad expression."
	| CST (_, INTV k) ->
		fprintf out "%d" k
	| CST (_, FLOATV x) ->
		fprintf out "%f" x
	| REF (_, r) ->
		refr out r
	| UNOP (_, op, e1) ->
		op1 out op; expr out e1 (prec e)
	| BINOP (_, op, e1, e2) ->
		expr out e1 (prec e); op2 out op; expr out e2 ((prec e) - 1)
	| ADDR (_, r) ->
		output_char out '&'; refr out r
	| ELINE (_, _, e) -> expr out e worst_prec
	| CAST (t, e) ->
		output_char out '(';
		type_ out t; 
		output_char out ')';
		expr out e 2
	| ECALL (_, f, es) ->
		expr out f 2;
		output_char out '(';
		ignore (fold_left (fun c e ->
			if c then output_string out ", ";
			expr out e worst_prec;
			true)
		false es);
		output_char out ')');
	if ap < p then output_char out ')'

let rec is_multi s =
	match s with
	| NOP			-> false
	| DECLARE _		-> true
	| SET _			-> false
	| SEQ _			-> true
	| IF _			-> false
	| WHILE _		-> false
	| DOWHILE _		-> false
	| CASE _		-> false
	| DEFAULT		-> false
	| SWITCH _		-> false
	| SLINE (_, s)	-> is_multi s
	| BLOCK _		-> false
	| CALL _		-> false
	| RETURN _		-> false



let rec stmt_tab out t s =

	let rec indent n =
		if n = 0 then () else
		begin output_string out "  "; indent (n - 1) end in

	let sub s =
		if is_multi s then output_string out " {\n"
		else output_string out "\n";
		stmt_tab out (t + 1) s;
		if is_multi s then begin
			indent t;
			output_string out "}\n"
		end in
	
	match s with
	| NOP ->
		failwith "Cout: stmt"
	| DECLARE (tt, i, s) ->
		indent t;
		type_ out tt;
		fprintf out " %s;\n" (clean i);
		stmt_tab out t s
	| SET (_, r, e) ->
		indent t;
		refr out r;
		output_string out " = ";
		expr out e worst_prec;
		output_string out ";\n"
	| SEQ (s1, s2) ->
		stmt_tab out t s1;
		stmt_tab out t s2
	| IF (c, s1, s2) ->
		indent t;
		output_string out "if(";
		expr out c worst_prec;
		output_char out ')';
		sub s1;
		if(s2 <> NOP) then begin
			indent t;
			output_string out "else";
			sub s2;
		end
	| WHILE (e, s) ->
		indent t;
		output_string out "while(";
		expr out e worst_prec;
		output_char out ')';
		sub s
	| DOWHILE (s, e) ->
		indent t;
		output_string out "do";
		sub s;
		indent t;
		output_string out "while(";
		expr out e worst_prec;
		output_string out ");\n"
	| CASE e ->
		indent t;
		output_string out "case ";
		expr out e worst_prec;
		output_string out ":\n"
	| DEFAULT ->
		indent t;
		output_string out "default:\n"
	| SWITCH (e, s) ->
		indent t;
		output_string out "switch(";
		expr out e worst_prec;
		output_string out ") {\n";
		stmt_tab out (t + 1) s;
		indent t;
		output_string out "}\n"
	| SLINE (_, s) ->
		stmt_tab out t s
	| BLOCK s ->
		indent t;
		output_string out "{\n";
		stmt_tab out (t + 1) s;
		indent t;
		output_string out "}\n"
	| RETURN (_, e) ->
		indent t;
		output_string out "return";
		if e <> NONE then begin
			output_char out ' ';
			expr out e worst_prec;
		end;
		output_string out ";\n"
	| CALL (f, es) ->
		indent t;
		output_char out '(';
		expr out f 2;
		ignore (fold_left (fun c e ->
			if c then output_string out ", ";
			expr out e worst_prec; true) 
		false es);
		output_char out ')'

let stmt out s =
	stmt_tab out 0 s 

let decl out d =
	match d with
	| NODECL ->
		()
	| VARDECL (_, t, i, e) ->
		type_ out t;
		output_char out ' ';
		output_string out i;
		if e <> NONE then begin
			output_string out " = ";
			expr out e worst_prec
		end;
		output_string out ";\n\n"
	| FUNDECL (_, FUN(rt, ps), i, s) ->
		type_ out rt;
		fprintf out " %s(" i;
		ignore (fold_left (fun c (t, i) ->
			if c then output_string out ", ";
			type_ out t; fprintf out " %s" i;
			true)
		false ps);
		output_char out ')';
		if s = NOP then output_string out ";\n\n" else
		begin
			output_string out " {\n";
			stmt_tab out 1 s;
			output_string out "}\n\n"
		end
	| FUNDECL _ ->
		failwith "Cout: decl"

let decls out ds =
	iter (decl out) ds
