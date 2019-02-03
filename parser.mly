/*	
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
 */
%{
open Common
open Ast

let loc f l		= (!current_source, f, l)
let eline l e	= ELINE (VOID, l, e)
let sline l s	= SLINE (l, s)
let rline l r	= RLINE (VOID, l, r)

let rec set_type bt t =
	match t with
	| VOID -> bt
	| PTR t -> PTR (set_type bt t)
	| FUN (t, ps) -> FUN(set_type bt t, ps)
	| _ -> failwith "no base type to set"

let rec make_decs bt decs s =

	let make t i d s =
		let s = if d = NONE
			then s
			else SEQ(SET(VOID, ID(VOID, i), d), s) in
		DECLARE(set_type bt t, i, s) in

	match decs with
	| [] -> NOP
	| [(t, i, d)]	-> make t i d s
	| (t, i, d)::n	-> make t i d (make_decs bt n s)

let check decs =
	let decs = Sem.check_names decs in
	let decs = Sem.check_types decs in
	let decs = Cst.check decs in
	decs
%}

%token<string> ID
%token<int> INT
%token<Common.type_t> TYPE
%token<float> FLOAT

%token EQ
%token EQ_EQ EXCLAM_EQ LT LT_EQ GT GT_EQ
%token PLUS MINUS STAR SLASH PERCENT
%token LT_LT GT_GT
%token LPAR RPAR LBRACE, RBRACE
%token AND_AND PIPE_PIPE EXCLAM AND
%token COM
%token SEMI
%token COL
%token EOF

%token CASE
%token DEFAULT
%token DO
%token ELSE
%token FOR
%token IF
%token SWITCH
%token WHILE

%token PLUS_PLUS
%token MINUS_MINUS

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE
%nonassoc EXCLAM
%nonassoc AND

%left EQ
%left AND_AND PIPE_PIPE
%left EQ_EQ EXCLAM_EQ LT LT_EQ GT GT_EQ
%left LT_LT GT_GT
%left PLUS MINUS
%left STAR SLASH PERCENT

%type <unit> prog
%start prog

%%

prog:	opt_defs EOF
	{ List.iter print_decl (check $1) }
;

opt_defs:	/* empty */
	{ [] }
|			defs
	{ $1 }
;

defs:	def
	{ $1 }
|		defs def
	{ $1 @ $2 }
;

def:
	SEMI
		{ [] }
|	TYPE left decls right SEMI
		{ List.map (fun (t, n, e) -> VARDECL (loc $2 $4, set_type $1 t, n, e)) $3 }
|	TYPE left decl right LBRACE opt_stmts RBRACE
		{ let (t, n, _) = $3 in [FUNDECL (loc $2 $4, set_type $1 t, n, $6)] }
;

opt_params:	/* empty */
	{ [] }
|			params
	{ $1 }
;

params:	params COM param
	{ $3::$1 }
|		param
	{ [$1] }
;

param:	TYPE param_decl
	{ let (t, i) = $2 in (set_type $1 t, i) }
;

param_decl:	ID
	{ (VOID, $1) }
|			STAR param_decl
	{ let (t, i) = $2 in (PTR t, i) }
;

opt_stmts:	/* empty */
	{ NOP }
|			stmts
	{ $1 }
;

stmts:
	stmt
		{ $1 }
|	stmt stmts
		{ SEQ($1, $2) }
|	TYPE left decls right SEMI stmts
		{ sline (loc $2 $4) (make_decs $1 $3 $6) }
;

stmt:
	SEMI
		{ NOP }
|	side_effect SEMI
		{ $1 }
|	IF left LPAR expr RPAR stmt	%prec LOWER_THAN_ELSE
		{ sline (join_loc (loc $2 $2) (stmt_loc $6)) (IF ($4, $6, NOP)) }
|	IF left LPAR expr RPAR stmt ELSE stmt
		{ sline (join_loc (loc $2 $2) (stmt_loc $8)) (IF ($4, $6, $8)) }
|	WHILE left LPAR expr RPAR stmt
		{ sline (join_loc (loc $2 $2) (stmt_loc $6)) (WHILE($4, $6)) }
|	CASE left expr COL right
		{ sline (loc $2 $5) (CASE $3) }
|	DEFAULT left COL right
		{ sline (loc $2 $4) DEFAULT }
|	SWITCH left LPAR expr RPAR stmt
		{ sline (join_loc (loc $2 $2) (stmt_loc $6)) (SWITCH($4, $6)) }
|	LBRACE opt_stmts RBRACE
		{ BLOCK $2 }
|	DO left stmt WHILE LPAR expr RPAR right
		{ sline (loc $2 $8) (DOWHILE($3, $6)) }
|	FOR LPAR opt_side_effect SEMI opt_expr SEMI opt_side_effect RPAR stmt
		{ SEQ($3, WHILE($5, SEQ($9, $7))) }
;	

opt_side_effect:
	/* empty */
		{ NOP }
|	side_effect
		{ $1 }
;

side_effect:
	refr EQ expr right
		{ sline (join_loc (refr_loc $1) (loc $4 $4)) (SET(VOID, $1, $3)) }
|	refr PLUS_PLUS right
		{ sline (join_loc (refr_loc $1) (loc $3 $3))
			(SET(VOID, $1, BINOP(VOID, ADD, REF(VOID, $1), CST(VOID, INTV(1))))) }
|	refr MINUS_MINUS right
		{ sline (join_loc (refr_loc $1) (loc $3 $3))
			(SET(VOID, $1, BINOP(VOID, SUB, REF(VOID, $1), CST(VOID, INTV(1))))) }
;

decls: decl
	{ [$1] }
|			decls COM decl
	{ $3::$1 }
;

decl:	ID
	{ (VOID, $1, NONE) }
|		ID EQ expr
	{ (VOID, $1, $3) }
|		STAR decl
	{ 	let (t, i, e) = $2 in
		match t with
		| FUN(t, ps) -> (FUN(PTR(t), ps), i, e)
		| _ -> (PTR t, i, e)
	}
|		ID LPAR opt_params RPAR
	{ (FUN(VOID, $3), $1, NONE) }
;

opt_expr:	/* empty */
	{ NONE }
|			expr
	{ $1 }
;



expr:
	simple_expr
		{ $1 }
|	PLUS expr
		{ $2 }
|	expr PLUS expr
		{ BINOP(VOID, ADD, $1, $3) }
|	expr MINUS expr
		{ BINOP(VOID, SUB, $1, $3) }
|	expr STAR expr
		{ BINOP(VOID, MUL, $1, $3) }
|	expr SLASH expr
		{ BINOP(VOID, DIV, $1, $3) }
|	expr PERCENT expr
		{ BINOP(VOID, MOD, $1, $3) }
|	expr EQ_EQ expr
		{ BINOP(VOID, EQ, $1, $3) }
|	expr EXCLAM_EQ expr
		{ BINOP(VOID, NE, $1, $3) }
|	expr LT expr
		{ BINOP(VOID, LT, $1, $3) }
|	expr LT_EQ expr
		{ BINOP(VOID, LE, $1, $3) }
|	expr GT expr
		{ BINOP(VOID, GT, $1, $3) }
|	expr GT_EQ expr
		{ BINOP(VOID, GE, $1, $3) }
|	expr AND_AND expr
		{ BINOP(VOID, LOG_AND, $1, $3) }
|	expr PIPE_PIPE expr
		{ BINOP(VOID, LOG_OR, $1, $3) }
|	expr LT_LT expr
		{ BINOP(VOID, SHL, $1, $3) }
|	expr GT_GT expr
		{ BINOP(VOID, SHR, $1, $3) }
;

simple_expr:
	INT left right
		{ eline (loc $2 $3) (CST (VOID, INTV $1)) }
|	FLOAT left right
		{ eline (loc $2 $3) (CST (VOID, FLOATV $1)) }
|	refr
		{ REF(VOID, $1) }
|	LPAR left expr RPAR right
		{ eline (loc $2 $5) $3 }
|	AND left refr
		{ eline (join_loc (loc $2 $2) (refr_loc $3)) (ADDR (VOID, $3)) }
|	LPAR left free_type RPAR simple_expr
		{ ELINE(VOID, join_loc (!current_source, $2, $2) (expr_loc $5), CAST($3, $5)) }
|	EXCLAM left simple_expr
		{ ELINE(VOID, join_loc (!current_source, $2, $2) (expr_loc $3), UNOP (VOID, NOT, $3)) }
|	MINUS left simple_expr
		{ ELINE(VOID, join_loc (!current_source, $2, $2) (expr_loc $3), UNOP (VOID, NEG, $3)) }
;

refr:
	ID left right
		{ rline (loc $2 $3) (ID (VOID, $1)) }
|	STAR left expr
		{ rline (join_loc (loc $2 $2) (expr_loc $3)) (AT (VOID, $3)) }
;

free_type:
	TYPE
		{ $1 }
|	free_type STAR 
		{ PTR $1 }
;

left:	/* empty */
	{ Lexing.lexeme_start !current_lexbuf }
;

right:	/* empty */
	{ Lexing.lexeme_end !current_lexbuf }
;
