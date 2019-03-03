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

open Common
open Ast
open List
open Printf

(** Mode of access to the environment. *)
type env_action_t = LOCAL | GLOBAL

(** Empty environement: always return v.
	@param v	Default value.
	@return		Created environment. *)
let empty_env v = fun i m -> v

(** Create a new environment based on e where i is bound to v.
	@param e	Parent environment.
	@param i	Identifier.
	@param v	Value for v.
	@return		New environment. *)
let add_env e i v =
	fun i' m -> if i = i' then v else e i' m

(** Create a block environment: if eached by a local access,
	return the default value.
	@param e	Base environment.
	@param v	Default value for local access.
	@return		Created environment. *)
let block_env e v =
	fun i' m -> if m = LOCAL then v else e i' m

(** Access the environment on global way (not blocked by blocks).
	@param e	Environment to look in.
	@param i	Looked identifier.
	@return		Identifier value. *)
let global e i = e i GLOBAL

(** Access the environment on local way (blocked by blocks).
	@param e	Environment to look in.
	@param i	Looked identifier.
	@return		Identifier value. *)
let local e i = e i LOCAL


(** Get the value corresponding to i in the environment.
	@param e	Environment to look in.
	@param i	Looked identifier.
	@return		Found value. *)
let get_env e i = e i LOCAL


(** Ensure uniqueness of names for local variables.
	@param ds	Declarations to rename.
	@return		Declarations with unique local variable name. *)
let rec check_names ds =
	rename_decs ds (empty_env "")

and rename_decs ds env =

	let rec declare_params ps env =
		fold_left (fun e (_, i) -> add_env e i i) env ps in
	
	match ds with
	| [] 		-> []
	| h::t	->
		let (h, env) = 
			match h with
			| NODECL ->
				(h, env)
			| VARDECL (l, t, i, e)	->
				(VARDECL (l, t, i, rename_expr e env), add_env env i i)
			| FUNDECL (loc, t, i, s)	->
				match t with
				| FUN(rt, ps) ->
					(FUNDECL (loc, FUN(rt, ps), i, rename_stmt s (declare_params ps env)),
					add_env env i i)
				| _ ->
					failwith "function without function type" in
		h::(rename_decs t env)

and rename_stmt s env =

	let rec name n i env =
		if (global env n) = "" then n else
		name (n ^ "$") i env in

	let declare i env =
		if (local env i) <> ""
		then raise (PreError (fun out -> fprintf out "'%s' declared twice!" i))
		else name i i env in

	match s with
	| NOP -> NOP
	| DECLARE (t, i, s) -> let ni = declare i env in DECLARE(t, ni, rename_stmt s (add_env env i ni))
	| SET (t, r, v)		-> SET(t, rename_refr r env, rename_expr v env) 
	| SEQ (s1, s2)		-> SEQ(rename_stmt s1 env, rename_stmt s2 env)
	| IF (c, s1, s2)	-> IF(rename_expr c env, rename_stmt s1 env, rename_stmt s2 env)
	| WHILE (c, s)		-> WHILE (rename_expr c env, rename_stmt s env)
	| DOWHILE (s, c)	-> DOWHILE (rename_stmt s env, rename_expr c env)
	| CASE e			-> s
	| DEFAULT			-> s
	| SWITCH (c, s)		-> SWITCH (rename_expr c env, rename_stmt s env)
	| SLINE (l, s)		-> handle_error l (fun _ -> SLINE (l, rename_stmt s  env))
	| BLOCK s			-> BLOCK (rename_stmt s (block_env env ""))
	| CALL (f, es)		-> CALL (rename_expr f env, map (fun a -> rename_expr a env) es)
	| RETURN (t, e)		-> RETURN (t, rename_expr e env)

and rename_expr e env =
	match e with
	| NONE					-> e
	| CST _					-> e
	| REF (t, r)			-> REF (t, rename_refr r env)
	| UNOP (t, o, e)		-> UNOP (t, o, rename_expr e env)
	| BINOP (t, o, e1, e2)	-> BINOP (t, o, rename_expr e1 env, rename_expr e2 env)
	| ADDR (t, r)			-> ADDR (t, rename_refr r env)
	| ELINE (t, l, e)		-> handle_error l (fun _ -> ELINE (t, l, rename_expr e env))
	| CAST (t, e)			-> CAST (t, rename_expr e env)
	| ECALL (t, f, es)		-> ECALL (t, rename_expr f env, map (fun e -> rename_expr e env) es)

and rename_refr r env =

	let real_name env i =
		let n = global env i in
		if n = "" then raise (PreError (fun out -> fprintf out "'%s' undeclared!" i))
		else n in

	match r with
	| NOREF					-> r
	| ID (t, i)				-> ID (t, real_name env i)
	| AT (t, e)				-> AT (t, rename_expr e env)
	| RLINE (t, l, r)		-> handle_error l (fun _ -> RLINE (t, l, rename_refr r env))


(** Test if an automatic conversion exists between the given types.
	@param tt	Type to convert to.
	@param ft	Type to convert from.
	@return		True if the type ft can be automatically converted from fr to tt. *)
let auto_convert tt ft =
	(*printf "tt=%a ft=%a\n" outt tt outt ft;*)
	if tt = ft then true else
	match (tt, ft) with
	| (INT, CHAR)
	| (INT, FLOAT)
	| (CHAR, INT)
	| (CHAR, FLOAT)
	| (FLOAT, INT)
	| (FLOAT, CHAR)
	| (PTR VOID, PTR _) ->
		true
	| _ ->
		false


(** Test if an explicit conversion is allowed.
	@param tt	Type to convert to.
	@param ft	Type to convert from.
	@return		True if the explicit conversion is allowed. *)
let explicit_convert tt ft =
	(auto_convert tt ft) ||
	(match (tt, ft) with
	| (PTR _, INT)
	| (PTR _, CHAR)
	| (INT, PTR _)
	| (CHAR, PTR _)
	| _ -> false)


(** Test if the given is numeric.
	@param t	Type to test.
	@return		True if t is numeric, false else. *)
let is_numeric_type t =
	match t with
	| CHAR
	| INT
	| FLOAT	-> true
	| VOID
	| PTR _
	| FUN _	-> false


(** Give the computation type for the given type.
	@param t1	First type.
	@param t2	Second type. *)
let get_compute_type t1 t2 =
	match t1, t2 with
	| INT, INT
	| INT, CHAR
	| CHAR, INT
		-> INT
	| FLOAT, FLOAT
	| FLOAT, INT
	| FLOAT, CHAR
	| INT, FLOAT
	| CHAR, FLOAT
		-> FLOAT
	| _
		-> failwith "bad types to call compute_type"


(** Test if a type is an integer type.
	@param t	Type to test.
	@return		True if t is an integer. *)
let is_int_type t =
	match t with
	| CHAR
	| INT
		-> true
	| VOID
	| FLOAT
	| PTR _
	| FUN _
		-> false


(** Test if a type is a pointer.
	@param t	Type to test.
	@return		True if t is a pointer. *)
let is_ptr_type t =
	match t with
	| PTR _ -> true
	| _		-> false


(** Check the types of the given declarations.
	@param decs	Declaration to check type in.
	@return		Declarations with fixed type. *)
let rec check_types decs =
	type_decs decs (empty_env VOID)

and type_decs decs env =
	match decs with
	| []	->
		[]
	| h::t	->
		let (dec, env) = type_dec h env in
		dec :: (type_decs t env)

and type_dec dec env =

	let declare_params t env =
		match t with
		| FUN (rt, ps) ->
			fold_left (fun e (t, i) -> add_env e i t) (add_env env "$return" rt) ps
		| _ ->
			failwith "bad function type in type_dec" in

	match dec with
	| NODECL ->
		(NODECL, env)
	| VARDECL (l, t, i, e) ->
		let e = type_expr e env in
		let et = expr_type e in
		let e =
			if et = VOID || t = et then e else
			if auto_convert t et then CAST(t, e) else
			fatal l (fun out -> fprintf out "cannot convert initializer of type %a to %a!" outt et outt t) in
		(VARDECL (l, t, i, e), add_env env i t)
	| FUNDECL (l, t, i, s) ->
		(FUNDECL (l, t, i, type_stmt s (declare_params t env)), add_env env i t)

and type_expr e env =
	let cast tt ft e =
		if tt = ft then e else CAST(tt, e) in

	match e with

	| NONE ->
		NONE

	| CST (_, NULL) ->
		CST (VOID, NULL)
	| CST (_, INTV k) ->
		CST (INT, INTV k)
	| CST (_, FLOATV x) ->
		CST (FLOAT, FLOATV x)
	
	| REF (t, r) ->
		let r = type_refr r env in
		REF (refr_type r, r)
	
	| UNOP (_, op, e) ->
		let e = type_expr e env in
		let t = expr_type e in
		(match op with
		| NOT | INV | NEG when (is_int_type t) ->
			UNOP (INT, op, cast INT t e)
		| NEG when t = FLOAT ->
			UNOP (FLOAT, NEG, e)
		| _ ->
			raise (PreError (fun out -> fprintf out "Bad operand type!")))

	| BINOP (_, op, e1, e2) ->
		let error () =
			raise (PreError (fun out -> fprintf out "Incompatible operand types!")) in
		let e1 = type_expr e1 env in
		let e2 = type_expr e2 env in
		let t1 = expr_type e1 in
		let t2 = expr_type e2 in
		
		let rt, c1, e1, c2, e2 =
			match op with
			| ADD | SUB | MUL | DIV when (is_numeric_type t1) && (is_numeric_type t2) ->
				let t = get_compute_type t1 t2 in
				(t, t, e1, t, e2)
			| ADD when (is_ptr_type t1) && (is_int_type t2) ->
				(t1, t1, e1, INT, e2)
			| ADD when (is_int_type t2) && (is_ptr_type t2) ->
				(t2, t2, e2, INT, e1)
			| SUB when (is_ptr_type t1) && (is_int_type t2) ->
				(t1, t1, e1, INT, e2)
			| SUB when (is_ptr_type t1) && t1 = t2 && t1 <> PTR VOID ->
				(t1, t1, e1, t1, e2)
			| MOD | SHL | SHR | LOG_AND | LOG_OR | BIT_AND | BIT_OR | XOR when (is_int_type t1) && (is_int_type t2) ->
				(INT, INT, e1, INT, e2)
			| EQ | NE | LT | LE | GT | GE when (is_numeric_type t1) && (is_numeric_type t2) ->
				let t = get_compute_type t1 t2 in
				(INT, t, e1, t, e2)
			| EQ | NE | LT | LE | GT | GE when (is_ptr_type t1) && t1 = t2 ->
				(INT, t1, e1, t1, e2)
			| _
				-> error () in
		
		BINOP (rt, op, cast c1 t1 e1, cast c2 t2 e2)
	
	| ELINE (_, l, e) ->
		handle_error l (fun _ ->
			let e = type_expr e env in
			ELINE (expr_type e, l, e))
	
	| ADDR (_, r) ->
		let r = type_refr r env in
		ADDR (PTR (refr_type r), r)
	
	| CAST (tt, e) ->
		let e = type_expr e env in
		let ft = expr_type e in
		if explicit_convert tt ft then CAST(tt, e) else
		raise (PreError (fun out -> fprintf out "Illegal conversion!"))
	
	| ECALL (_, f, es) ->
		let f = type_expr f env in
		(match expr_type f with
		| FUN(rt, ps) when rt <> VOID ->
			ECALL(rt, f, type_args ps es env)
		| FUN _ ->
			raise (PreError (fun out -> fprintf out "Cannot use function returning void in expression"))
		| _ ->
			raise (PreError (fun out -> fprintf out "Cannot call a non-function value!")))

and type_stmt s env =

	let cond e =
		let e = type_expr e env in
		let t = expr_type e in
		if t = INT then e else
		if auto_convert INT t then CAST(INT, e)
		else raise (PreError (fun out -> fprintf out "Bad condition type!")) in

	match s with
	| NOP ->
		NOP
		
	| DECLARE (t, i, s)	->
		(* printf "declare %s: %a\n" i outt t; *)
		let r = DECLARE (t, i, type_stmt s (add_env env i t)) in
		(* printf "end declare %s: %a\n" i outt t; *)
		r
	
	| SET (_, r, e) ->
		let e = type_expr e env in
		let et = expr_type e in
		let r = type_refr r env in
		let rt = refr_type r in
		if et = rt then SET(rt, r, e) else
		if auto_convert rt et then SET(rt, r, CAST(rt, e)) else
		pre_error (fun out -> fprintf out "Type error in assignment.")
	
	| SEQ (s1, s2) ->
		SEQ (type_stmt s1 env, type_stmt s2 env)
		
	| IF (e, s1, s2) ->
		IF (cond (type_expr e env), type_stmt s1 env, type_stmt s2 env) 
	
	| WHILE (e, s) ->
		WHILE (cond (type_expr e env), type_stmt s env)
	
	| DOWHILE (s, e) ->
		DOWHILE (type_stmt s env, cond (type_expr e env))
	
	| CASE e ->
		CASE (type_expr e env)
	
	| DEFAULT ->
		DEFAULT
	
	| SWITCH (e, s) ->
		SWITCH (cond (type_expr e env), type_stmt s env)
	
	| SLINE (l, s) ->
		handle_error l (fun () -> SLINE (l, type_stmt s env))
	
	| BLOCK s ->
		BLOCK (type_stmt s env)
	
	| CALL (e, es) ->
		let e = type_expr e env in
		(match expr_type e with
		| FUN (rt, ps) ->
			CALL(e, type_args ps es env)
		| _ ->
			raise (PreError (fun out -> fprintf out "Cannot call this value.")))

	| RETURN (_, e) ->
		let e = type_expr e env in
		let t = expr_type e in
		let ft = get_env env "$return" in
		if t = ft then RETURN (t, e) else
		if auto_convert ft t then RETURN (ft, CAST(ft, e))
		else pre_error (fun out -> fprintf out "cannot convert to function return type")


and type_refr r env =
	match r with
	| NOREF				-> NOREF
	| ID (_, i) 		->
		(*printf "%s: %a\n" i outt (get_env env i);*)
		ID(get_env env i, i)
	| AT (_, e)			-> let e = type_expr e env in AT (expr_type e, e)
	| RLINE (_, l, r)	-> handle_error l (fun _ -> let r = type_refr r env in RLINE (refr_type r, l, r)) 


and type_args ps es env =
	match (ps, es) with
	| [], [] ->
		[]
	| (t, i)::ps, e::es ->
		let e = type_expr e env in
		let et = expr_type e in
		if t = et then e :: (type_args ps es env) else
		if auto_convert t et then (CAST (t, e)) :: (type_args ps es env) else
		raise (PreError (fun out -> fprintf out "Bad argument type!"))
	| [], _ ->
		raise (PreError (fun out -> fprintf out "Too many arguments!"))
	| _, [] ->
		raise (PreError (fun out -> fprintf out "Not enough arguments!"))

