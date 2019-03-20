(** Module of performing translation from typed AST to quad IR. *)

open Ast
open Common
open List
open Printf
open Quad

(** Position of variables in memory. *)
type var =
	| NOLOC
	| LOCAL of int
	| GLOBAL of string


(** Empty environment. *)
let empty_env = fun _ -> NOLOC

(** Add a value to the environment. *)
let add_env e i l = fun i' -> if i = i' then l else e i'


(** Get value of an environment. *)
let get_env e i = e i


(** Return alignment constraint of the given type.
	Target dependent function (ARM implementation).
	@param t	Type.
	@return		Alignment constraint. *)
let align t =
	match t with
	| VOID
	| FUN _	-> failwith "Comp.align"
	| INT
	| PTR _
	| FLOAT	-> 4
	| CHAR	-> 1


(** Get the size in bytes of the given type.
	Target dependent function (ARM implementation).
	@param t	Type to get size for.
	@return		Type size in bytes. *)
let sizeof t =
	match t with
	| VOID
	| FUN _	-> failwith "Comp.align"
	| INT
	| PTR _
	| FLOAT	-> 4
	| CHAR	-> 1


(** Get the size in bytes of the given type to store it in the stack
	as a parameter.
	Target dependent function (ARM implementation).
	@param t	Type to get size for.
	@return		Type size in bytes. *)
let param_size t =
	match t with
	| VOID
	| FUN _	-> failwith "Comp.align"
	| INT
	| PTR _
	| FLOAT
	| CHAR	-> 4


(** Build a data entry.
	@param t	Data type.
	@param i	Data identifier.
	@param e	Data initial value.
	@return		Built data entry. *)
let make_data t i e =
	match t, e with
	| CHAR, NONE				-> QCHAR (i, 0)
	| CHAR, CST (_, INTV k)		-> QCHAR (i, k)
	| INT, NONE					-> QINT (i, 0)
	| INT, CST (_, INTV k)		-> QINT (i, k)
	| FLOAT, NONE				-> QFLOAT (i, 0.)
	| FLOAT, CST (_, FLOATV x)	-> QFLOAT (i, x)
	| PTR _, NONE				-> QINT (i, 0)
	| _							-> failwith "Comp.make_data"



(** Compile the given reference into quads.
	@param r	Reference to translate.
	@param env	Current environment.
	@return		(result register, generated quads). *)
let rec t_refr r env =
	match r with
	
	| RLINE (_, _, r) ->
		t_refr r env
		
	| ID (t, i) ->
		(match env i with
		| LOCAL k ->
			let v = new_tmp INT in
			let v' = new_tmp (PTR t) in
			(v', [QSETI (v, k); QADD (PTR t, v', fp, v)])
		| GLOBAL l ->
			let v = new_tmp (PTR t) in
			(v, [QSETL (v, l)])
		| _ ->
			failwith "Comp.r_ref ID")
			
	| AT (t, e) ->
		t_expr e env
		
	| _ ->
		failwith "Comp.t_r"


(** Compile the given expression into quads.
	@param e	Expression to translate.
	@param env	Current environment.
	@return		(result register, generated quads). *)
and t_expr e env =
	match e with
	(* !!TODO!! *)
	
	| ELINE (_, _, e) ->
		t_expr e env
		
	| _ ->
		failwith "Comp.t_e"


(** Compile the given expression into the quads of a condition.
	@param r	Condition expression to translate.
	@param l_t	Label to call if the condition is true.
	@param l_f	Label to call if the condition is false.
	@param env	Current environment.
	@return		Generated quads. *)
let rec t_cond e l_t l_f env =
	match e with
	(* !!TODO!! *)
	
	| ELINE (_, _, e)	->
		t_cond e l_t l_f env
		
	| _ ->
		failwith "Comp.t_c"


(** Compile the given statement into quads.
	@param s	Statement to translate.
	@param env	Current environment.
	@return		Generated quads. *)
let rec t_stmt s env =
	match s with
	(* !!TODO!! *)
	
	| NOP ->
		[]
	
	| SET (t, r, e) ->
		[]
	
	| SEQ (s1, s2) ->
		[]
	
	| IF (e, s1, s2) ->
		[]
	
	| WHILE (e, s) ->
		[]
	
	| DOWHILE (s, e) ->
		[]
	
	| RETURN (t, e) ->
		[]
	
	| BLOCK s
	| DECLARE (_, _, s)
	| SLINE (_, s) ->
		t_stmt s env
	
	| _ ->
		failwith "Comp.t_s"


(** Compile the given declaration into a compilation unit.
	@param ds	Declaration.
	@return		Compilation unit (code, data). *)
let t_d ds =
	
	let rec call_expr e =
		match e with
		| NONE
		| CST _
			-> false
		| REF (_, r)
		| ADDR (_, r)
			-> call_refr r
		| UNOP (_, _, e)
		| ELINE (_, _, e)
		| CAST (_, e)
			-> call_expr e
		| BINOP (_, _, e1, e2)
			-> (call_expr e1) || (call_expr e2)
		| ECALL _
			-> true
	
	and call_refr r =
		match r with
		| NOREF
		| ID _
			-> false
		| AT (_, e)
			-> call_expr e
		| RLINE (_, _, r)
			-> call_refr r
	
	and call_stmt s =
		match s with
		| NOP
		| CASE _
		| DEFAULT
			-> false
		| DECLARE (_, _, s)
		| SLINE (_, s)
		| BLOCK s
			-> call_stmt s
		| SEQ (s1, s2)
			-> (call_stmt s1) || (call_stmt s2)
		| IF (e, s1, s2)
			-> (call_expr e) || (call_stmt s1) || (call_stmt s2)
		| WHILE (e, s)
		| DOWHILE (s, e)
		| SWITCH(e, s)
			-> (call_expr e) || (call_stmt s)
		| SET (_, r, e)
			->  (call_refr r) || (call_expr e)
		| RETURN (_, e)
			-> call_expr e
		| CALL (e, es)
			-> true in
	
	let param (env, off, offs) (t, i) =
		(add_env env i (LOCAL off), off + param_size t, (i, off)::offs) in
	
	let local (env, off, offs) t i =
		let off = (off - (sizeof t)) land (lnot ((align t) - 1)) in
		(add_env env i (LOCAL off), off, (i, off)::offs) in
	
	let rec decl s (env, off, offs) =
		match s with
		| DECLARE (t, i, s) -> decl s (local (env, off, offs) t i)
		| SEQ (s1, s2)		
		| IF (_, s1, s2)	-> decl s2 (decl s1 (env, off, offs))
		| WHILE (_, s)		
		| DOWHILE (s, _)	
		| SWITCH (_, s)		
		| SLINE (_, s)
		| BLOCK s			-> decl s (env, off, offs)
		| _					-> (env, off, offs) in
	
	let comp (code, data, env) d =
		match d with
		| NODECL ->
			(code, data, env)
		| VARDECL (_, t, i, e) ->
			(code, data @ [make_data t i e], add_env env i (GLOBAL i))
		| FUNDECL (_, FUN (rt, ps), i, s) ->
			let do_call = call_stmt s in
			let rlab = new_lab () in
			let env = add_env env i (GLOBAL i) in
			let env', _, offs = fold_left param (env, (if do_call then 8 else 4), []) ps in
			let env', fsize, offs = decl s (env', 0, []) in
			let q =
				  [ LABEL i ]
				@ (if do_call then [QPUSH lr] else [])
				@ [
					QPUSH fp;
					QSET (INT, fp, sp);
					QSETI (zr, -fsize);
					QSUB (INT, sp, sp, zr)
				]
				@ (t_stmt s (add_env env' "$return" (GLOBAL rlab)))
				@ [
					LABEL rlab;
					QSETI (zr, -fsize);
					QADD (INT, sp, sp, zr);
					QPOP fp;
				]
				@ [if do_call then QPOP lr else RETURN] in
			(code @ [(i, fsize, q)], data, env)
		| FUNDECL _ ->
			failwith "Comp.decs"
		in
	
	let (code, data, _) = fold_left comp ([], [], empty_env) ds in
	(code, data)

