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
	
	let param (env, off) (t, i) =
		(add_env env i (LOCAL off), off + param_size t) in
	
	let local (env, off) t i =
		let off = (off - (sizeof t)) land (lnot ((align t) - 1)) in
		(add_env env i (LOCAL off), off) in
	
	let rec decl s (env, off) =
		match s with
		| DECLARE (t, i, s) -> decl s (local (env, off) t i)
		| SEQ (s1, s2)		
		| IF (_, s1, s2)	-> decl s2 (decl s1 (env, off))
		| WHILE (_, s)		
		| DOWHILE (s, _)	
		| SWITCH (_, s)		
		| SLINE (_, s)
		| BLOCK s			-> decl s (env, off)
		| _					-> (env, off) in
	
	let comp (code, data, env) d =
		match d with
		| NODECL ->
			(code, data, env)
		| VARDECL (_, t, i, e) ->
			(code, data @ [make_data t i e], add_env env i (GLOBAL i))
		| FUNDECL (_, FUN (rt, ps), i, s) ->
			let env', _ = fold_left param (env, 0) ps in
			let env', fsize = decl s (env', 0) in
			(code @ [LABEL i] @ (t_stmt s env') @ [RETURN], data, env)
		| FUNDECL _ ->
			failwith "Comp.decs"
		in
	
	let (code, data, _) = fold_left comp ([], [], empty_env) ds in
	(code, data)

