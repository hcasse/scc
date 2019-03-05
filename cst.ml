(* TP 1 *)

open Common
open Ast
open List
open Printf
open Sem

(* Vérification d'initialisation de constante *)

let is_cst e =
	true


(** Evalue l'expression passée en paramètre.
	@param e	Expression à évaluer.
	@param env	Environnement courant.
	@return		Expression constante. *)
let rec eval_env e env =
	let mint k = CST (INT, INTV k) in
	let mflt x = CST (FLOAT, FLOATV x) in

	let rec eval_ref r =
		match r with
		| ID (_, i) ->
			let v = get_env env i in
			if v = NONE then failwith "référence non constante (1)"
			else v
		| _ -> failwith "référence non constante (2)" in

	match e with
	| NONE
	| CST _	->
		e
	| ADDR _
	| ECALL _ ->
		failwith "expression non constante"
	| ELINE (_, _, e) ->
		eval_env e env
	| REF (_, r) ->
		eval_ref r
	
	| UNOP (_, op, e) ->
		(match (op, eval_env e env) with
		| (NEG, CST (INT, INTV k))		-> mint (-k)
		| (NEG, CST (FLOAT, FLOATV x))	-> mflt (-.x)
		| (INV, CST (INT, INTV k)) 		-> mint (lnot k)
		| (NOT, CST (INT, INTV k)) 		-> mint (if k = 0 then 1 else 0)
		| _ -> failwith "erreur interne 1")

	| CAST(t, e) ->
		let e = eval_env e  env in
		(match (t, e) with
		| INT, CST (INT, INTV k)		-> e
		| CHAR, CST (INT, INTV k)		-> mint (k land 0xff)
		| FLOAT, CST (INT, INTV k)		-> mflt (float k)
		| INT, CST (FLOAT, FLOATV x)	-> mint (int_of_float x)
		| CHAR, CST (FLOAT, FLOATV x)	-> mint ((int_of_float x) land 0xff)
		| FLOAT, CST (FLOAT, FLOATV x)	-> e
 		| _ -> failwith "erreur interne 2")

	| BINOP (_, op, e1, e2) ->
		(match (op, eval_env e1 env, eval_env e2 env) with
		| ADD, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (k1 + k2)
		| ADD, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mflt (x1 +. x2)
		| SUB, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (k1 - k2)
		| SUB, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mflt (x1 -. x2)
		| MUL, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (k1 * k2)
		| MUL, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mflt (x1 *. x2)
		| DIV, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (k1 / k2)
		| DIV, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mflt (x1 /. x2)
		| MOD, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (k1 mod k2)
		| SHL, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (k1 lsl k2)
		| SHR, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (k1 asr k2)
		| BIT_AND, CST (INT, INTV k1), CST (INT, INTV k2)		-> mint (k1 land k2)
		| BIT_OR, CST (INT, INTV k1), CST (INT, INTV k2)		-> mint (k1 lor k2)
		| XOR, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (k1 lxor k2)
		| EQ, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (if k1 = k2 then 1 else 0)
		| EQ, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mint (if x1 = x2 then 1 else 0)
		| NE, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (if k1 <> k2 then 1 else 0)
		| NE, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mint (if x1 <> x2 then 1 else 0)
		| LT, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (if k1 < k2 then 1 else 0)
		| LT, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mint (if x1 < x2 then 1 else 0)
		| LE, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (if k1 <= k2 then 1 else 0)
		| LE, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mint (if x1 <= x2 then 1 else 0)
		| GT, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (if k1 > k2 then 1 else 0)
		| GT, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mint (if x1 > x2 then 1 else 0)
		| GE, CST (INT, INTV k1), CST (INT, INTV k2)			-> mint (if k1 >= k2 then 1 else 0)
		| GE, CST (FLOAT, FLOATV x1), CST (FLOAT, FLOATV x2)	-> mint (if x1 >= x2 then 1 else 0)
		| LOG_AND, CST (INT, INTV k1), CST (INT, INTV k2)		-> mint (if k1 <> 0 && k2 <> 0 then 1 else 0)
		| LOG_OR, CST (INT, INTV k1), CST (INT, INTV k2)		-> mint (if k1 = 0 && k2 = 0 then 0 else 1)
		| _ -> failwith "erreur interne 3")



(* Réduction des expressions constantes *)

(** Evalue l'expression passée en paramètre.
	@param e	Expression à évaluer.
	@return		Expression constante. *)
let eval e =
	eval_env e (empty_env NONE)

let rec reduce_stmt s =
	s

and reduce_expr e =
	e

and reduce_refr r =
	r


(* Réduction des expressions constantes avec variables constantes *)

let reduce_env s env =
	s


(* entrée de vérification *)
let check decs =
	iter (fun d ->
		match d with
		| NODECL ->
			()
		| FUNDECL _ ->
			()
		| VARDECL (l, t, i, e) ->
			if not (is_cst e)
			then fatal l (fun out -> fprintf out "should be constant!")
	) decs;
	let decs = map (fun d ->
		match d with
		| NODECL ->
			d
		| VARDECL (l, t, i, e) ->
			VARDECL (l, t, i, eval e)
		| FUNDECL (l, t, i, s) ->
			FUNDECL (l, t, i, reduce_stmt s)
	) decs in
	map (fun d ->
		match d with
		| NODECL
		| VARDECL _ ->
			d
		| FUNDECL (l, t, i, s) ->
			FUNDECL (l, t, i, reduce_env s (empty_env NONE))
	) decs
		

