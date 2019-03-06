(** Module dedicated to the selection of instructions. *)

open Common
open List
open Printf
open Quad

(** Registers used by an instruction. *)
type argument =
	| RREG of int		(** Read-register. *)
	| WREG of int		(** Write-register. *)

(** Instruction made of its opcode and its used registers.
	Each $r in opcode is replaced by the corresponding register. *)
type inst = string * argument list


(** Program representation as a list of instructions. *)
type prog = inst list


(* Generation of instructions. *)
let op_rrr code ri rj rk = (sprintf "\t%s $r, $r, $r" code, [WREG ri; RREG rj; RREG rk])
let add ri rj rk = op_rrr "add" ri rj rk
let sub ri rj rk = op_rrr "sub" ri rj rk
(* Ajoutez ici les instructions manquantes. *)

let op_rri code ri rj k = (sprintf "\t%s $r, $r, #%d" code k, [WREG ri; RREG rj])
let addi ri rj k = op_rri "add" ri rj k
let subi ri rj k = op_rri "sub" ri rj k
(* Ajoutez ici les instructions manquantes. *)

let b lab = (sprintf "\tb %s" lab, [])
let b_cnd cnd lab = (sprintf "\tb%s %s" cnd lab, [])
let b_eq lab = b_cnd "eq" lab
(* Ajoutez ici les instructions manquantes. *)


(** Translate a data item into instructions.
	@param d	Data item to translate.
	@return		Corresponding instructions. *)
let data_item d =
	match d with
	| QCHAR (l, k)	-> [(sprintf "%s: .byte %d" l k, [])]
	| QINT (l, k)	-> [(sprintf "%s: .int %d" l k, [])]
	| QFLOAT (l, x)	-> [(sprintf "%s: .float %f" l x, [])]


(** Translate data into instructions.
	@param ds	Data to translate.
	@return		Instructions result of data translation. *)
let rec data ds =
	match ds with
	| []	-> []
	| d::ds -> (data_item d) @ (data ds)


(** Translate the given quad into quadruplets.
	@param q	Quadruplet to translate.
	@return		Corresponding instructions. *)
let quad q =
	match q with
	| QNONE 	-> []
	| LABEL l	-> [(sprintf "%s:" l, [])]
	| RETURN	-> [("\tmov pc, lr", [])]
	(** Ajoutez ici les cas manquants. *)
	| _			-> []


(** Translate quads into instructions.
	@param qs	Quads to translate.
	@return		Instructions result of quads translation. *)
let rec quads qs =
	match qs with
	| [] 	-> []
	| q::qs	-> (quad q) @ (quads qs)


(** Perform selection of instruction in the corresponding unit.
	@param unit		Unit to work on.
	@return			Corresponding list of instructions. *)
let prog unit =
	let (qs, ds) = unit in
	  [("\t.text", [])]
	@ (quads qs)
	@ [("\n\t.data", [])]
	@ (data ds)


(** Output the given instruction (replacing $r by corresponding registers).
	@param out		Output channel to use.
	@param i		Instruction to output. *)
let output_inst out i =
	let (fmt, rs) = i in
	
	let rname n =
		match n with
		| 0 -> "tmp"
		| 1 -> "pc"
		| 2 -> "sp"
		| 3 -> "fp"
		| 4 -> "r0"
		| _	 -> sprintf "r%d" n in
	
	let rec rep i rs =
		if i >= (String.length fmt) then () else
		match String.get fmt i with
		| '$'	-> esc (i + 1) rs
		| c		-> output_char out c; rep (i + 1) rs
	
	and esc i rs =
		match (String.get fmt i, rs) with
		| ('r', (WREG n)::rs)
		| ('r', (RREG n)::rs) ->
			(output_string out (rname n); rep (i + 1) rs)
		| _ ->
			failwith "Select.output_instr: unknown escape" in
	
	rep 0 rs; output_char out '\n'


(** Output the given program to the given channel.
	@param out	Output channel.
	@param prog	Program to output. *)
let output_prog out prog =
	iter (output_inst out) prog

