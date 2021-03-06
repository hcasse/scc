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

let push vi = ("\tstr $r, [sp, #-4]!", [RREG vi])
let pop vi = ("\tldr $r, [sp], #4", [WREG vi])
let stmfd_sp rs =
	(sprintf "\tstmfd sp!, {%s}" (fold_left (fun s r -> if s = "" then "$r" else s ^ ", $r") "" rs),
	map (fun r -> RREG r) rs)
let ldmfd_sp rs =
	(sprintf "\tldmfd sp!, {%s}" (fold_left (fun s r -> if s = "" then "$r" else s ^ ", $r") "" rs),
	map (fun r -> WREG r) rs)

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
	| QPUSH vi	-> [push vi]
	| QPOP vi	-> [pop vi]
	(** Ajoutez ici les cas manquants. *)
	| _			-> []


(** Aggregate multiple pushes in one stm.
	@param qs	Quads to process.
	@param rs	Aggregated registers.
	@return		Instructions. *)
let rec make_stm qs rs =
	match qs with
	| (QPUSH r)::qs
		-> make_stm qs (r::rs)
	| _
		-> (stmfd_sp rs)::(quads qs)

(** Aggregate multiple pops in one stm.
	@param qs	Quads to process.
	@param rs	Aggregated registers.
	@return		Instructions. *)
and make_ldm qs rs =
	match qs with
	| (QPOP r)::qs
		-> make_ldm qs ((if r = lr then pc else r)::rs)
	| _
		-> (ldmfd_sp rs)::(quads qs)

(** Translate quads into instructions.
	@param qs	Quads to translate.
	@return		Instructions result of quads translation. *)
and quads qs =
	match qs with
	| [] -> 
		[]
	| (QPUSH v)::qs ->
		make_stm qs [v]
	| (QPOP v)::qs ->
		make_ldm qs [v]
	| q::qs	->
		(quad q) @ (quads qs)


(** Perform selection of instruction in the corresponding unit.
	@param unit		Unit to work on.
	@return			Corresponding list of instructions. *)
let prog unit =
	let (fs, ds) = unit in
	(
		map (fun (i, qs) -> (i, quads qs)) fs,
		data ds
	)


(** Output the given instruction (replacing $r by corresponding registers).
	@param out		Output channel to use.
	@param i		Instruction to output. *)
let output_inst out i =
	let (fmt, rs) = i in
	
	let rname n =
		match n with
		| 11 -> "tmp"
		| 15 -> "pc"
		| 13 -> "sp"
		| 12 -> "fp"
		| 14 -> "lr"
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


(** Output the given list of instructions.
	@param out		Output channel.
	@param insts	Instructions to output. *)
let output_insts out insts =
	iter (output_inst out) insts

(** Output the given program to the given channel.
	@param out	Output channel.
	@param prog	Program to output. *)
let output_prog out prog =
	let (fs, ds) = prog in
	output_inst out ("\t.text", []);
	iter (fun (_, _, qs) -> iter (output_inst out) qs) fs;
	output_inst out ("\n\t.data", []);
	iter (output_inst out) ds


(** Select machine instructions in the given CFG.
	@param g	Quad CFG to select instructions in.
	@return		Instruction CFG after selection. *)
let cfg g =
	let (i, l, g) = g in
	(i, l, Cfg.replace (fun i v -> quads v) g)
