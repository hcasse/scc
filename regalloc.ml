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

(** This module performs register allocation. *)

open List
open Printf

(** Get the position of a virtual register.
	(real register, in stack) *)
type local = int * int


(** context = (
		r: real register free,
		sigma: map between virtual and ream register,
		m: modified virtual registers,
		l: local memory size) *)
type context = int list * (int * local) list * int list * int


(** Code to represent no registers. *)
let no_reg = -1


(** Registers available on ARM *)
let free_regs = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]


(** Creates a new empty context with all free registers.
	@param l	Size of local memory area.
	@return		New allocation context. *)
let new_context l = (free_regs, [], [], l)


(** Try to allocate a new register.
	@param ctx	Current context.
	@param v	Virtual register to allocate to a real register.
	@return		(allocated register, new context). *)
let alloc_reg ctx v =
	let (f, s, m, l) = ctx in
	match f with
	| [] 	-> (-1, ctx)
	| h::t	-> (h, (t, (v, (h, -1))::s, m, l))


(** Get the real register containing the given virtual.
	@param ctx	Current context.
	@param v	Virtual register.
	@return		Real register or -1. *)
let get_reg ctx v =
	let (_, s, _, _) = ctx in
	match assoc_opt v s with
	| None			-> -1
	| Some (r, _)	-> r


(** Get offset in local memory to store virtual register v.
	If not allocated, allocate it.
	@param ctx	Current context.
	@param v	Virtual register.
	@return		(frame offset, new context). *)
let get_offset ctx v =
	let (f, s, m, l) = ctx in
	match assoc_opt v s with
	| None ->
		let l = l + 4 in
		(-l, (f, (v, (-1, -l))::(remove_assoc v s), m, l))
	| Some (r, -1) ->
		let l = l + 4 in
		(-l, (f, (v, (r, -l))::(remove_assoc v s), m, l))
	| Some (_, o) ->
		(o, ctx)


(** Add the given virtual register to the list of modified
	registers.
	@param ctx	Current context.
	@param v	Modified variable.
	@return		New context. *)
let add_modif ctx v =
	let (f, s, m, l) = ctx in
	if not (mem v m)
	then (f, s, v::m, l)
	else ctx


(** Get the list of modified virtual registers.
	@param ctx	Current context.
	@return		Modified list. *)
let get_modifs ctx =
	let (_, _, m, _) = ctx in
	m


(** Free the real register allocated to a virtual register.
	@param ctx	Current context.
	@param v	Virtual register to free.
	@return		New context. *)
let free_reg ctx v =
	let (f, s, m, l) = ctx in
	match assoc_opt v s with
	| None 
	| Some (-1, _) ->
		failwith "Regalloc.free_reg"
	| Some (r, o) -> (
		r::f,
		(v, (-1, o))::(remove_assoc v s),
		filter (fun r' -> r <> r) m,
		l)


(** Generate code to save a register to the frame.
	@param ctx	Current context.
	@param v	Virtual register to save.
	@return		(generated instruction list, new context) *)
let save_reg ctx v =
	let r = get_reg ctx v in
	let (o, ctx) = get_offset ctx v in
	([(sprintf "str r%d, [fp, #%d]" r o, [])], ctx)


(** Generate code to restore a register from the frame.
	@param ctx	Current context.
	@param v	Virtual register to restore.
	@return		Generated instruction list. *)
let restore_reg ctx v =
	let r = get_reg ctx v in
	let (o, ctx) = get_offset ctx v in
	[(sprintf "ldr r%d, [fp, #%d]" r o, [])]


(** Try to find a real register for the given virtual register.
	@param ctx	Current register.
	@param v	Virtual register to find a real register for.
	@return		(real register, new context). *)
let spill ctx v w =
	failwith "spill: not enough registers."


(** Perform local register allocation in the given BB.
	@param l	Local memory size.
	@param i	BB index.
	@param v	BB.
	@return		BB after allocation. *)
let alloc_bb l i b =
	b


(** Allocate register in the given CFG.
	@param g		CFG to allocate register in.
	@return			Resulting CFG. *)
let cfg g =
	let (i, l, g) = g in
	(i, l, Cfg.replace (alloc_bb l) g)
