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

(** This module provides a CFG representation. *)

open Printf
open Quad

(** Entry vertex index. *)
let entry = 0

(** Exit vertex index. *)
let exit = 1

(** Description of a graph.
	'a is the type of nodes.
	'b is the value stored on an edge. *)
type ('a, 'b) graph = (int * 'a) list * ((int * int) * 'b) list

 
(** Creates a new graph. The entry vertex will get the number 0
	and exit vertex the number 1.
	@param en	Entry node.
	@param ex	Exit node.
	@return		Empty graph except en and ex. *)
let new_graph en ex = ([(0, en); (1, ex)], [])


(** Add a vertex to the graph.
	@param g	Graph to add to.
	@param v	Added vertex.
	@return		(vertex index, modified graph). *)
let add_vertex g v =
	let (vs, es) = g in
	let i = List.length vs in
	(i, (vs @ [(i, v)], es))


(** Get the vertex corresponding to an index.
	@param g	Graph to look in.
	@param i	Vertex index.
	@return		Corresponding vertex. *)
let get_vertex g i =
	let (vs, _) = g in
	List.assoc vs i


(** Change the vertex value.
	@param g	Graph to change in.
	@param i	Index of vertex to change.
	@param v	New vertex value.
	@return		New graph. *)
let set_vertex g i v =
	let rec set vs i =
		match vs with
		| []	-> failwith "bad index"
		| _::t when i = 0	-> v :: t
		| h::t				-> h :: (set t (i - 1)) in
	let (vs, es) = g in
	(set vs i, es)


(** Add an edge to the graph.
	@param g	Graph to add edge to.
	@param i	Source vertex index.
	@param j	Sink vertex index.
	@param x	Edge value.
	@return		Changed graph. *)
let add_edge g i j x =
	let (vs, es) = g in
	(vs, ((i, j), x)::es)


(** Get the value of an edge.
	@param g	Graph to look in.
	@param i	Source vertex index.
	@param j	Sink vertex index.
	@return		Edge value. *)
let get_edge g i j =
	let (_, es) = g in
	List.assoc (i, j) es


(** Iterate on vertices of a graph. f is called as f i v with
	i the index of the vertex and v the vertex value itself.
	@param f	Function applied to each vertex.
	@param g	Graph to work on. *)
let iter f g =
	let (vs, _) = g in
	List.iter (fun (i, v) -> f i v) vs


(** Apply the given f function to map each vertex of the graph.
	f is called as f i v with i the index of the vertex and v
	the vertex value itself.
	@param f	Function to apply.
	@param g	Graph to work on.
	@return		List of results of each f on each vertex. *)
let map f g =
	let (vs, _) = g in
	List.map (fun (i, v) -> f i v) vs


(** Fold the vertices of the graph with the given function.  f is called
	as f x i v with w the folded value, i the index of the vertex and
	v the vertex value itself.
	@param f	Function to fold vertices.
	@param g	Graph to work on.
	@param x	Initial value.
	@return		Resulting value. *)
let fold f g x =
	let (vs, _) = g in
	List.fold_left (fun x (v, i) -> f x v i) x vs


(** Iterate on successors of the given vertex index.
	@param f	Function to call on each successor.
	@param g	Graph to work on.
	@param i	Index of vertex to look successor for. *)
let iter_succs f g i =
	let (_, es) = g in
	List.iter (fun ((j, k), x) -> if j = i then f k x) es


(** Fold on successors of the given vertex index.
	@param f	Function to call on each successor.
	@param g	Graph to work on.
	@param i	Index of vertex to look successor for.
	@param x	Initial value.
	@return		Final value. *)
let fold_succs f g i x =
	let (_, es) = g in
	List.fold_right
		(fun ((j, k), x) y -> if j = i then f k x y else y)
		es
		x


(** Map on successors of the given vertex index.
	@param f	Function to call on each successor.
	@param g	Graph to work on.
	@param i	Index of vertex to look successor for.
	@return		List of result of f on each successor. *)
let map_succs f g i =
	fold_succs (fun j x y -> (f j x)::y) g i []


(** Replace the vertices by the vertices result from g applied on each
	vertex.  f is called as f i v with i the index of the vertex and v
	the vertex value itself. f returns the new value vertex.
	@param f	Function to apply to each vertex.
	@param g	Graph to work.
	@return		Change graph. *)
let replace f g =
	let (vs, es) = g in
	(List.map (fun (i, v) -> (i, f i v)) vs, es)


(** Build CFG from quads.
	@param qs	Quads.
	@return		Quad CFG. *)
let from_quads qs =
	new_graph [] []


(** Rebuild the list of instructions from the CFG.
	@param g	CFG.
	@return		List of instructions. *)
let to_list g =
	fold (fun l i v -> l @ v) g []


(** Output the CFG.
	@param out	Output channel.
	@param g	Graph to output.
	@param f	Function to print vertex value. *)
let output out g f =
	iter (fun i v ->
			fprintf out "BB %d" i;
			iter_succs
				(fun k t ->
					if t then fprintf out " T(%d)" k
					else fprintf out " NT(%d)" k)
				g
				i;
			output_char out '\n';
			f out v;
			output_char out '\n')
		g 


(** Print the CFG to standard output. *)
let print g f = output stdout g f
