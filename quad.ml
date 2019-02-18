(*	Representation of our quad low-level intermediate representation
	and friend functions. *)

open Common
open List
open Printf

(** Label representation. *)
type label = string

(** Virtual registers. *)
type vreg = int

(** Null register *)
let zr = 0

(** Virtual register for PC. *)
let pc = 1

(* Virtual register for SP. *)
let sp = 2

(* Virtual register for FP. *)
let fp = 3

(** Virtual register to return a value. *)
let vr = 4


(** Representation of quadruplets. *)
type quad =
	| QNONE										(** empty quad *)
	| QSETI of vreg * int						(** vi <- integer *)
	| QSETF of vreg * float						(** vi <- float *)
	| QSETL of vreg * string					(** vi <- l *)
	| QSET of type_t * vreg * vreg				(** vi <- vj *)
	| QNEG of type_t * vreg * vreg				(** vi <- -vj *)
	| QINV of type_t * vreg * vreg				(** vi <- ~vj *)
	| QADD of type_t * vreg * vreg * vreg		(** vi <- vj + vk *)
	| QSUB of type_t * vreg * vreg * vreg		(** vi <- vj - vk *)
	| QMUL of type_t * vreg * vreg * vreg		(** vi <- vj * vk *)
	| QDIV of type_t * vreg * vreg * vreg		(** vi <- vj / vk *)
	| QMOD of type_t * vreg * vreg * vreg		(** vi <- vj % vk *)
	| QSHL of type_t * vreg * vreg * vreg		(** vi <- vj << vk *)
	| QSHR of type_t * vreg * vreg * vreg		(** vi <- vj >> vk *)
	| QAND of type_t * vreg * vreg * vreg		(** vi <- vj & vk *)
	| QOR of type_t * vreg * vreg * vreg		(** vi <- vj | vk *)
	| QXOR of type_t * vreg * vreg * vreg		(** vi <- vj ^ vk *)
	| QCAST of type_t * vreg * type_t * vreg	(** (tt)vi <- (ft)vj *)
	| LABEL of label							(** l: *)
	| GOTO of label								(** goto l *)
	| IFEQ of type_t * vreg * vreg * label		(** if vi == vj goto l *)
	| IFNE of type_t * vreg * vreg * label		(** if vi != vj goto l *)
	| IFLT of type_t * vreg * vreg * label		(** if vi < vj goto l *)
	| IFLE of type_t * vreg * vreg * label		(** if vi <= vj goto l *)
	| IFGT of type_t * vreg * vreg * label		(** if vi > vj goto l *)
	| IFGE of type_t * vreg * vreg * label		(** if vi >= vj goto l *)
	| CALL of type_t * vreg * vreg * vreg list	(** vi <- vj(vs) @ subprogram call *)
	| RETURN									(** subrogram return *)
	| LOAD of type_t * vreg * vreg				(** vi <- M_t[vj] *)
	| STORE of type_t * vreg * vreg				(** M_t[vi] <- vj *)


(** Representation of data in memory. *)
type data =
	| QCHAR of string * int
	| QINT of string * int
	| QFLOAT of string * float


(** Output a data item.
	@param out	Output stream to use.
	@param d	Data item to output. *)
let output_data out d =
	match d with
	| QCHAR (l, v)	-> fprintf out "%s: .char %d" l v
	| QINT (l, v)	-> fprintf out "%s: .int %d" l v
	| QFLOAT (l, v)	-> fprintf out "%s: .float %f" l v
	

(** Representation of the program. *)
type comp_unit = quad list * data list


(** Label counter. *)
let lab_cnt = ref 0


(** Generator of new label. *)
let new_lab _ =
	incr lab_cnt;
	sprintf "__l%d" !lab_cnt


(** Virtual integer register counter. *)
let int_vreg_cnt = ref 4


(** Virtual float register counter. *)
let flt_vreg_cnt = ref 1000000


(** Generate a new virtual register. *)
let new_tmp t =
	match t with
	| VOID
	| FUN _ ->
		failwith "Quad: new_tmp"
	|	INT
	|	CHAR
	|	PTR _ ->
		incr int_vreg_cnt;
		!int_vreg_cnt
	|	FLOAT ->
		incr flt_vreg_cnt;
		!flt_vreg_cnt


(** Output a quadruplet.
	@param out	Output stream to ouput to.
	@param q	Quad to output. *)
let output_quad out q =

	let rec outp com out vs =
		match vs with
		| [] -> ()
		| v::vs ->
			if com then output_string out ", ";
			fprintf out "v%d" v;
			outp true out vs in

	match q with
	| QNONE					-> output_string out "nop"
	| QSETI (i, k)			-> fprintf out "v%d <- %d" i k
	| QSETF (i, x)			-> fprintf out "v%d <- %f" i x
	| QSETL (i, l)			-> fprintf out "v%d - %s" i l
	| QSET (t, i, j)		-> fprintf out "v%d <- v%d (%a)" i j outt t
	| QNEG (t, i, j)		-> fprintf out "v%d <- -v%d (%a)" i j outt t
	| QINV (t, i, j)		-> fprintf out "v%d <- ~v%d (%a)" i j outt t
	| QADD (t, i, j, k)		-> fprintf out "v%d <- v%d + v%d (%a)" i j k outt t
	| QSUB (t, i, j, k)		-> fprintf out "v%d <- v%d - v%d (%a)" i j k outt t
	| QMUL (t, i, j, k)		-> fprintf out "v%d <- v%d * v%d (%a)" i j k outt t
	| QDIV (t, i, j, k)		-> fprintf out "v%d <- v%d / v%d (%a)" i j k outt t
	| QMOD (t, i, j, k)		-> fprintf out "v%d <- v%d %% v%d (%a)" i j k outt t
	| QSHL (t, i, j, k)		-> fprintf out "v%d <- v%d << v%d (%a)" i j k outt t
	| QSHR (t, i, j, k)		-> fprintf out "v%d <- v%d >> v%d (%a)" i j k outt t
	| QAND (t, i, j, k)		-> fprintf out "v%d <- v%d & v%d (%a)" i j k outt t
	| QOR (t, i, j, k)		-> fprintf out "v%d <- v%d | v%d (%a)" i j k outt t
	| QXOR (t, i, j, k)		-> fprintf out "v%d <- v%d ^ v%d (%a)" i j k outt t
	| QCAST (tt, i, ft, j)	-> fprintf out "(%a)v%d <- (%a)v%d" outt tt i outt ft j
	| LABEL l				-> fprintf out "%s:" l
	| GOTO l				-> fprintf out "goto %s" l
	| IFEQ (t, i, j, l)		-> fprintf out "if v%d == v%d (%a) goto %s" i j outt t l
	| IFNE (t, i, j, l)		-> fprintf out "if v%d != v%d (%a) goto %s" i j outt t l
	| IFLT (t, i, j, l)		-> fprintf out "if v%d < v%d (%a) goto %s" i j outt t l
	| IFLE (t, i, j, l)		-> fprintf out "if v%d <= v%d (%a) goto %s" i j outt t l
	| IFGT (t, i, j, l)		-> fprintf out "if v%d > v%d (%a) goto %s" i j outt t l
	| IFGE (t, i, j, l)		-> fprintf out "if v%d >= v%d (%a) goto %s" i j outt t l
	| CALL (t, i, j, vs)	-> fprintf out "v%d <- v%d(%a) (%a)" i j (outp false) vs outt t 
	| RETURN				-> fprintf out "return"
	| LOAD (t, i, j)		-> fprintf out "v%d <-M(%a)[v%d]" i outt t j
	| STORE (t, i, j)		-> fprintf out "M(%a)[v%d] <- v%d" outt t i j


(** Output the given compilation unit.
	@param out	Output to use.
	@param unit	Unit to display. *)
let output_unit out unit =
	let (code, data) = unit in
	output_string out "CODE:\n\t";
	iter (fun q -> output_quad out q; output_string out "\n") code;
	output_string out "\nDATA:\n\t";
	iter (fun d -> output_data out d; output_string out "\n") data
	

let outd = output_data
let outq = output_quad
let outu = output_unit
