(* Copyright (C) 2004, Luca Padovani <lpadovan@cs.unibo.it>.
 *
 * j.ml is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * j.ml is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with j.ml; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * For details, see the World-Wide-Web page
 * http://www.cs.unibo.it/~lpadovan/ or mail <lpadovan@cs.unibo.it>
 *)

exception ConsistencyCheck

module Access =
struct
  type t =
      Public
    | Final
    | Super
    | Interface
    | Abstract

  let serialize =
    function
	Public -> "public"
      | Final -> "final"
      | Super -> "super"
      | Interface -> "interface"
      | Abstract -> "abstract"

  let serialize_list l =
    String.concat " " (List.map serialize l)
end

module Type =
struct
  type t =
      Void
    | Bool
    | Char
    | Int
    | Float
    | Double
    | Object of string
    | Array of t 
	
  let sizeof =
    function
	Void -> 0
      | Bool
      | Char
      | Int
      | Float
      | Object _
      | Array _ -> 1
      | Double -> 2

  let rec serialize =
    function
	Void -> "V"
      | Bool -> "B"
      | Char -> "C"
      | Int -> "I"
      | Float -> "F"
      | Double -> "D"
      | Object path -> "L" ^ path ^ ";"
      | Array t -> "[" ^ serialize t

  let rec serialize_prefix =
    function
	Bool
      | Char
      | Int -> "i"
      | Float -> "f"
      | Double -> "d"
      | Object _
      | Array _ -> "a"
      | _ -> assert false
end

module Path =
struct
  type t = Path of string list

  let parse s =
    let rec aux l =
      function
	  "" -> l
	| s ->
	    try
	      let index = String.index s '/' in
		aux (String.sub s 0 index::l) (String.sub s (index + 1) (String.length s - index - 1))
	    with
		Not_found -> s::l
    in
      Path (aux [] s)

  let create sl = Path sl

  let serialize =
    function
	Path sl -> String.concat "/" (List.rev sl)

  let cons hd =
    function
	Path sl -> Path (hd::sl)
    
  let tail =
    function
	Path sl -> Path (List.tl sl)

  let head =
    function
	Path sl -> List.hd sl
end

module Label =
struct
  let next_label_id = ref 0

  type t = L of int

  let create () =
    let id = !next_label_id in
      incr next_label_id ;
      L id

  let serialize =
    function
	L id -> "L" ^ string_of_int id
end

module Cmp =
struct
  type t = Eq | Ne | Lt | Le | Gt | Ge

  let serialize =
    function
	Eq -> "eq"
      | Ne -> "ne"
      | Lt -> "lt"
      | Le -> "le"
      | Gt -> "gt"
      | Ge -> "ge"

  let invert =
    function
	Eq -> Ne
      | Ne -> Eq
      | Lt -> Ge
      | Le -> Gt
      | Gt -> Le
      | Ge -> Lt
end

let output_endline ch s = output_string ch (s ^ "\n")
let indent = List.map (fun s -> "\t" ^ s)
let flatten = String.concat ""
let serialize_fun_sig tl typ =
  "(" ^ flatten (List.map Type.serialize tl) ^ ")" ^ Type.serialize typ
let fits_byte n = -(1 lsl 8) <= n && n < (1 lsl 8)
let fits_short n = -(1 lsl 16) <= n && n < (1 lsl 16)

module Code =
struct
  type t =
      Label of Label.t
    | JumpIf of Cmp.t * Type.t * Label.t
    | JumpIfZero of Cmp.t * Label.t
    | Jump of Label.t
    | Call of Path.t * Type.t list * Type.t
    | New of Path.t
    | NewArray of Type.t
    | Load of Type.t * int
    | Store of Type.t * int
    | ArrayLoad of Type.t
    | ArrayStore of Type.t
    | ArrayLength
    | Convert of Type.t * Type.t
    | LoadNull
    | LoadInt of int
    | LoadFloat of float
    | LoadDouble of float
    | LoadString of string
    | Inc of int * int
    | Neg of Type.t
    | Add of Type.t
    | Sub of Type.t
    | Mul of Type.t
    | Div of Type.t
    | Rem of Type.t
    | And
    | Or
    | Pop of Type.t
    | Return of Type.t
    | TableSwitch of int * Label.t list * Label.t
    | LookupSwitch of (int * Label.t) list * Label.t

  let load typ index = Load (typ, index)
  let store typ index = Store (typ, index)
  let jumpIf c typ l = JumpIf (c, typ, l)
  let jumpIfZero c l = JumpIfZero (c, l)
  let convert s t = Convert (s, t)

  let delta =
    function
	Label _
      | Jump _
      | Neg _
      | Inc _ 
      | NewArray _
      | ArrayLength -> 0
      | Convert (s, t) -> Type.sizeof t - Type.sizeof s
      | Load (t, _) -> Type.sizeof t
      | New _
      | LoadNull
      | LoadInt _
      | LoadFloat _
      | LoadString _ -> 1
      | LoadDouble _ -> 2
      | ArrayLoad t -> Type.sizeof t - 2
      | Add t
      | Sub t
      | Mul t
      | Div t
      | Rem t 
      | Pop t
      | Store (t, _)
      | Return t -> -(Type.sizeof t)
      | JumpIf (_, t, _) -> -(2 * Type.sizeof t)
      | ArrayStore t -> -(Type.sizeof t) - 2
      | And
      | Or -> -1
      | Call (_, tl, typ) ->
	  -(List.fold_left (+) 0 (List.map Type.sizeof tl)) + Type.sizeof typ
      | JumpIfZero _
      | TableSwitch _
      | LookupSwitch _ -> -1

  let serialize instr =
    let serialize_index n =
      (if n >= 0 && n <= 3 then "_" else " ") ^ string_of_int n
    in
      match instr with
	  Label l -> [Label.serialize l ^ ":"]
	| JumpIf (c, typ, l) -> 
	    indent
	      (match typ with
		   Type.Bool
		 | Type.Char
		 | Type.Int -> ["if_icmp" ^ Cmp.serialize c ^ " " ^ Label.serialize l]
		 | Type.Float
		 | Type.Double ->
	             [Type.serialize_prefix typ ^ "cmpl" ;
		      "if" ^ Cmp.serialize c ^ " " ^ Label.serialize l]
		 | Type.Object _ ->
	             [if c = Cmp.Eq || c = Cmp.Ne then
			"if_acmp" ^ Cmp.serialize c ^ " " ^ Label.serialize l
		      else
			assert false]
		 | _ -> assert false)
	| JumpIfZero (c, l) ->
	    indent ["if" ^ Cmp.serialize c ^ " " ^ Label.serialize l]
	| Jump l -> indent ["goto " ^ Label.serialize l]
	| Call (path, tl, typ) ->
	    indent ["invokestatic " ^ Path.serialize path ^ serialize_fun_sig tl typ]
	| New path -> indent ["new " ^ Path.serialize path]
	| NewArray typ ->
	    indent
	      [match typ with
		   Type.Array _ -> "anewarray " ^ Type.serialize typ
		 | Type.Object s -> "anewarray " ^ s
		 | Type.Bool
		 | Type.Int -> "newarray int"
		 | Type.Char -> "newarray char"
		 | Type.Float -> "newarray float"
		 | Type.Double -> "newarray double"
		 | _ -> assert false]
	| ArrayLength -> indent ["arraylength"]
	| Load (typ, index) ->
	    indent
	      [match typ with
		   Type.Bool | Type.Char | Type.Int
		 | Type.Array _ | Type.Object _
		 | Type.Float | Type.Double ->
		     Type.serialize_prefix typ ^ "load" ^ serialize_index index
		 | _ -> assert false]
	| Store (typ, index) ->
	    indent
	      [match typ with
		   Type.Bool | Type.Char | Type.Int
		 | Type.Array _ | Type.Object _
		 | Type.Float | Type.Double ->
		     Type.serialize_prefix typ ^ "store" ^ serialize_index index
		 | _ -> assert false]
	| ArrayLoad typ ->
	    indent
	      [match typ with
		   Type.Bool -> "baload"
		 | Type.Char -> "caload"
		 | Type.Int
		 | Type.Array _ | Type.Object _
		 | Type.Float | Type.Double ->
		     Type.serialize_prefix typ ^ "aload"
		 | _ -> assert false]
	| ArrayStore typ ->
	    indent
	      [match typ with
		   Type.Bool -> "bastore"
		 | Type.Char -> "castore"
		 | Type.Int | Type.Array _ | Type.Object _ | Type.Float | Type.Double ->
		     Type.serialize_prefix typ ^ "astore"
		 | _ -> assert false]
	| Convert (s, t) ->
	    indent
	      [match s, t with
		   Type.Int, Type.Float -> "i2f"
		 | Type.Int, Type.Double -> "i2d"
		 | Type.Float, Type.Int -> "f2i"
		 | Type.Float, Type.Double -> "f2d"
		 | Type.Double, Type.Int -> "d2i"
		 | Type.Double, Type.Float -> "d2f"
		 | _ -> assert false]
	| LoadNull -> indent ["aconst_null"]
	| LoadInt n ->
	    let opcode = 
	      if fits_byte n then "bipush"
	      else if fits_short n then "sipush"
	      else "ldc"
	    in
	      indent [opcode ^ " " ^ string_of_int n]
	| LoadFloat f -> indent ["ldc " ^ string_of_float f]
	| LoadDouble f -> indent ["ldc_w " ^ string_of_float f]
	| LoadString s -> indent ["ldc \"" ^ String.escaped s ^ "\""]
	| Inc (index, by) -> indent ["iinc " ^ string_of_int index ^ " " ^ string_of_int by]
	| Neg typ ->
	    indent
	      [match typ with
		   Type.Int
		 | Type.Float | Type.Double -> Type.serialize_prefix typ ^ "neg"
		 | _ -> assert false]
	| Add typ ->
	    indent
	      [match typ with
		   Type.Int
		 | Type.Float | Type.Double -> Type.serialize_prefix typ ^ "add"
		 | _ -> assert false]
	| Sub typ ->
	    indent
	      [match typ with
		   Type.Int
		 | Type.Float | Type.Double -> Type.serialize_prefix typ ^ "sub"
		 | _ -> assert false]
	| Mul typ ->
	    indent
	      [match typ with
		   Type.Int
		 | Type.Float | Type.Double -> Type.serialize_prefix typ ^ "mul"
		 | _ -> assert false]
	| Div typ ->
	    indent
	      [match typ with
		   Type.Int
		 | Type.Float | Type.Double -> Type.serialize_prefix typ ^ "div"
		 | _ -> assert false]
	| Rem typ ->
	    indent
	      [match typ with
		   Type.Int
		 | Type.Float | Type.Double -> Type.serialize_prefix typ ^ "rem"
		 | _ -> assert false]
	| And -> indent ["iand"]
	| Or -> indent ["ior"]
	| Pop typ ->
	    indent
	      [match typ with
		   Type.Bool | Type.Char | Type.Int
		 | Type.Float | Type.Object _ | Type.Array _ -> "pop"
		 | Type.Double -> "pop2"
		 | _ -> assert false]
	| Return typ ->
	    indent
	      [match typ with
		   Type.Void -> "return"
		 | Type.Bool | Type.Int
		 | Type.Float | Type.Double
		 | Type.Array _ | Type.Object _ ->
		     Type.serialize_prefix typ ^ "return"
		 | _ -> assert false]
	| TableSwitch (offset, ll, default) ->
	    indent
	      ["tableswitch " ^ string_of_int offset]
	    @ (List.map (fun l -> " " ^ Label.serialize l) ll)
	    @ [" default: " ^ Label.serialize default]
	| LookupSwitch (ll, default) ->
	    indent
	      ["lookup"]
	    @ (List.map
		 (fun (n, l) -> " case " ^ string_of_int n ^ ": " ^ Label.serialize l)
		 (List.sort (fun (n1, _) (n2, _) -> n1 - n2) ll))
	    @ [" default: " ^ Label.serialize default]

end

module Method =
struct
  type t =
      { path : Path.t;
	tl : Type.t list;
	typ : Type.t;
	mutable access : Access.t list;
	mutable code : Code.t list }

  let create p tl' typ' =
    { path = p;
      tl = tl';
      typ = typ';
      access = [Access.Public];
      code = [] }

  let consistency_error s =
    prerr_endline
      ("WARNING: consistency problem (" 
       ^ s ^ "), check the generated code!") ;
    (* raise ConsistencyCheck ; *)
    ()

  let wrong_args instr found expected =
    prerr_endline ("at: " ^ flatten (Code.serialize instr)) ;
    consistency_error
      ("found " 
       ^ string_of_int found 
       ^ " operands on stack, expected " 
       ^ string_of_int expected)

  let emit meth c =
    meth.code <- c::meth.code

  let compute_limits meth =
    let lt = Hashtbl.create 211 in
    let max_locals = ref (List.length meth.tl)
    and stack_size = ref 0
    and max_stack_size = ref 0
    in
    let aux c =
      begin
	match c with
	    Code.Load (_, n)
	  | Code.Store (_, n)
	  | Code.Inc (_, n) -> max_locals := max !max_locals (n + 1)
	  | _ -> ()
      end ;
      if !stack_size <> ~-1 then 
	begin
	  if !stack_size + Code.delta c < 0 then
	    wrong_args c !stack_size (!stack_size - Code.delta c) ;
	  stack_size := !stack_size + Code.delta c
	end
      else
	begin
	  match c with
	      Code.Label _ -> ()
	    | _ ->
		if Code.delta c < 0 then
		  (consistency_error "not enough operands on stack") ;
		stack_size := Code.delta c
	end ;
      max_stack_size := max !max_stack_size !stack_size ;
      match c with
	  Code.Label l ->
	    if Hashtbl.mem lt l then
	      begin
		let size = Hashtbl.find lt l in
		  if !stack_size <> ~-1 && !stack_size <> size then
		    wrong_args c !stack_size size ;
		  stack_size := size
	      end
	    else
	      Hashtbl.add lt l !stack_size
	| Code.Jump l ->
	    if Hashtbl.mem lt l then
	      begin
		let size = Hashtbl.find lt l in
		  if !stack_size <> size then wrong_args c !stack_size size ;
	      end
	    else
	      Hashtbl.add lt l !stack_size ;
	    stack_size := ~-1
	| Code.JumpIf (_, _, l)
	| Code.JumpIfZero (_, l) ->
	    if Hashtbl.mem lt l then
	      begin
		if !stack_size <> Hashtbl.find lt l then
		  (consistency_error "jump to different stack configuration")
	      end
	    else
	      Hashtbl.add lt l !stack_size
	| Code.Return Type.Void ->
	    if !stack_size <> 0 then
	      (consistency_error
		 ("void return with non-empty stack ("
		  ^ string_of_int !stack_size
		  ^ " values)")) ;
	    stack_size := ~-1
	| Code.Return _ ->
	    if !stack_size <> 0 then
	      (consistency_error
		 ("value return with non-empty stack ("
		  ^ string_of_int !stack_size
		  ^ " values)")) ;
	    stack_size := ~-1
	| _ -> ()
    in
      List.iter aux (List.rev meth.code) ;
      !max_locals, !max_stack_size

  let improve meth =
    let lc = Hashtbl.create 211 in
    let aux =
      function
	  Code.Label l when not (Hashtbl.mem lc l) -> Hashtbl.add lc l (ref 0)
	| Code.Jump l
	| Code.JumpIf (_, _, l)
	| Code.JumpIfZero (_, l) ->
	    begin
	      try
		let c = Hashtbl.find lc l in
		  incr c
	      with
		  Not_found -> Hashtbl.add lc l (ref 1)
	    end
	| _ -> ()
    in
      List.iter aux meth.code ;
      let rec aux improved new_code =
	function
	    [] -> improved, new_code
	  | Code.LoadInt 0::Code.JumpIf (c, Type.Int, l)::c' -> aux true new_code (Code.JumpIfZero (c, l)::c')
	  | Code.Load (Type.Int, i1)::Code.LoadInt n::Code.Add Type.Int::Code.Store (Type.Int, i2)::c'
	      when i1 = i2 && fits_byte n ->
	      aux true new_code (Code.Inc (i1, n)::c')
	  | Code.Load (Type.Int, i1)::Code.LoadInt n::Code.Sub Type.Int::Code.Store (Type.Int, i2)::c'
	      when i1 = i2 && fits_byte (-n) ->
	      aux true new_code (Code.Inc (i1, -n)::c')
	  | Code.Label l::c' when !(Hashtbl.find lc l) = 0 -> aux true new_code c'
	  | Code.Jump l1::Code.Label l2::c' when l1 = l2 ->
	      let counter = Hashtbl.find lc l1 in
		decr counter ;
		aux true new_code (Code.Label l1::c')
	  | Code.JumpIf (c, t, l1)::Code.Jump l2::Code.Label l3::c' when l1 = l3 ->
	      let counter = Hashtbl.find lc l1 in
		decr counter ;
		aux true new_code (Code.JumpIf (Cmp.invert c, t, l2)::Code.Label l3::c')
	  | Code.JumpIfZero (c, l1)::Code.Jump l2::Code.Label l3::c' when l1 = l3 ->
	      let counter = Hashtbl.find lc l1 in
		decr counter ;
		aux true new_code (Code.JumpIfZero (Cmp.invert c, l2)::Code.Label l3::c')
	  | ((Code.Jump _) as instr)::c' 
	  | ((Code.Return _) as instr)::c' -> aux_discard improved (instr::new_code) c'
	  | instr::c' -> aux improved (instr::new_code) c'
      and aux_discard improved new_code =
	function
	    [] -> improved, new_code
	  | (Code.Label _::_) as c -> aux improved new_code c
	  | Code.Jump l::c'
	  | Code.JumpIf (_, _, l)::c'
	  | Code.JumpIfZero (_, l)::c' ->
	      let counter = Hashtbl.find lc l in
		decr counter ;
		aux_discard true new_code c'
	  | _::c' -> aux_discard true new_code c'
      in
      let rec aux_iter code =
	let improved, new_code = aux false [] (List.rev code) in
	  if improved then aux_iter new_code else code
      in
	meth.code <- aux_iter meth.code

  let serialize ch meth =
    let max_locals, max_stack_size = compute_limits meth in
    output_endline ch (".method public static " ^ Path.head meth.path ^ serialize_fun_sig meth.tl meth.typ) ;
    output_endline ch
      (flatten (indent [".limit stack " ^ string_of_int max_stack_size])) ;
    output_endline ch
      (flatten (indent [".limit locals " ^ string_of_int max_locals]) ^ "\n") ;
    output_endline ch "" ;
    List.iter (output_endline ch)
      (List.flatten (List.map Code.serialize (List.rev meth.code))) ;
    output_endline ch ".end method" ;
    output_endline ch ""

end

module Class =
struct
  type t =
      { path : Path.t;
	mutable super : Path.t;
	mutable access : Access.t list;
	mutable implements : Path.t list;
	mutable methods : Method.t list }

  let create p =
    { path = p;
      super = Path.create ["Object"; "lang"; "java"];
      access = [Access.Public];
      implements = [];
      methods = [] }

  let add_method cls meth = 
    cls.methods <- meth::cls.methods

  let create_method name cls tl typ =
    let meth = Method.create (Path.cons name cls.path) tl typ in
      add_method cls meth ;
      meth

  let get_path cls = cls.path

  let serialize ch cls =
    output_endline ch (".class " ^ Access.serialize_list cls.access ^ " " ^ Path.serialize cls.path) ;
    output_endline ch (".super " ^ Path.serialize cls.super) ;
    output_endline ch ("\n\
;\n\
; standard initializer\n\
.method public <init>()V\n\
   aload_0\n\
\
   invokenonvirtual " ^ Path.serialize cls.super ^ "/<init>()V\n\
   return\n\
.end method") ;
    List.iter (Method.serialize ch) cls.methods

end

