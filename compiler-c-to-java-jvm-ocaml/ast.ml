type gdecl = 
	 Efunct of string * ctype * string * ctype list
	|Funct of ctype * string * ctid list * stmt


and ctype =
	 Tvoid 
	|Tbool
 	|Tint 
	|Tfloat 
	|Tstring 
	|Tarray of ctype
	|Tfunction of ctype list * ctype * string
	

and ctid = Tid of ctype * string


and const = 
	 CBool of bool
	|CInt of int
	|CString of string
	|CFloat of float 


and aexpr = 
	 Id of string
	|Access of aexpr * expr


and expr =
	 Const of const
	|Aexpr of aexpr
	|Call of string * expr list
	|Create of ctype * expr
	|Cast of ctype * expr
	|Neg of expr
	|Pos of expr
	|Not of expr
	|Plus of expr * expr
	|Minus of expr * expr
	|Times of expr * expr
	|Divide of expr * expr
	|Rest of expr * expr
	|Eq of expr * expr
	|Ne of expr * expr
	|Lt of expr * expr
	|Le of expr * expr
	|Gt of expr * expr
	|Ge of expr * expr
	|And of expr * expr
	|Or of expr * expr


and stmt =
	 Line of expr
	|Assign of aexpr * expr
	|Cond of expr * stmt
	|Cond_else of expr * stmt * stmt
	|Loop of expr * stmt
	|Ret of expr
	|Block of block
	|For of stmt * expr * stmt * stmt
	|Do_While of stmt * expr
	|Switch of expr * branch list
	|Switch_default of expr * branch list * stmt


and branch = TBranch of expr * stmt
	 

and block = TBlock of ldecl list * stmt list


and ldecl = Ldecl of ctid
;;
