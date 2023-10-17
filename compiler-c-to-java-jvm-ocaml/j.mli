exception ConsistencyCheck

module Access :
  sig
    type t = Public | Final | Super | Interface | Abstract
  end

module Type :
  sig
    type t =
        Void
      | Bool
      | Char
      | Int
      | Float
      | Double
      | Object of string
      | Array of t
  end

module Path :
  sig
    type t
    val parse : string -> t
    val create : string list -> t
    val cons : string -> t -> t
    val tail : t -> t
    val head : t -> string
  end

module Label :
  sig
    type t
    val create : unit -> t
  end

module Cmp :
  sig
    type t = Eq | Ne | Lt | Le | Gt | Ge
  end

module Code :
  sig
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
    val load : Type.t -> int -> t
    val store : Type.t -> int -> t
    val jumpIf : Cmp.t -> Type.t -> Label.t -> t
    val jumpIfZero : Cmp.t -> Label.t -> t
    val convert : Type.t -> Type.t -> t
  end

module Method :
  sig
    type t
    val create : Path.t -> Type.t list -> Type.t -> t
    val emit : t -> Code.t -> unit
    val improve : t -> unit
  end

module Class :
  sig
    type t
    val create : Path.t -> t
    val add_method : t -> Method.t -> unit
    val create_method : string -> t -> Type.t list -> Type.t -> Method.t
    val get_path : t -> Path.t
    val serialize : out_channel -> t -> unit
  end
