module AbSyn

type Type =
  | Int
  | Array   of Type

type Value =
  | IntVal   of int64
  | ArrayVal of Value list * Type

type Exp =
  | Const   of Value
  | Var     of string
  | Index   of string * Exp * Exp * int
  | And     of Exp * Exp
  | Or      of Exp * Exp
  | Not     of Exp
  | Equal   of Exp * Exp
  | Plus    of Exp * Exp
  | Minus   of Exp * Exp
  | NotEq   of Exp * Exp
  | Times   of Exp * Exp
  | Divide  of Exp * Exp
  | Xor     of Exp * Exp
  | BAnd    of Exp * Exp
  | BOr     of Exp * Exp
  | Mod     of Exp * Exp
  | Geq     of Exp * Exp
  | Leq     of Exp * Exp
  | Great   of Exp * Exp
  | Less    of Exp * Exp
  | Par     of Exp

type Defvar =
    Dvar    of Type * string
  | Array   of Type * string * Exp * Exp * int

type Op_r =
  | PlusE   of Exp
  | MinusE  of Exp
  | PowerE  of Exp
  | Switch  of Exp

type Proc = Procedure of string * Exp list

type Stmt =
  | Call    of Proc
  | Uncall  of Proc
  | If      of Exp * Stmt * Stmt * Exp
  | From    of Exp * Stmt * Stmt * Exp * int
  | Iter    of Defvar * Exp * Exp * Exp * Stmt
  | VarApp  of Exp * Op_r
  | Local   of Defvar * Exp * Stmt * Defvar * Exp
  | Stmts   of Stmt * Stmt

type Def = Define of string * Defvar list * Stmt

type Defmain = Main of Defvar list * Stmt

type Prog = Program of Def list * Defmain * Def list
