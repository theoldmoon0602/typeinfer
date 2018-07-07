exception TypeError of string

type id = string
type op =
  |Add
  |Mul
  |Gt
  |Lt
  |And
  |Or

type typ =
  |NumT
  |BoolT
  |VarT of string
  |FunT of typ * typ

let arg_typ funt =
  match funt with
  |FunT(argt, _) -> argt
  |_ -> raise (TypeError "function type requried")

let ret_typ funt =
  match funt with
  |FunT(_, rett) -> rett
  |_ -> raise (TypeError "function type requried")

type expr =
  |NumLit of int
  |BoolLit of bool
  |Var of string
  |Binop of op * expr * expr
  |Fun of id * expr
  |App of expr * expr
  |If of expr * expr * expr

type expr_t =
  |TNumLit of int * typ
  |TBoolLit of bool * typ
  |TVar of string * typ
  |TBinop of op * expr_t * expr_t * typ
  |TFun of id * expr_t * typ
  |TApp of expr_t * expr_t * typ
  |TIf of expr_t * expr_t * expr_t * typ

let string_of_binop op =
  match op with
  |Add -> "+"
  |Mul -> "*"
  |Gt -> ">"
  |Lt -> "<"
  |And -> "&&"
  |Or -> "||"

let rec string_of_expr expr =
  match expr with
  |NumLit(v) -> string_of_int v
  |BoolLit(v) -> if v then "true" else "false"
  |Var(v) -> v
  |Binop(op, lhs, rhs) -> (string_of_expr lhs)^" "^((string_of_binop op)^" "^string_of_expr rhs)
  |Fun(name, body) -> "(fun "^name^" -> "^(string_of_expr body)^")"
  |App(func, arg) -> "("^(string_of_expr func)^" "^(string_of_expr arg)^")"
  |If(cond, t, e) -> "if "^(string_of_expr cond)^" then "^(string_of_expr t)^" else "^(string_of_expr e)


let rec string_of_typ typ =
  match typ with
  |NumT -> "number"
  |BoolT -> "bool"
  |VarT(s) -> s
  |FunT(arg, ret) -> (string_of_typ arg)^"->"^(string_of_typ ret)

