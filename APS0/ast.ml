type oprim = Add | Mul | Sub | Div | And | Or | Eq | Lt
type opUnary = Not


type type_ =
    AstTypeInt
  | AstTypeBool
  | AstTypeFun of types * type_ (* correspond à (TYPES -> TYPE) *)

and types =
    AstType of type_
  | AstTypes of type_ * types

type prog =
    AstProg of cmds

and cmds =
    AstStat of stat
  | AstDec of dec * cmds
  | AstStats of stat *cmds

and arg = (expr * type_)

and args =
    AstArg of arg
  | AstArgs of arg * args

and dec =
    AstConst of expr * type_ * expr
  | AstFun of expr * type_ * args * expr
  | AstFunRec of expr * type_ * args * expr

and stat =
  AstEcho of expr

and expr =
    AstNum of int
	| AstTrue
 	| AstFalse
	| AstId of string
 	| AstPrim of oprim * expr list
  | AstUnary of opUnary * expr
  | AstIf of expr * expr * expr
  | AstAbstraction of args * expr
  | AstApply of expr * expr list



(* BINARY OPERATION *)
let string_of_oprim oprim = match oprim with
    Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | Div -> "div"
  | And -> "and"
  | Or -> "or"
  | Eq -> "eq"
  | Lt -> "lt"

let oprim_of_string oprim = match oprim with
	"add" -> Add
	| "mul" -> Mul
	| "sub" -> Sub
	| "div" -> Div
	| "and" -> And
	| "or" -> Or
	| "eq" -> Eq
  | "lt" -> Lt
  | _ -> failwith "not a oprim"

(* UNARY OPERATION*)
let string_of_opUnary op = match op with
   Not -> "not"

let opUnary_of_string op = match op with
   "not" -> Not
  | _ -> failwith "not a oprim"

