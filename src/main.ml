(* main.ml *)

(* 構文定義ファイル syntax.ml で定義された exp型を使う *)
open Syntax ;;
open Eval;;
(* 与えられた文字列の字句解析と構文解析だけを行う関数 *)
(* parse : string -> exp *)

let parse str = 
  Parser.main Lexer.token 
    (Lexing.from_string str)

(* 使用例は以下の通り。parse関数は Mainモジュールにはいっているので
   open Main;; parse "...";; とするか Main.parse "...";;
   として呼びだす。

$ ./miniocaml
      Objective Caml version 3.09.1

# Main.parse "let x = 3 + 1 * 4 in fun y -> x + y";;
- : Syntax.exp =
Syntax.Let ("x",
 Syntax.Plus (Syntax.IntLit 3,
  Syntax.Times (Syntax.IntLit 1, Syntax.IntLit 4)),
 Syntax.Fun ("y", Syntax.Plus (Syntax.Var "x", Syntax.Var "y")))
#  Main.parse "1 + 2 * 3 ";;
- : Syntax.exp =
Syntax.Plus (Syntax.IntLit 1,
 Syntax.Times (Syntax.IntLit 2, Syntax.IntLit 3))
#
*)
let emptyenv () = [];;
let print_value v =
  let print_default t v =
    print_string ("miniml : "^ t ^ " = " ^ v ^ "\n")
  in
  let rec print_rec v =
  match v with
    | Syntax.IntVal i -> print_default "int" (string_of_int i)
    | Syntax.BoolVal b -> print_default "bool"  (if b then "true" else "false")
  in
  print_rec v ;;
let run str =
 print_value (Eval.eval (parse str) (emptyenv()));;
let run2 str = 
  (Eval.eval (parse str) (emptyenv()));;
