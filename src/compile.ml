open List;;
open Cam;;
open Syntax;;

let rec position (x : string) (venv : string list) : int =
  match venv with
    | [] -> failwith "no matching variable in environment"
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1;;

let rec compile ast venv =
  match ast with 
    | Var(x) -> [CAM_Access (position x venv)]
    | Fun(x, e) -> [CAM_Closure (append (compile e (x :: "_" :: venv)) [CAM_Return])]
  
    | App(e1, e2) -> append (compile e2 venv)
      (append (compile e1 venv) [CAM_Apply])
    | Let(x, e1, e2) ->
      append (compile e1 venv)
	(append [ CAM_Let ] 
	   (append (compile e2 (x::venv) )  [CAM_EndLet])
	)
    | LetRec(f, x, e1, e2) ->
      append [CAM_Closure(append (compile e1 (x::f::venv))  [CAM_Return])]
  	(append [CAM_Let]
  	   (append (compile e2 (f::venv)) [CAM_EndLet])
  	)
    | IntLit(n) -> [CAM_Ldi(n)]
    | BoolLit(b) -> [CAM_Ldb(b)]
    | Plus(e1, e2) -> concat [(compile e2 venv); (compile e1 venv); [CAM_Add]]
    | Minus(e1, e2) -> concat [(compile e2 venv); (compile e1 venv); [CAM_Minus]]
    | Times(e1, e2) -> concat [(compile e2 venv); (compile e1 venv); [CAM_Times]]
    | Div(e1, e2) -> concat [(compile e2 venv); (compile e1 venv); [CAM_Div]]
    | Eq(e1, e2) ->
      append (compile e2 venv)
	(append (compile e1 venv) [CAM_Eq])
    | If(e1, e2, e3) ->
      append (compile e1 venv)
	[CAM_Test( (compile e2 venv), (compile e3 venv))]
    | Empty -> []
;;
      
