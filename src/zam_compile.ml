open List;;
open Zam;;
open Syntax;;

let rec position (x : string) (venv : string list) : int =
  match venv with
    | [] -> failwith "no matching variable in environment"
    | y::venv2 -> if x=y then 0 else (position x venv2) + 1;;


	
      

let rec compile ast venv =
  let rec apphelper ast venv =
    match ast with
      | App(e1, e2) -> concat [(apphelper e2 venv); (compile e1 venv)]
      | _ -> compile ast venv
  in

  let rec helper ast venv =
    let rec apphelper ast venv =
      match ast with
	| App(e1, e2) -> concat [(apphelper e2 venv); (compile e1 venv)]
	| _ -> compile ast venv
    in
    match ast with  
      | Var(x) -> [ZAM_Access (position x venv); ZAM_Return]
      | IntLit(n) -> [ZAM_Ldi(n); ZAM_Return]
      | BoolLit(b) -> [ZAM_Ldb(b); ZAM_Return]
      | Plus(e1, e2) ->
	concat [(compile e2 venv); (compile e1 venv); [ZAM_Add; ZAM_Return]]
      | Minus(e1, e2) ->
	concat [(compile e2 venv); (compile e1 venv); [ZAM_Minus; ZAM_Return]]
      | Eq(e1, e2) ->
	concat [(compile e2 venv); (compile e1 venv); [ZAM_Eq; ZAM_Return]]
      | Let(x, e1, e2) ->
	concat [(compile e1 venv); [ZAM_Let]; (helper e2 (x::venv))]
      | If(e1, e2, e3) ->
	concat [(compile e1 venv); [ZAM_Test( (helper e2 venv), (helper e3 venv))]]
	  
      | Fun(x, e) -> concat [[ZAM_Grab];  (helper e (x :: "_" :: venv))]
      | LetRec(f, x, e1, e2) ->
	concat [[ZAM_Closure(helper e1 (x::f::venv))]; [ZAM_Let]; (helper e2 (f::venv))]
      | App(e1, e2) ->
	concat [ (compile e2 venv); (apphelper e1 venv); [ZAM_TailApply]]
      | Empty -> []
  in

  match ast with 
    | Var(x) -> [ZAM_Access (position x venv)]
    | IntLit(n) -> [ZAM_Ldi(n)]
    | BoolLit(b) -> [ZAM_Ldb(b)]
    | Plus(e1, e2) ->
      concat [(compile e2 venv); (compile e1 venv); [ZAM_Add]]
    | Minus(e1, e2) ->
      concat [(compile e2 venv); (compile e1 venv); [ZAM_Minus]]
    | Eq(e1, e2) ->
      concat [(compile e2 venv); (compile e1 venv); [ZAM_Eq]]
    | Let(x, e1, e2) ->
      concat [(compile e1 venv); [ZAM_Let]; (compile e2 (x::venv)); [ZAM_EndLet]]
    | If(e1, e2, e3) ->
      concat [(compile e1 venv); [ZAM_Test( (compile e2 venv), (compile e3 venv))]]

    | Fun(x, e) -> [ZAM_Closure (helper e (x :: "_" :: venv))]
    | LetRec(f, x, e1, e2) ->
      concat [[ZAM_Closure(helper e1 (x::f::venv))]; [ZAM_Let]; (compile e2 (f::venv)); [ZAM_EndLet]]
    | App(e1, e2) ->
      concat [[ZAM_PushMark]; (compile e2 venv); (apphelper e1 venv); [ZAM_Apply]]
	
    | Empty -> []
;;

