open Syntax;;
open Eval;;



let current_id = ref 0
let fresh_var () =
  let i = !current_id in
  current_id := i+1;
  "var" ^ (string_of_int i);;

let rec conversion ast cont =
  match ast with
    | Var(x) -> App(cont, Var(x))
    | IntLit(n) -> App(cont, (IntLit(n)))
    | BoolLit(b) -> App(cont, (BoolLit(b)))
    | If(e1, e2, e3) ->
      let r1 = fresh_var() in
      (conversion e1 (Fun(r1, If(Var(r1), (conversion e2 cont), (conversion e3 cont)))))
    | Let(x, e1, e2) ->
      let r1 = fresh_var() in
      (conversion e1 
	 (Fun(r1, (Let(x, Var(r1), (conversion e2 cont))))))
    | LetRec(f, x, e1, e2) ->
      LetRec(f, x, e1, (conversion e2 cont))
	
    | Plus(e1, e2) ->
      let r1 = fresh_var() in
      let r2 = fresh_var() in
      (conversion e1 (Fun(r1,(conversion e2 (Fun(r2,App(cont, (Plus(Var(r1), Var (r2))))))))))
    | Times(e1, e2) ->
      let r1 = fresh_var() in
      let r2 = fresh_var() in
      (conversion e1 (Fun(r1,(conversion e2 (Fun(r2,App(cont, (Times(Var(r1), Var (r2))))))))))
    | Minus(e1, e2) ->
      let r1 = fresh_var() in
      let r2 = fresh_var() in
      (conversion e1 (Fun(r1,(conversion e2 (Fun(r2,App(cont, (Minus(Var(r1), Var (r2))))))))))
    | Div(e1, e2) ->
      let r1 = fresh_var() in
      let r2 = fresh_var() in
      (conversion e1 (Fun(r1,(conversion e2 (Fun(r2,App(cont, (Div(Var(r1), Var (r2))))))))))	

    | Eq(e1, e2) ->
      let r1 = fresh_var() in
      let r2 = fresh_var() in
      (conversion e1 (Fun(r1,(conversion e2 (Fun(r2,App(cont, (Eq(Var(r1), Var (r2))))))))))
    | Greater(e1, e2) ->
      let r1 = fresh_var() in
      let r2 = fresh_var() in
      (conversion e1 (Fun(r1,(conversion e2 (Fun(r2,App(cont, (Greater(Var(r1), Var (r2))))))))))
    | Less(e1, e2) ->
      let r1 = fresh_var() in
      let r2 = fresh_var() in
      (conversion e1 (Fun(r1,(conversion e2 (Fun(r2,App(cont, (Less(Var(r1), Var (r2))))))))))
    | App(e1, e2) ->
      let r1 = fresh_var() in
      let r2 = fresh_var() in
      (conversion e1 
	 (Fun (r1, (conversion e2 
		      (Fun (r2, (App(Var(r1), App(cont, Var(r1))))))))))  
    | Empty -> Empty
