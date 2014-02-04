open Syntax;;
open Parser;;
open Lexer;;



let ext env x v = (x,v) :: env;;                                                 

let rec lookup x env =                                                           
   match env with                                                                
     | [] -> failwith ("unbound variable: " ^ x)                                 
     | (y,v)::tl -> if x=y then v                                                
       else lookup x tl ;; 


let rec eval e env =
  let binop f e1 e2 env =
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match (v1, v2) with
    | (IntVal(n1),IntVal(n2)) -> IntVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in 
  let binop_bool f e1 e2 env = 
    let v1 = eval e1 env in
    let v2 = eval e2 env in
    match (v1, v2) with
    | (IntVal(n1),IntVal(n2)) -> BoolVal(f n1 n2)
    | _ -> failwith "integer value expected"
  in 
  match e with
  | Var(x)       -> lookup x env 
  | IntLit(n)    -> IntVal(n)
  | Plus(e1,e2)  -> binop (+) e1 e2 env
  | Times(e1,e2) -> binop ( * ) e1 e2 env
  | Minus(e1,e2) -> binop (-) e1 e2 env
  | Eq(e1,e2)    ->
    begin
      let v1 = eval e1 env in
      let v2 = eval e2 env in
      match (v1, v2) with
	| (IntVal(n1),IntVal(n2)) -> BoolVal( n1 =n2)
	| (BoolVal(b1),BoolVal(b2)) -> BoolVal( b1 = b2)
	| _ -> failwith "integer value expected"
    end 
  | Neq(e1, e2)  -> binop_bool (!=) e1 e2 env
  | If(e1,e2,e3) ->
    begin
      match (eval e1 env) with
	| BoolVal(true)  -> eval e2 env
	| BoolVal(false) -> eval e3 env
	| _ -> failwith "wrong value"
    end
  | Let(x,e1,e2) ->  
    let env1 = ext env x (eval e1 env) 
    in eval e2 env1
  | LetRec(f,x,e1,e2) ->
    let env1 = ext env f (RecFunVal (f, x, e1, env))
    in eval e2 env1
  | Fun(x, e1) -> FunVal(x, e1, env)
  | App(e1,e2) -> 
    let funpart = (eval e1 env) in
    let arg = (eval e2 env) in
    begin
      match funpart with
        | FunVal(x,body,env1) ->
          let env2 = (ext env1 x arg) in
          eval body env2
        | RecFunVal(f,x,body,env1) ->
          let env2 = (ext (ext env1 x arg) f funpart) in
          eval body env2
        | _ -> failwith "wrong value in App"
    end
  | Empty -> 
    ListVal([])
  | Cons(e1,e2) ->
    begin
      match (eval e1 env, eval e2 env) with
	| (v1,ListVal(v2)) -> ListVal(v1 :: v2)
    end
  | Head(e) -> 
    begin 
      match (eval e env) with
	| ListVal(v1 :: v2) -> v1
	| _ -> failwith "wrong value"
    end
  | Tail(e) ->
    begin 
      match (eval e env) with
	| ListVal(v1 :: v2) -> ListVal(v2)
	| _ -> failwith "wrong value"
    end
  | _ -> failwith "unknown expression"
;;


