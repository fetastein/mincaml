open Syntax ;;

(*type exp =
  | IntLit of int
  | BoolLit of bool
  | Plus of exp * exp 
  | If of exp * exp * exp
  | Eq of exp * exp 
  | Var of string
  | Fun of string * exp   (* fun x -> e *)
  | App of exp * exp      (* function application i.e. e e *)
*)
type ty = TInt | TBool | TArrow of ty * ty

let ext env x v = (x,v) :: env;;

let rec lookup x env =                                                           
   match env with
     | [] -> failwith ("unbound variable: " ^ x)
     | (y,v)::tl -> if x=y then v                                 
       else lookup x tl ;;


(* tcheck1 : exp -> ty *)
let rec tcheck1  e =
  match e with
    | IntLit(_)    -> TInt
    | BoolLit(_)   -> TBool
    | Plus(e1,e2)  -> 
      begin
        match (tcheck1 e1, tcheck1 e2) with
            (TInt,TInt) -> TInt
          | _ -> failwith "type error in Plus"
      end
    | If(e1,e2,e3) -> 
      begin
        match (tcheck1 e1, tcheck1 e2, tcheck1 e3) with
            (TBool,TInt,TInt) -> TInt
          | (TBool,TBool,TBool) -> TBool
          | _ -> failwith "type error in IF"
      end
    | Eq(e1, e2) -> 
      begin
	match   (tcheck1 e1, tcheck1 e2) with
	  |(TBool, TBool) -> TBool
	  |(TInt, TInt) -> TBool
	  | _ -> failwith "type error in EQ"
      end
    | _ -> failwith "unknown expression"
      
let emptyenv () = [];;

(* tcheck1 : (string * ty) list -> exp -> ty *)
let rec tcheck2 te e =
  match e with
    | Var(s) -> lookup s te
    | IntLit(_)    -> TInt
    | BoolLit(_)   -> TBool
    | Plus(e1,e2)  -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2) with
            (TInt,TInt) -> TInt
          | _ -> failwith "type error in Plus"
      end
    | If(e1,e2,e3) -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2, tcheck2 te e3) with
            (TBool,TInt,TInt) -> TInt
          | (TBool,TBool,TBool) -> TBool
          | _ -> failwith "type error in IF"
      end
    | Eq(e1, e2) -> 
      begin
	match   (tcheck2 te e1, tcheck2 te e2) with
	  |(TBool, TBool) -> TBool
	  |(TInt, TInt) -> TBool
	  | _ -> failwith "type error in EQ"
      end
    | _ -> failwith "unknown expression"

(* tcheck2 : (string * ty) list -> exp -> ty *)
let rec tcheck2 te e =
  match e with
    | Var(s) -> lookup s te
    | IntLit(_)    -> TInt
    | BoolLit(_)   -> TBool
    | Plus(e1,e2)  -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2) with
            (TInt,TInt) -> TInt
          | _ -> failwith "type error in Plus"
      end
    | If(e1,e2,e3) -> 
      begin
        match (tcheck2 te e1, tcheck2 te e2, tcheck2 te e3) with
            (TBool,TInt,TInt) -> TInt
          | (TBool,TBool,TBool) -> TBool
          | _ -> failwith "type error in IF"
      end
    | Eq(e1, e2) -> 
      begin
	match   (tcheck2 te e1, tcheck2 te e2) with
	  |(TBool, TBool) -> TBool
	  |(TInt, TInt) -> TBool
	  | _ -> failwith "type error in EQ"
      end
    | _ -> failwith "unknown expression"

(* tcheck3 : (string * ty) list -> exp -> ty *)
let rec tcheck3 te e =
  match e with
    | Var(s) -> lookup s te
    | IntLit(_)    -> TInt
    | BoolLit(_)   -> TBool
    | Plus(e1,e2)  -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
            (TInt,TInt) -> TInt
          | _ -> failwith "type error in Plus"
      end
    | Times(e1,e2)  -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2) with
            (TInt,TInt) -> TInt
          | _ -> failwith "type error in Times"
      end
    | If(e1,e2,e3) -> 
      begin
        match (tcheck3 te e1, tcheck3 te e2, tcheck3 te e3) with
	  | (t1, t2, t3) when (t2 = t3)-> t2 
          | _ -> failwith "type error in IF"
      end
    | Eq(e1, e2) -> 
      begin
	match   (tcheck3 te e1, tcheck3 te e2) with
	  |(TBool, TBool) -> TBool
	  |(TInt, TInt) -> TBool
	  | _ -> failwith "type error in EQ"
      end
    | Fun(x, e1) ->
      let t1 = lookup x te in
      let t2 = tcheck3 te e1 in 
      TArrow(t1,t2)
    | App(e1,e2) ->
      let t1 = tcheck3 te e1 in
      let t2 = tcheck3 te e2 in 
      begin
        match t1 with
	  | TArrow(t10,t11) -> if t2=t10 then t11
            else failwith "type error in App"
	  | _ -> failwith "type error in App"
      end
    | Let(x, e1, e2) ->
      let t1 = lookup x te in
      let t2 = tcheck3 te e1 in
      let t3 = tcheck3 te e2 in
      t3
    | _ -> failwith "unknown expression"
;;
