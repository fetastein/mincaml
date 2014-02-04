open List;;

type cam_value =  
  | CAM_IntVal  of int 
  | CAM_BoolVal of bool
  | CAM_ClosVal of cam_code * cam_env
and cam_stack = cam_value list
and cam_env = cam_value list 
and  cam_instr =    
  | CAM_Ldi of int  
  | CAM_Ldb of bool 
  | CAM_Access of int
  | CAM_Closure of cam_code	
  | CAM_Apply              
  | CAM_Return             
  | CAM_Let                
  | CAM_EndLet             
  | CAM_Test of cam_code * cam_code
  | CAM_Add                        
  | CAM_Eq                         
  | CAM_Minus
  | CAM_Times
  | CAM_Div
  | CAM_Greater
  | CAM_Less
and cam_code = cam_instr list ;; 

    (*実験テキストの表のc,c'はt,cに対応する*)
let rec cam_eval cam_code cam_env cam_stack  =
  match cam_code with
    | CAM_Ldi(n) :: t -> cam_eval t cam_env (append [CAM_IntVal(n)] cam_stack)
    | CAM_Ldb(b) :: t -> cam_eval t cam_env (append [CAM_BoolVal(b)] cam_stack) 
    | CAM_Access(i) :: t -> cam_eval t cam_env (append [(nth cam_env (i))] cam_stack) 
    | CAM_Closure(c) :: t -> cam_eval t cam_env (append [CAM_ClosVal(c, cam_env)] cam_stack) 
    | CAM_Apply :: t -> 
      begin
	match cam_stack with
	  | CAM_ClosVal(c, env) :: v :: s ->
	    cam_eval c (append [v ; CAM_ClosVal(c, env)] env) (append [CAM_ClosVal(t, cam_env)] s)
	  | _ -> failwith "Error in Apply"
      end
    | CAM_Return :: t -> 
      begin 
	match cam_stack with
	  | v :: CAM_ClosVal(c, env) :: s -> 
	    cam_eval c env (append [v] s)
	  | _ -> failwith "Error in Return"
      end
    | CAM_Let :: t ->
      begin 
	match cam_stack with
	  | v :: s ->
	    cam_eval t (append [v] cam_env) s
	  | _ -> failwith "Error in Let"
      end
    | CAM_EndLet :: t ->
      begin
	match cam_env with 
	  | v :: env -> 
	    cam_eval t env cam_stack
	  | _ -> failwith "Error in EndLet"
      end
    | CAM_Test(c1, c2) :: t ->
      begin
	match cam_stack with
	  | CAM_BoolVal(true) :: s -> cam_eval (append c1 t) cam_env s
	  | CAM_BoolVal(false) :: s -> cam_eval (append c2 t) cam_env s
	  | _ -> failwith "Error in Test"
      end
    | CAM_Add :: t ->
      begin match cam_stack with
	| CAM_IntVal(n1) :: CAM_IntVal(n2) :: s ->
	  cam_eval t cam_env (append [CAM_IntVal(n1+n2)] s)
	| _ -> failwith "Error in Add"
      end
    | CAM_Times :: t ->
      begin match cam_stack with
	| CAM_IntVal(n1) :: CAM_IntVal(n2) :: s ->
	  cam_eval t cam_env (append [CAM_IntVal(n1*n2)] s)
	| _ -> failwith "Error in Times"
      end
    | CAM_Minus :: t ->
      begin match cam_stack with
	| CAM_IntVal(n1) :: CAM_IntVal(n2) :: s ->
	  cam_eval t cam_env (append [CAM_IntVal(n1-n2)] s)
	| _ -> failwith "Error in Minus"
      end
    | CAM_Div :: t ->
      begin match cam_stack with
	| CAM_IntVal(n1) :: CAM_IntVal(n2) :: s ->
	  cam_eval t cam_env (append [CAM_IntVal(n1/n2)] s)
	| _ -> failwith "Error in Minus"
      end

    | CAM_Eq :: t ->
      begin match cam_stack with
	| CAM_IntVal(n1) :: CAM_IntVal(n2) :: s ->
	  cam_eval t cam_env (append [CAM_BoolVal(n1=n2)] s)
	| _ -> failwith "Error Eq"
      end

    | CAM_Greater :: t ->
      begin match cam_stack with
	| CAM_IntVal(n1) :: CAM_IntVal(n2) :: s ->
	  cam_eval t cam_env (append [CAM_BoolVal(n2>n1)] s)
	| _ -> failwith "Error in Greater"
      end
    | CAM_Less :: t ->
      begin match cam_stack with
	| CAM_IntVal(n1) :: CAM_IntVal(n2) :: s ->
	  cam_eval t cam_env (append [CAM_BoolVal(n2<n1)] s)
	| _ -> failwith "Error in Less"
      end
    | _ -> 
      begin match cam_stack with
	| v :: [] when cam_env = [] -> v
	| _ -> failwith "Error" 
      end
;;

