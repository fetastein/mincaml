open List;;

type zam_value =  
  | ZAM_IntVal  of int 
  | ZAM_BoolVal of bool
  | ZAM_ClosVal of zam_code * zam_env
  | ZAM_Epsilon
and zam_stack = zam_value list
and zam_env = zam_value list 
and  zam_instr =    
  | ZAM_Ldi of int  
  | ZAM_Ldb of bool 
  | ZAM_Access of int
  | ZAM_Closure of zam_code	

  | ZAM_Let                
  | ZAM_EndLet             
  | ZAM_Test of zam_code * zam_code
  | ZAM_Add                        

  | ZAM_Eq                         
  | ZAM_Minus
  | ZAM_Times
  | ZAM_Greater
  | ZAM_Less
  | ZAM_Apply              
  | ZAM_TailApply
  | ZAM_PushMark
  | ZAM_Grab
  | ZAM_Return
and zam_code = zam_instr list ;; 

let rec zam_eval zam_code zam_env zam_stack zam_retst =
  match zam_code with
    | ZAM_Ldi(n) :: t -> zam_eval t zam_env (append [ZAM_IntVal(n)] zam_stack) zam_retst
    | ZAM_Ldb(b) :: t -> zam_eval t zam_env (append [ZAM_BoolVal(b)] zam_stack) zam_retst
    | ZAM_Access(i) :: t -> zam_eval t zam_env (append [(nth zam_env (i))] zam_stack) zam_retst
    | ZAM_Closure(c) :: t -> zam_eval t zam_env (append [ZAM_ClosVal(c, zam_env)] zam_stack) zam_retst
    | ZAM_Let :: t ->
      begin 
	match zam_stack with
	  | v :: s ->
	    zam_eval t (append [v] zam_env) s zam_retst
	  | _ -> failwith "Error in Let"
      end
    | ZAM_EndLet :: t ->
      begin
	match zam_env with 
	  | v :: env -> 
	    zam_eval t env zam_stack zam_retst
	  | _ -> failwith "Error in EndLet"
      end
    | ZAM_Test(c1, c2) :: t ->
      begin
	match zam_stack with
	  | ZAM_BoolVal(true) :: s -> zam_eval (append c1 t) zam_env s zam_retst
	  | ZAM_BoolVal(false) :: s -> zam_eval (append c2 t) zam_env s zam_retst
	  | _ -> failwith "Error in Test"
      end
    | ZAM_Add :: t ->
      begin match zam_stack with
	| ZAM_IntVal(n1) :: ZAM_IntVal(n2) :: s ->
	  zam_eval t zam_env (append [ZAM_IntVal(n1+n2)] s) zam_retst
	| _ -> failwith "Error in Add"
      end
    | ZAM_Times :: t ->
      begin match zam_stack with
	| ZAM_IntVal(n1) :: ZAM_IntVal(n2) :: s ->
	  zam_eval t zam_env (append [ZAM_IntVal(n1*n2)] s) zam_retst
	| _ -> failwith "Error in Times"
      end
    | ZAM_Minus :: t ->
      begin match zam_stack with
	| ZAM_IntVal(n1) :: ZAM_IntVal(n2) :: s ->
	  zam_eval t zam_env (append [ZAM_IntVal(n1-n2)] s) zam_retst
	| _ -> failwith "Error in Minus"
      end
    | ZAM_Eq :: t ->
      begin match zam_stack with
	| ZAM_IntVal(n1) :: ZAM_IntVal(n2) :: s ->
	  zam_eval t zam_env (append [ZAM_BoolVal(n1=n2)] s) zam_retst
	| _ -> failwith "Error Eq"
      end

    | ZAM_Greater :: t ->
      begin match zam_stack with
	| ZAM_IntVal(n1) :: ZAM_IntVal(n2) :: s ->
	  zam_eval t zam_env (append [ZAM_BoolVal(n1>n2)] s) zam_retst
	| _ -> failwith "Error in Greater"
      end
    | ZAM_Less :: t ->
      begin match zam_stack with
	| ZAM_IntVal(n1) :: ZAM_IntVal(n2) :: s ->
	  zam_eval t zam_env (append [ZAM_BoolVal(n1<n2)] s) zam_retst
	| _ -> failwith "Error in Less"
      end
	
    | ZAM_Apply :: t -> 
      begin
	match zam_stack with
	  | ZAM_ClosVal(c, env) :: v :: s ->
	    zam_eval c  (append [v ; ZAM_ClosVal(c, env)] env) s (append [ZAM_ClosVal(t, zam_env)] zam_retst)
	  | _ -> failwith "Error in Apply"
      end


    | ZAM_TailApply :: t -> 
      begin
	match zam_stack with
	  | ZAM_ClosVal(c, env) :: v :: s ->
	    zam_eval c (append [v ; ZAM_ClosVal(c, env)] env)  s zam_retst
	  | _ -> failwith "Error in TailApply"
      end

    | ZAM_PushMark :: t ->
      zam_eval t zam_env (append [ZAM_Epsilon] zam_stack) zam_retst

    | ZAM_Grab :: t ->
      begin
	match zam_stack with
	  | ZAM_Epsilon :: s ->
	    begin
	      match zam_retst with
		| ZAM_ClosVal(c, env) :: r-> 
		  zam_eval c env (append [ZAM_ClosVal(t, zam_env)] s) r
	    end
	  | v :: s -> 
	    zam_eval t (append [v ; ZAM_ClosVal(t, zam_env)] zam_env) s zam_retst
      end
	
    | ZAM_Return :: t ->
      begin
	match zam_stack with
	  |v :: ZAM_Epsilon :: s -> 
	    begin 
		match zam_retst with
		  | ZAM_ClosVal(c, env) :: r ->  zam_eval c env (append [v] s) r
		  | _ -> failwith "Error in ZAM_Retrun_1"
	    end
	  | ZAM_ClosVal(c, env) :: v :: s ->
	    zam_eval c (append [v; ZAM_ClosVal(c, env)] env) s zam_retst
	  | _ -> failwith "Error in ZAM_Retrun_2"
      end
	
    | _ -> zam_stack
    (*  begin match zam_stack with
	| v :: [] when zam_env = [] -> v
	| _ -> failwith "Error" 
      end*)
;;

