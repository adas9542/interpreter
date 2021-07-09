type stackValue = Bool of bool | Int of int | Error | Unit| String of string | Name of string 

type command = ADD | SUB | MUL | DIV | POP | PUSH of stackValue | QUIT | PRINTLN | TOSTRING | SWAP | NEG | REM | CAT | IF | BIND 
              | AND | OR | NOT | EQUAL | LESSTHAN | LET | END
let interpreter ( (input : string), (output : string )) : unit = 
  let ic = open_in input in
  let oc = open_out output in 
  let file_write cd = Printf.fprintf oc "%s\n" cd in
  let rec loop_read acc = 
    try 
          let l = String.trim(input_line ic) in loop_read (l::acc)
      with
      | End_of_file -> List.rev acc in
  (*need to fix stringtosv and stringtocommand *)

  let strList = loop_read[] in 

  let stringtosv (st:string) : stackValue = 
    let letters = Str.regexp "[a-zA-Z]" in
    let store = Str.regexp "[\"]" in 
    let first_char = st.[0] in
    match st with 
    | ":true:" -> Bool (true)
    | ":false:" -> Bool (false) 
    | ":error:" -> Error
    | ":unit:" -> Unit
    | _ ->   
            if Str.string_match letters st 0 || first_char = '_' then Name (st)
            else if Str.string_match store st 0 then String(String.sub st 1 ((String.length st)-2)) 
            else (try Int (int_of_string st) with
                    | Failure x -> Error)
    in        

  let stringtocommand (st:string) : command = 
    (* exception Invalid_input *)
    match st with 
    | "add" -> ADD
    | "sub" -> SUB
    | "mul" -> MUL 
    | "div" -> DIV
    | "quit" -> QUIT
    | "println" -> PRINTLN
    | "toString" -> TOSTRING
    | "swap" -> SWAP
    | "neg" -> NEG
    | "rem" -> REM
    | "pop" -> POP
    | "cat" -> CAT
    | "and" -> AND
    | "or" -> OR
    | "not" -> NOT
    | "equal" -> EQUAL
    | "lessThan" -> LESSTHAN
    | "if" -> IF
    | "bind" -> BIND
    | "let" -> LET
    | "end" -> END
    (* = vs ==  *)
    | _ -> if String.sub st 0 4 = "push" then PUSH(stringtosv (String.sub st 5 ((String.length st)-5)))     
    else invalid_arg "not valid input" 
  in

  (*the following svtostr and comtostr are for debugging purposes*)
  let svtostr (svlue:stackValue) :string = 
    match svlue with 
    | Bool b -> string_of_bool b
    | Int i -> string_of_int i
    | Error -> ":error:"
    | String s -> s 
    | Name n -> n
    | Unit -> ":unit:"

  in 

  (* let comtostr (comma:command) : string = 
     match comma with 
     | ADD -> "add"
     | SUB -> "sub"
     | MUL -> "mul"
     | DIV -> "div"
     | NEG -> "neg"
     | POP -> "pop"
     | CAT -> "cat"
     | PUSH pushval -> "push " ^ svtostr(pushval) 
     | QUIT -> "quit"
     | PRINTLN -> "println"
     | REM -> "rem"
     | TOSTRING -> "toString"
     | AND -> "and"
     | OR -> "or"
     | NOT -> "not"
     | SWAP -> "swap"
     | EQUAL -> "equal"
     | LESSTHAN -> "lessthan"
     | IF -> "if"
     | BIND -> "bind"
     | LET -> "let"
     | END -> "end"
  in
   *)
  let comList = List.map stringtocommand strList 
  
  in 

  (* function to view the list of commands  *)
  (* let rec viewcom (input: command list):unit = 
    match input with 
    | [] -> ()
    | hd::tl -> print_string (comtostr hd); print_string "\n"; viewcom(tl)

  in *)

  (* finds the value and returns it or the input *)
  let rec find (Name k) (givenlist) = 
    match givenlist with
    | ((Name(firstv),secondv)::hd)::tl -> if firstv = k then secondv 
                                    else find (Name(k)) (hd::tl)
    | []::tl -> find (Name(k)) (tl)
    | _ -> (Name k) 
in

let rec viewstack(input: stackValue list list) = 
  match input with
  | (firstelem::hd)::tl -> print_string (svtostr(firstelem)); viewstack (hd::tl)
  | []::tl -> viewstack (tl)
  | [] -> print_string "\n\n";()  

  in

(* let rec viewenv(input:(stackValue * stackValue) list list) = 
  match input with
  | ((one,two)::hd)::tl -> (match (one,two) with 
                            | (hd1,tl1) -> print_string (svtostr(one)); print_string "="; print_string (svtostr(two)); print_string "\n\n";viewenv (hd::tl))
                            | _-> () 
in *)

(* cl is the list of commands, stack is where the stackvalues are stored, env is where the bindings are stored*)
  let rec processor cl (stack: stackValue list list) (env) = 
  (* slowly build up the stack where you iterate through the cl and update the stack *)
  viewstack(stack);
  (* viewenv(env); *)

  match (cl, stack, env) with
  (*stops the execution of interpreter *)
  | (QUIT::restofCommands, _, _) -> ()
  (* pops a stackvalue from the stackvalue list *)
  | (POP::restofCommands, (firstitempop::hd)::restofStack, env) -> processor restofCommands ((hd)::restofStack) env
  (* performs the not operator with 1 bool otherwise error is pushed onto stack *) 
  | (NEG::restofCommands, (hdelem::hd)::restofStack, env) -> (match hdelem with 
                  | Int(numb) -> processor restofCommands ((Int(-1*numb)::hd)::restofStack) env
                  | Name(givenname) -> (match (find(Name(givenname)) env ) with 
                            | Name(givenname) -> processor restofCommands ((Error::hdelem::hd)::restofStack) env
                            | Int(number) -> processor restofCommands ((Int(-1*number)::hd)::restofStack) env
                            | _ -> processor restofCommands ((Error::hdelem::hd)::restofStack) env)
                  | _-> processor restofCommands ((Error::hdelem::hd)::restofStack) env) 
    (* only converts valid commands into strings or if there's an element in the stack environment *)
  | (TOSTRING::restofCommands,stack, env) -> (match stack with 
                  | []::stack -> processor restofCommands (((Error)::[])::stack) env
                  | (firstelem::hd)::restofStack -> (match firstelem with
                            | Bool(bval) -> processor restofCommands ((String (":" ^ (svtostr(Bool(bval)) ^ ":" ))::hd)::restofStack) env
                            | _ -> processor restofCommands ((String (svtostr(firstelem))::hd)::restofStack) env )
                  | _ -> processor restofCommands (((Error)::[])::stack) env)
  (* pushes a stackvalue onto the stackvalue list *)
  | (PUSH(item)::restofCommands, hd::stack, env) -> processor restofCommands ((item::hd)::stack) env
  (*prints strings onto the output.txt *)
  | (PRINTLN::restofCommands, (firstval::hd)::restofStack, env) -> (match firstval with 
                  | String first -> file_write first; processor restofCommands ((hd)::restofStack) env
                  | _ -> processor restofCommands ((Error::firstval::hd)::restofStack) env)
  (* performs the not operator with 1 bool otherwise error is pushed onto stack *)              
  | (NOT::restofCommands, (firsteleme::hd):: restofStack, env) -> (match firsteleme with
                  | Bool b -> processor restofCommands (((Bool(not b))::hd)::restofStack) env
                  | Name b ->  (match (find(Name(b)) env ) with 
                            | Name(givenname) -> processor restofCommands ((Error::firsteleme::hd)::restofStack) env
                            | Bool d -> processor restofCommands (((Bool(not d))::hd)::restofStack) env
                            | _ -> processor (NOT::restofCommands) (((find (Name(b)) env )::hd)::restofStack) env)
                  | _ -> processor restofCommands ((Error::firsteleme::hd)::restofStack) env)
  (* find if the vlue for the Name(fvalue) exists, if it exists then have Name b = that value otherwise return error*)
  | (BIND::restofCommands, (Name(fvalue)::Name(b)::hd)::restofStack, hdenv::tlenv) -> (match (find (Name(fvalue)) env) with
                  | Int(newvalue) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Int(newvalue))::hdenv):: tlenv)
                  | Bool(newvalue) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Bool(newvalue)) ::hdenv):: tlenv)
                  | String(newvalue) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), String(newvalue)) ::hdenv):: tlenv)
                  | Unit -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Unit) :: hdenv):: tlenv)
                  | _ -> processor restofCommands ((Error::Name(fvalue)::Name b::hd)::restofStack) env) 
  | (BIND::restofCommands, (Name(fvalue)::Name(b)::hd)::restofStack, env) -> (match (find (Name(fvalue)) env) with
                  | Int(newvalue) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Int(newvalue))::[]):: env)
                  | Bool(newvalue) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Bool(newvalue)) ::[]):: env)
                  | String(newvalue) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), String(newvalue)) ::[]):: env)
                  | Unit -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Unit) :: []):: env)
                  | _ -> processor restofCommands ((Error::Name(fvalue)::Name b::hd)::restofStack) env) 
  (* checks if the environment stack head is empty or not and then performs binding on the 2 nonnames *)
  | (BIND::restofCommands, (firstvalue::secondvalue::hd)::restofStack, hdenv::tlenv) -> (match (firstvalue,secondvalue) with
                  | (Int(fvalue), Name(b)) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Int(fvalue)) :: hdenv):: tlenv)
                  | (Bool(fvalue),Name(b)) -> processor restofCommands ((Unit::hd)::restofStack) (((secondvalue, firstvalue) :: hdenv) ::tlenv);
                  | (String(fvalue),Name(b)) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), String(fvalue)) :: hdenv) ::tlenv)
                  | (Unit,Name(b)) -> processor restofCommands ((Unit::hd)::restofStack) (((secondvalue, firstvalue) :: hdenv):: tlenv)
                  | _-> processor restofCommands ((Error::firstvalue::secondvalue::hd)::restofStack) env)
  (* this next case is here because what if there are no preexisting bindings in the env *)
  | (BIND::restofCommands, (firstvalue::secondvalue::hd)::restofStack, env) -> (match (firstvalue,secondvalue) with
                  | (Int(fvalue), Name(b)) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Int(fvalue)) :: []) :: env)
                  | (Bool(fvalue),Name(b)) ->  processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Bool(fvalue)) :: []) ::env)
                  | (String(fvalue),Name(b)) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), String(fvalue)) :: []) ::env)
                  | (Unit,Name(b)) -> processor restofCommands ((Unit::hd)::restofStack) (((Name(b), Unit) :: []):: env)
                  | _-> processor restofCommands ((Error::firstvalue::secondvalue::hd)::restofStack) env)  
    (* concatenates the first 2 elements if they are strings *)
  | (CAT::restofCommands, (firstelement::secondelement::hd)::restofStack, env) -> (match (firstelement,secondelement) with
                  | (String(a),String(b))-> processor restofCommands ((String(b ^ a)::hd)::restofStack) env                                                 
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                  | (String(a),String(b)) -> processor restofCommands ((String(b ^ a)::hd) ::restofStack) env
                                  | _ -> processor restofCommands ((Error::firstelement::secondelement::hd)::restofStack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                  | (String(a),String(b)) -> processor restofCommands ((String(b ^ a)::hd) ::restofStack) env
                                  | _ -> processor restofCommands ((Error::firstelement::secondelement::hd):: restofStack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                  | (String(a),String(b)) -> processor restofCommands ((String(b ^ a)::hd) ::restofStack) env
                                  | _ -> processor restofCommands ((Error::firstelement::secondelement::hd):: restofStack) env)
                  | (_,_) -> processor restofCommands ((Error::firstelement::secondelement::hd) :: restofStack) env)
    (* performs the and operator with 2 bools otherwise error is pushed onto stack *)
  | (AND::restofCommands, (firstelement::secondelement::hd)::restofStack, env) -> (match (firstelement,secondelement) with
                  | (Bool b, Bool c)-> processor restofCommands (((Bool(b && c))::hd)::restofStack) env
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                        | (Bool(a),Bool(b)) -> processor restofCommands (((Bool(a && b))::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelement::secondelement::hd):: restofStack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                        | (Bool(a),Bool(b)) -> processor restofCommands (((Bool(a && b))::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelement::secondelement::hd)::restofStack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                        | (Bool(a),Bool(b)) -> processor restofCommands (((Bool(a && b))::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelement::secondelement::hd):: restofStack) env) 
                  | (_,_) -> processor restofCommands ((Error::firstelement::secondelement::hd):: restofStack) env)
  (* performs the or operator with 2 bools otherwise error is pushed onto stack *)
  | (OR::restofCommands, (firstelement :: secondelement :: hd)::restofStack, env) -> (match (firstelement,secondelement) with
                  | (Bool b, Bool c)-> processor restofCommands ((Bool(b || c)::hd)::restofStack) env
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                    | (Bool(a),Bool(b)) -> processor restofCommands ((Bool(a || b)::hd)::restofStack) env
                                    | _ -> processor restofCommands ((Error::firstelement::secondelement::hd) :: restofStack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                    | (Bool(a),Bool(b)) -> processor restofCommands ((Bool(a || b)::hd)::restofStack) env
                                    | _ -> processor restofCommands ((Error::firstelement::secondelement::hd) :: restofStack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                    | (Bool(a),Bool(b)) -> processor restofCommands ((Bool(a || b)::hd)::restofStack) env
                                    | _ -> processor restofCommands ((Error::firstelement::secondelement::hd) :: restofStack) env) 
                  | (_,_) -> processor restofCommands ((Error::firstelement::secondelement::hd) :: restofStack) env)
  (*checks if the first two elements are equal in value *)
  | (EQUAL::restofCommands, (firstelem::secondelem::hd)::restofStack, env) -> (match (firstelem, secondelem) with
                  | (Int(a),Int(b))-> processor restofCommands (((Bool(a=b))::hd)::restofStack) env
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                    | (Int(a),Int(b)) -> processor restofCommands (((Bool(a=b))::hd)::restofStack) env
                                    | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                    | (Int(a),Int(b)) -> processor restofCommands (((Bool(a=b))::hd)::restofStack) env
                                    | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                    | (Int(a),Int(b)) -> processor restofCommands (((Bool(a=b))::hd)::restofStack) env
                                    | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (_,_) -> processor restofCommands ((Error::hd)::stack) env)
  (*checks if the firstelement is less than the second element *)
  | (LESSTHAN::restofCommands, (firstelem::secondelem::hd)::restofStack, env) -> (match (firstelem, secondelem) with
                  | (Int(a),Int(b))->  processor restofCommands (((Bool(a>b))::hd)::restofStack) env
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                  | (Int(a),Int(b)) -> processor restofCommands (((Bool(a>b))::hd)::restofStack) env
                                  | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                  | (Int(a),Int(b)) -> processor restofCommands (((Bool(a>b))::hd)::restofStack) env
                                  | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                  | (Int(a),Int(b)) -> processor restofCommands (((Bool(a>b))::hd)::restofStack) env
                                  | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (_,_) -> processor restofCommands ((Error::hd)::stack) env)
  (*swaps the first two element*)
  | (SWAP::restofCommands, (firstitem::seconditem::hd)::restofStack, env) -> processor restofCommands ((seconditem::firstitem::hd)::restofStack) env             
  (*takes addition into account *)
  | (ADD::restofCommands, (firstelem::secondelem::hd)::restofStack, env) -> (match (firstelem, secondelem) with
                  | (Int(a),Int(b))-> processor restofCommands ((Int(a+b)::hd)::restofStack) env
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                      | (Int(a), Int(b)) -> processor restofCommands ((Int(a+b)::hd)::restofStack) env
                                      (* a argument = Name c; c argument = 3(the value of the Name a)*)
                                      | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                                      (* error in the above line so fix it pleaseeeeeeee*)
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                      | (Int(a), Int(b)) -> processor restofCommands ((Int(a+b)::hd)::restofStack) env
                                      | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                      | (Int(a), Int(b)) -> processor restofCommands ((Int(a+b)::hd)::restofStack) env
                                      | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (_,_) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env)
  (*takes subtraction into account *)
  | (SUB::restofCommands, (firstelem::secondelem::hd)::restofStack, env) ->  (match (firstelem, secondelem) with
                  | (Int(a),Int(b))-> processor restofCommands ((Int(b-a)::hd)::restofStack) env
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                      | (Int(a), Int(b)) -> processor restofCommands ((Int(b-a)::hd)::restofStack) env
                                      | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                      | (Int(a), Int(b)) -> processor restofCommands ((Int(b-a)::hd)::restofStack) env
                                      | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                      | (Int(a), Int(b)) -> processor restofCommands ((Int(b-a)::hd)::restofStack) env
                                      | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (_,_) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env)
  (*takes multiplication into account *)
  | (MUL::restofCommands, (firstelem::secondelem::hd)::restofStack, env) -> (match (firstelem, secondelem) with
                  | (Int(a),Int(b))-> processor restofCommands ((Int(a*b)::hd)::restofStack) env
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(a*b)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(a*b)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(a*b)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (_,_) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env)
  (* these 2 cases take care of division by zero if both values in the stack are numbers  *)
  | (REM::restofCommands, (Int(0)::Int(b)::hd)::restofStack, env) -> processor restofCommands ((Error::Int(0)::Int(b)::hd)::stack) env
  | (DIV::restofCommands, (Int(0)::Int(b)::hd)::restofStack, env) -> processor restofCommands ((Error::Int(0)::Int(b)::hd)::stack) env
  | (REM::restofCommands, (firstelem::secondelem::hd)::restofStack, env) -> (match (firstelem, secondelem) with
                  (* first cases in the following statements tries to avoid division by zero *)
                  | (Int(a),Int(b))-> processor restofCommands ((Int(b mod a)::hd)::restofStack) env
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                        | (Int(0), _) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env  
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(b mod a)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                        | (Int(0), y) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env  
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(b mod a)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                        | (x, Name(b)) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env 
                                        | (Int(0), _) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env 
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(b mod a)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (_,_) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env)
  | (DIV::restofCommands, (firstelem::secondelem::hd)::restofStack, env) -> (match (firstelem, secondelem) with
                  | (Int(a),Int(b))-> processor restofCommands ((Int(b/a)::hd)::restofStack) env
                  (* first cases in the following statements tries to avoid division by zero *)
                  | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                        | (Int(0), _) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env   
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(b/a)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                        | (Int(0),y) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(b/a)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                        | (Int(0), _) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env
                                        | (Int(a), Int(b)) -> processor restofCommands ((Int(b/a)::hd)::restofStack) env
                                        | _ -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env) 
                  | (_,_) -> processor restofCommands ((Error::firstelem::secondelem::hd)::stack) env)
  (* if statement for now checks if the third element is true or not *)
  | (IF::restofCommands, (x::y::third::hd)::restofStack, env) -> (match third with
                  | Bool(true) -> processor restofCommands ((x::hd)::restofStack) env
                  | Bool(false) -> processor restofCommands ((y::hd)::restofStack) env
                  | Name(a) -> (match (find (Name(a)) env ) with
                                | Bool(true) -> processor restofCommands ((x::hd)::restofStack) env
                                | Bool(false) -> processor restofCommands ((y::hd)::restofStack) env
                                | _-> processor restofCommands ((Error::x::y::third::hd)::restofStack) env)
                  | _ -> processor restofCommands ((Error::x::y::third::hd)::restofStack) env)
  (* beginning of an opening clause *)
  | (LET::restofCommands, stack ,env) -> print_string "let"; processor restofCommands ([]::stack) ([]::env)
  (* ending of an opening clause *)
  | (END::restofCommands, hds::restofStack , hde::env) -> print_string "end"; (match (hds, restofStack) with
                  | ([], restofinnerStack) -> processor restofCommands restofinnerStack env
                  | (hd1::tl,hd2::restofinnerStack) -> processor restofCommands ((hd1::hd2)::restofinnerStack) env
                  | (_,_) -> processor restofCommands restofStack env)
  (* if there is nothing in the stack*)       
  | (c::restofCommands,(firstelem::secondelem::hd)::restofStack, env) -> processor restofCommands ((Error::hd)::restofStack) env 
    (*all the cases that could not be matched by the previous cases *)
  | (_,_, _) -> print_string "hello";()
  
  in

processor comList [[]] [[]];;

interpreter("input24.txt", "output.txt")