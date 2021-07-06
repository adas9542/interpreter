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
  in *)
  
    (* converts input string list to command list *)
(* 
  let rec view (input:string list):unit =
    match input with
    | [] -> ()
    | hd::tl -> print_string hd; print_string "\n"; view(tl)
  in  *)

  let comList = List.map stringtocommand strList in 

  (* function to view the list of commands 
  let rec viewcom (input: command list):unit = 
    match input with 
    | [] -> ()
    | hd::tl -> print_string (comtostr hd); print_string "\n"; viewcom(tl)

  in *)

  (* finds the value and returns it *)
  let rec find (Name k) (givenlist) = 
    match givenlist with
    | (Name(firstv),secondv)::tl -> if firstv = k then secondv 
                                    else find (Name(k)) tl
    | _ -> (Name k) 
      (* (invalid_arg "not valid input") *)
in

  (* cl is the list of commands, stack is where the stackvalues are stored, env is where the bindings are stored*)
   let rec processor cl stack env = 
    (* slowly build up the stack where you iterate through the cl and update the stack *)
    match (cl, stack, env) with
    | (QUIT::restofCommands, _, _) -> ()
    (* | (LET::restofCommands, restofStack, env) -> processor restofCommands ([]::restofStack) (insert (Let) ([]) env) *)
    | (POP::restofCommands, firstitempop::restofStack, env) -> processor restofCommands restofStack env
    | (NEG::restofCommands, hdelem::restofStack, env) -> (match hdelem with 
                                                    | Int(numb) -> processor restofCommands (Int(-1*numb)::restofStack) env
                                                    | Name(givenname) -> (match (find(Name(givenname)) env ) with 
                                                                          | Name(givenname) -> processor restofCommands (Error::hdelem::restofStack) env
                                                                          | _ -> processor (NEG::restofCommands) ((find(Name(givenname)) env)::restofStack) env)
                                                    | _-> processor restofCommands (Error::hdelem::restofStack) env) 
                                                         (* find the value associated with the names here*)
    | (TOSTRING::restofCommands,firstelem::restofStack, env) -> (match firstelem with 
                                                    | Bool(bval) -> processor restofCommands (String (":" ^ (svtostr(Bool(bval)) ^ ":" ))::restofStack) env
                                                    | _ -> processor restofCommands (String (svtostr(firstelem))::restofStack) env )
    | (PUSH(item)::restofCommands, stack, env) -> print_string "PUSH"; print_string (svtostr item); print_string "\n"; print_string "\n";processor restofCommands (item::stack) env
    | (PRINTLN::restofCommands, firstval::restofStack, env) -> (match firstval with 
                                                    | String first -> file_write first; processor restofCommands restofStack env
                                                    | _ -> processor restofCommands (Error::firstval::restofStack) env)
    | (NOT::restofCommands, firsteleme :: restofStack, env) -> (match firsteleme with
                                                    | Bool b -> processor restofCommands ((Bool(not b))::restofStack) env
                                                    | Name b ->  (match (find(Name(b)) env ) with 
                                                              | Name(givenname) -> processor restofCommands (Error::firsteleme :: restofStack) env
                                                              | _ -> processor (NOT::restofCommands) ((find (Name(b)) env )::restofStack) env)
                                                    | _ -> processor restofCommands (Error::firsteleme :: restofStack) env)
    (* bottom are functions that require 2 arguments *)
    (* find if the vlue for the Name(fvalue) exists, if it exists then have Name b = that value otherwise return error*)
    | (BIND::restofCommands, Name(fvalue)::Name b::restofStack, env) -> (match (find (Name(fvalue)) env) with
                                                    | Int(newvalue) -> processor restofCommands ((Unit)::restofStack) ((Name(b), Int(newvalue)) :: env)
                                                    | Bool(newvalue) -> processor restofCommands ((Unit)::restofStack) ((Name(b), Bool(newvalue)) :: env)
                                                    | String(newvalue) -> processor restofCommands ((Unit)::restofStack) ((Name(b), String(newvalue)) :: env)
                                                    | Unit -> processor restofCommands ((Unit)::restofStack) ((Name(b), Unit) :: env)
                                                    | _ -> processor restofCommands (Error::Name(fvalue)::Name b::restofStack) env)     
    | (BIND::restofCommands, firstval::secondval::restofStack, env) -> (match (firstval,secondval) with
                                                    | (Int(fvalue), Name(b)) -> processor restofCommands ((Unit)::restofStack) ((Name(b), Int(fvalue)) :: env)
                                                    | (Bool(fvalue),Name(b)) ->  processor restofCommands ((Unit)::restofStack) ((Name(b), Bool(fvalue)) :: env)
                                                    | (String(fvalue),Name(b)) -> processor restofCommands ((Unit)::restofStack) ((Name(b), String(fvalue)) :: env)
                                                    | (Unit,Name(b)) -> processor restofCommands ((Unit)::restofStack) ((Name(b), Unit) :: env)
                                                    | _-> processor restofCommands (Error::firstval::secondval :: restofStack) env)

    | (CAT::restofCommands, firstelement::secondelement::restofStack, env) -> (match (firstelement,secondelement) with
                                                    | (String(a),String(b))-> processor restofCommands (String(a ^ b) ::restofStack) env                                                    (*what if neither evaluates to anything besides a name what if one evaluates to a string and the other one doesnt*)
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                    | (String(a),String(b)) -> processor restofCommands (String(a ^ b) ::restofStack) env
                                                                    | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                    | (String(a),String(b)) -> processor restofCommands (String(a ^ b) ::restofStack) env
                                                                    | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                    | (String(a),String(b)) -> processor restofCommands (String(a ^ b) ::restofStack) env
                                                                    | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env)
                                                    | (_,_) -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env)
    | (AND::restofCommands, firstelement::secondelement :: restofStack, env) -> (match (firstelement,secondelement) with
                                                    | (Bool b, Bool c)-> print_string "AND"; processor restofCommands ((Bool(b && c))::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                          | (Bool(a),Bool(b)) -> processor restofCommands ((Bool(a && b))::restofStack) env
                                                                          | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                          | (Bool(a),Bool(b)) -> processor restofCommands ((Bool(a && b))::restofStack) env
                                                                          | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                          | (Bool(a),Bool(b)) -> processor restofCommands ((Bool(a && b))::restofStack) env
                                                                          | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env) 
                                                    | (_,_) -> print_string "here"; processor restofCommands (Error::firstelement::secondelement :: restofStack) env)
    | (OR::restofCommands, firstelement :: secondelement :: restofStack, env) -> (match (firstelement,secondelement) with
                                                    | (Bool b, Bool c)-> processor restofCommands (Bool(b || c)::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                      | (Bool(a),Bool(b)) -> processor restofCommands (Bool(a || b)::restofStack) env
                                                                      | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                      | (Bool(a),Bool(b)) -> processor restofCommands (Bool(a || b)::restofStack) env
                                                                      | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                      | (Bool(a),Bool(b)) -> processor restofCommands (Bool(a || b)::restofStack) env
                                                                      | _ -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env) 
                                                    | (_,_) -> processor restofCommands (Error::firstelement::secondelement :: restofStack) env)
    | (EQUAL::restofCommands, firstelem::secondelem::restofStack, env) -> (match (firstelem, secondelem) with
                                                    | (Int(a),Int(b))-> processor restofCommands ((Bool(a=b))::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                      | (Int(a),Int(b)) -> processor restofCommands ((Bool(a=b))::restofStack) env
                                                                      | _ -> processor restofCommands (Error::stack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                      | (Int(a),Int(b)) -> processor restofCommands ((Bool(a=b))::restofStack) env
                                                                      | _ -> processor restofCommands (Error::stack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                      | (Int(a),Int(b)) -> processor restofCommands ((Bool(a=b))::restofStack) env
                                                                      | _ -> processor restofCommands (Error::stack) env) 
                                                    | (_,_) -> processor restofCommands (Error::stack) env)
    | (LESSTHAN::restofCommands, firstelem::secondelem::restofStack, env) -> (match (firstelem, secondelem) with
                                                    | (Int(a),Int(b))->  processor restofCommands ((Bool(a>b))::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                    | (Int(a),Int(b)) -> processor restofCommands ((Bool(a>b))::restofStack) env
                                                                    | _ -> print_string "error name" ; processor restofCommands (Error::stack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                    | (Int(a),Int(b)) -> processor restofCommands ((Bool(a>b))::restofStack) env
                                                                    | _ -> processor restofCommands (Error::stack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                    | (Int(a),Int(b)) -> processor restofCommands ((Bool(a>b))::restofStack) env
                                                                    | _ -> processor restofCommands (Error::stack) env) 
                                                    | (_,_) -> processor restofCommands (Error::stack) env)
    | (SWAP::restofCommands, firstitem::seconditem::restofStack, env) -> processor restofCommands (seconditem::firstitem::restofStack) env             
    | (ADD::restofCommands, firstelem::secondelem::restofStack, env) -> (match (firstelem, secondelem) with
                                                    | (Int(a),Int(b))-> processor restofCommands (Int(a+b)::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                        | (Int(a), Int(b)) -> processor restofCommands (Int(a+b)::restofStack) env
                                                                        | _ -> processor restofCommands (Error::stack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                        | (Int(a), Int(b)) -> processor restofCommands (Int(a+b)::restofStack) env
                                                                        | _ -> processor restofCommands (Error::stack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                        | (Int(a), Int(b)) -> processor restofCommands (Int(a+b)::restofStack) env
                                                                        | _ -> processor restofCommands (Error::stack) env) 
                                                    | (_,_) -> processor restofCommands (Error::stack) env)
    | (SUB::restofCommands, firstelem::secondelem::restofStack, env) ->  (match (firstelem, secondelem) with
                                                    | (Int(a),Int(b))-> processor restofCommands (Int(b-a)::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                        | (Int(a), Int(b)) -> processor restofCommands (Int(b-a)::restofStack) env
                                                                        | _ -> processor restofCommands (Error::stack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                        | (Int(a), Int(b)) -> processor restofCommands (Int(b-a)::restofStack) env
                                                                        | _ -> processor restofCommands (Error::stack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                        | (Int(a), Int(b)) -> processor restofCommands (Int(b-a)::restofStack) env
                                                                        | _ -> processor restofCommands (Error::stack) env) 
                                                    | (_,_) -> processor restofCommands (Error::stack) env)
    | (MUL::restofCommands, firstelem::secondelem::restofStack, env) -> (match (firstelem, secondelem) with
                                                    | (Int(a),Int(b))-> processor restofCommands (Int(a*b)::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                          | (Int(a), Int(b)) -> processor restofCommands (Int(a*b)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                          | (Int(a), Int(b)) -> processor restofCommands (Int(a*b)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                          | (Int(a), Int(b)) -> processor restofCommands (Int(a*b)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (_,_) -> processor restofCommands (Error::stack) env)

    | (REM::restofCommands, Int(0)::Int(b)::restofStack, env) -> processor restofCommands (Error::stack) env
    | (DIV::restofCommands, Int(0)::Int(b)::restofStack, env) -> processor restofCommands (Error::stack) env

    | (REM::restofCommands, firstelem::secondelem::restofStack, env) -> (match (firstelem, secondelem) with
                                                    | (Int(a),Int(b))-> processor restofCommands (Int(b mod a)::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                          | (Int(a), Int(b)) -> processor restofCommands (Int(b mod a)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                          | (Int(a), Int(b)) -> processor restofCommands (Int(b mod a)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                          | (Int(a), Int(b)) -> processor restofCommands (Int(b mod a)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (_,_) -> processor restofCommands (Error::stack) env)

    | (DIV::restofCommands, firstelem::secondelem::restofStack, env) -> (match (firstelem, secondelem) with
                                                    | (Int(a),Int(b))-> processor restofCommands (Int(b/a)::restofStack) env
                                                    | (Name(a),Name(b)) -> (match ((find (Name(a)) env ),(find (Name(b)) env)) with
                                                                          | (Int(a), Int(b)) -> print_string "name divide"; processor restofCommands (Int(b/a)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (Name(a),y) -> (match ((find (Name(a)) env ), y) with
                                                                          | (Int(a), Int(b)) -> processor restofCommands (Int(b/a)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (x, Name(b)) -> (match (x, (find (Name(b)) env )) with
                                                                          | (Int(a), Int(b)) -> processor restofCommands (Int(b/a)::restofStack) env
                                                                          | _ -> processor restofCommands (Error::stack) env) 
                                                    | (_,_) -> processor restofCommands (Error::stack) env)
      
    | (IF::restofCommands, x::y::third::restofStack, env) -> (match third with
                                                    | Bool(true) -> processor restofCommands (x::restofStack) env
                                                    | Bool(false) -> processor restofCommands (y::restofStack) env
                                                    | Name(a) -> processor (IF::restofCommands) ((find (Name(a)) env )::restofStack) env
                                                    | _ -> processor restofCommands (Error::stack) env)
         
    | (c::restofCommands,[], env) -> print_string "lastcase";processor restofCommands (Error::stack) env (* if there is nothing in the stack*)
    | (c::restCommands,restStack, env) -> processor restCommands (Error::stack) env(*all the cases that could not be matched by the previous cases *)
    | ([],_, _) -> () (* changed the base case to return unit *) (* empty comlist and whatever stack*)
  
in 

processor comList [] [];;

(* interpreter("input7.txt", "output.txt") *)