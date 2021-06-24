type stackValue = Bool of bool | Int of int | Error | Unit| String of string | Name of string 

type command = ADD | SUB | MUL | DIV | POP | PUSH of stackValue | QUIT | PRINTLN | TOSTRING | SWAP | NEG | REM | CAT | AND | OR | NOT | EQUAL | LESSTHAN
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
            else Int (int_of_string st)
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
    | "lessthan" -> LESSTHAN
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

  let comtostr (comma:command) : string = 
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
  in
  
    (* converts input string list to command list *)
(* 
  let rec view (input:string list):unit =
    match input with
    | [] -> ()
    | hd::tl -> print_string hd; print_string "\n"; view(tl)
  in  *)

  let comList = List.map stringtocommand strList in 

  (* let rec viewcom (input: command list):unit = 
    match input with 
    | [] -> ()
    | hd::tl -> print_string (comtostr hd); print_string "\n"; viewcom(tl)

  in *)

  (* cl is the list of commands, you need to iterate through the cl here*)
   let rec processor cl stack = 
    (* slowly build up the stack where you iterate through the cl and update the stack *)
    match (cl, stack) with
    | (QUIT::restofCommands, _) -> ()
    
    | (POP::restofCommands, firstitempop::restofStack) ->  print_string (svtostr firstitempop); print_string "\n"; processor restofCommands restofStack
    | (NEG::restofCommands, Int(firstint)::restofStack) ->  processor restofCommands (Int(-1*firstint)::restofStack)
    | (TOSTRING::restofCommands,Bool(bval)::restofStack) ->  processor restofCommands (String (":" ^ (svtostr(Bool(bval)) ^ ":" ))::restofStack) 
    | (TOSTRING::restofCommands,firstval::restofStack) ->  processor restofCommands (String (svtostr(firstval))::restofStack) 
    | (PUSH(item)::restofCommands, stack) -> print_string (svtostr item);print_string "\n";processor restofCommands (item::stack) 
    | (PRINTLN::restofCommands, String first::restofStack) -> file_write first; processor restofCommands restofStack
    | (NOT::restofCommands, Bool b :: restofStack) -> processor restofCommands (Bool(not b)::restofStack)
    (* bottom are functions that require 2 arguments *)
    | (CAT::restofCommands, String a::String b::restofStack) -> processor restofCommands (String(a ^ b) ::restofStack)          
    | (AND::restofCommands, Bool b :: Bool c :: restofStack) -> processor restofCommands (Bool(b && c)::restofStack)
    | (OR::restofCommands, Bool b :: Bool c :: restofStack) -> processor restofCommands (Bool(b || c)::restofStack)
    | (SWAP::restofCommands, firstitem::seconditem::restofStack) -> processor restofCommands (seconditem::firstitem::restofStack)             
    | (ADD::restofCommands, Int(a)::Int(b)::restofStack) -> processor restofCommands (Int(a+b)::restofStack)
    | (SUB::restofCommands, Int(a)::Int(b)::restofStack) -> processor restofCommands (Int(b-a)::restofStack)
    | (MUL::restofCommands, Int(a)::Int(b)::restofStack) -> processor restofCommands (Int(a*b)::restofStack)
    
    | (REM::restofCommands, Int(0)::Int(b)::restofStack) -> processor restofCommands (Error::stack)
    | (DIV::restofCommands, Int(0)::Int(b)::restofStack) -> processor restofCommands (Error::stack)

    | (REM::restofCommands, Int(a)::Int(b)::restofStack) -> processor restofCommands (Int(b mod a)::restofStack)
    | (DIV::restofCommands, Int(a)::Int(b)::restofStack) -> processor restofCommands (Int(b/a)::restofStack)
    | (c::restofCommands,[]) -> print_string "\n"; print_string "\n";processor restofCommands (Error::stack) (* if there is nothing in the stack*)
    | (c::restCommands,restStack) -> processor restCommands (Error::restStack) (*all the cases that could not be matched by the previous cases *)
    | ([],_) -> () (* changed the base case to return unit *) (* empty comlist and whatever stack*)
    
    (* | (_::restofCommands,[]) -> processor restofCommands (Error::[])  : do i need this line??????????*) 

in 

(* view(strList); print_string "\n"; print_string "\n";
viewcom(comList); print_string "\n"; print_string "\n"; *)

processor comList [];;

(* interpreter("myinput.txt", "output.txt") *)

(*need the :true: and the :false: for the outputs and get rid of the "" for strings when you push them *)