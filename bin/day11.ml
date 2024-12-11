open Printf

let file_name = Sys.getcwd () ^ "/bin/input11.txt"

let explode str =   
  Array.init (String.length str) (fun i -> str.[i])  

let string_of_char c = String.make 1 c

let implode = Array.fold_left (fun acc curr -> acc ^ (string_of_char curr) ) ""

(* let htbl = Hashtbl.create 10000 *)

let get_input =
  let ic = open_in file_name in
  match input_line ic with
    | "" -> failwith "invalid input"
    | s -> s |> String.split_on_char ' ' |> List.map int_of_string


let divide_str_number n_str =
  let str_len = String.length n_str in  
  let char_arr = n_str |> explode in  
  let left = Array.make (str_len / 2) ' ' in  
  let right = Array.make (str_len / 2) ' ' in  
  for i = 0 to (str_len / 2) - 1 do 
    left.(i) <- char_arr.(i)
  done;    
  for i = (str_len / 2) to (str_len - 1) do 
    right.(i - (str_len / 2)) <- char_arr.(i);    
  done;  
  (implode left |> int_of_string, implode right |> int_of_string)


let blink lst =
  let rec loop lst new_list =
    match lst with
      | [] -> new_list
      | h :: t ->                
        let string_of_h = string_of_int h in
        if h = 0 then 
          loop t (1 :: new_list) 
        else if ((String.length string_of_h) mod 2) = 0 then (            
          let left, right = divide_str_number string_of_h in          
          loop t (left :: right :: new_list)
        ) else 
          loop t ((h * 2024) :: new_list)          
  in
  loop lst []

  

let puzzle1 =
  let input = get_input in
  (* input |> List.iter (printf "%d "); *)
  let rec loop lst count =
    match count with
      | c when c = 25 -> List.length lst
      | c -> loop (blink lst) (c + 1)
        
  in
  loop input 0  

let htbl = Hashtbl.create 200_000
let rec blink_loop blinks number = 
    if blinks = 0 then 1 else (
      try 
        Hashtbl.find htbl (blinks, number)
      with
      | Not_found ->
        let value = ref 0 in
        let number_str = string_of_int number in
        if number = 0 then (
          value := blink_loop (blinks -1) 1 
        ) else if ((String.length number_str) mod 2) = 0 then (            
          let left, right = divide_str_number number_str in          
          value := (blink_loop (blinks - 1) left) + (blink_loop (blinks - 1) right);
        ) else (
          value := blink_loop (blinks - 1) (number * 2024);          
        );
        Hashtbl.add htbl (blinks, number) !value;
        !value            
    )
    
let puzzle2 =
  let input = get_input in  
  (* input |> List.iter (printf "%d "); *)
  List.fold_left (fun acc curr -> acc + (blink_loop 75 curr)) 0 input


(* 
  maybe will need to run with
  export OCAMLRUNPARAM="stack=8192";
*)
let () =
  puzzle1 |> printf "\nresult 1: %d\n";
  puzzle2 |> printf "\nresult 2: %d\n";
  printf "\n"

