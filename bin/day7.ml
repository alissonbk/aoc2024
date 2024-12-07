open Printf

let file_name = Sys.getcwd () ^ "/bin/input7.txt"


type symbol = 
  | SUM 
  | MUL
  | JOIN

let apply_symbol = function 
  | SUM -> ( + ) 
  | MUL -> ( * ) 
  | JOIN -> (fun a b -> ((string_of_int a) ^ (string_of_int b)) |> int_of_string )

let calculate_array arr symbol_arr =       
  let iref = ref 0 in
  for i = 0 to (Array.length arr) - 2 do     
    if i = 0 then (            
      iref := ((apply_symbol symbol_arr.(i)) arr.(i)   arr.(i+1) )
    ) else (            
      iref := ((apply_symbol symbol_arr.(i)) !iref arr.(i + 1))
    )              
  done;  
  !iref


let rec generate_combinations n =
  if n = 0 then
    [[]] 
  else
    let smaller_combinations = generate_combinations (n - 1) in    
    List.flatten (
      List.map (fun comb -> [SUM :: comb; MUL :: comb]) smaller_combinations
    )
;;

let rec generate_combinations_p2 n =
  if n = 0 then
    [[]] 
  else
    let smaller_combinations = generate_combinations_p2 (n - 1) in    
    List.flatten (
      List.map (fun comb -> [SUM :: comb; MUL :: comb; JOIN :: comb]) smaller_combinations
    )
;;

let puzzle1 comb_func =
  let ic = open_in file_name in
  let rec sum_valids sum =
    try      
      match input_line ic with
        | "" -> sum
        | s ->
          let test, n_arr = s 
            |> String.split_on_char ':' 
            |> (function 
              | [] -> failwith "invalid input" 
              | e1 :: e2 :: [] -> 
                (
                  e1 
                    |> int_of_string, 
                  e2 
                    |> String.trim 
                    |> String.split_on_char ' ' 
                    |> List.map int_of_string
                    |> Array.of_list
                ) 
              | _ -> failwith "invalid input") in

          let exception Found in
          try            
            if Array.length n_arr = 1 then (
              if n_arr.(0) = test then (raise Found) else sum_valids sum
            ) else            
            let combinations = comb_func ((Array.length n_arr) -1) in              
            let rec loop (combs: symbol list list) =
              match combs with
                | [] -> sum_valids sum
                | h :: t ->
                  let res = calculate_array n_arr (Array.of_list h) in
                  if res = test then raise Found else loop t
            in
            loop combinations;              

          with
            | Found -> sum_valids (sum + test);          
    with
      | End_of_file -> sum
  in
  sum_valids 0



let () =  
  puzzle1 generate_combinations |> printf "result: %d\n%!";  
  puzzle1 generate_combinations_p2 |> printf "result: %d\n%!";  
  printf "\n"

