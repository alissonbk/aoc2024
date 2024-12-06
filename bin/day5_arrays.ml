open Printf

let file_name = Sys.getcwd () ^ "/bin/input5.txt"
let ic = open_in file_name

let get_ordering_hash = 
  let ht = Hashtbl.create 500 in   
  let rec loop str =    
    match str with
      | "" -> ht
      | s -> s
        |> String.split_on_char '|' 
        |> function 
          | [] -> failwith "invalid input size" 
          | e1 :: e2 :: [] -> Hashtbl.add ht (int_of_string e1) (int_of_string e2); loop (input_line ic)
          | _ -> failwith "invalid input size"
  in
  loop (input_line ic)
  
let get_pages_list : int list list =
  let rec loop lst =
    try
      match input_line ic with
        | "" -> lst
        | s -> s
          |> String.split_on_char ','
          |> List.map int_of_string
          |> (fun x -> loop (x :: lst))
    with
      | End_of_file -> lst  
  in
  loop []

let ordering_hash = get_ordering_hash

let pages_list = get_pages_list
let afters_of_v v =  Hashtbl.find_all ordering_hash v

let is_list_invalid (lst: int list) : bool =  
  let exception FoundInvalid in
  let rec loop l curr_idx =
    try     
      match l with
        | [] -> false
        | h :: t -> 
          let afters = afters_of_v h in         
          List.iter (fun x -> 
            if List.mem x afters then (
              match List.find_index (fun b -> b = x) lst with 
                | None -> failwith "shouldnt happen"
                | Some after_idx -> if after_idx < curr_idx then (raise FoundInvalid) else ()
            )) lst;
            loop t (curr_idx + 1)    
    with
      | FoundInvalid ->  true
  in
  loop lst 0
  
let sort = Array.sort (fun a b -> if List.mem b (afters_of_v a) then -1 else 1)

let get_mid arr : int =
  let exception Done in
  try
  let rec loop () =
    sort arr;
    let is_valid = not (is_list_invalid (Array.to_list arr)) in
    if is_valid then raise Done else loop ()
  in
  loop ()
  with 
    | Done -> printf "sorted"; arr.((Array.length arr) / 2)  


let puzzle =
  let rec sum_all pages sum =
    match pages with
      | [] -> sum
      | h :: t ->
        if (is_list_invalid h) then (
          let middle_number = get_mid (Array.of_list h) in
          sum_all t (middle_number + sum)
        ) 
        else sum_all t sum
  in
  sum_all pages_list 0  

let () =
  puzzle |> printf "result: %d\n";
  printf "\n"