open Printf

let file_name = Sys.getcwd () ^ "/bin/input5.txt"
type ordering = {before : int; after : int}
let ic = open_in file_name

let get_ordering_list =
  let rec loop lst =    
    match input_line ic with
      | "" -> lst
      | s -> s 
        |> String.split_on_char '|' 
        |> function 
          | [] -> failwith "invalid input size" 
          | e1 :: e2 :: [] -> loop ({before = int_of_string e1; after = int_of_string e2} :: lst) 
          | _ -> failwith "invalid input size"
  in
  loop []
  
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

let ordering_list = get_ordering_list

let pages_list = get_pages_list



let afters_of_v v =  
  let rec loop l found_list =
    match l with
      | [] -> found_list
      | h :: t -> if h.before = v then loop t (h.after :: found_list) else loop t found_list
  in
  loop ordering_list []

(* let befores_of_v v =  
  let rec loop l found_list =
    match l with
      | [] -> found_list
      | h :: t -> if h.after = v then loop t (h.before :: found_list) else loop t found_list
  in
  loop ordering_list [] *)

exception FoundInvalid
let get_validated_middle_number (lst: int list) : int =  
  let rec loop l curr_idx =
    try     
      match l with
        | [] -> List.nth lst ((curr_idx ) / 2)
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
      | FoundInvalid ->  0
  in
  loop lst 0

let is_list_invalid (lst: int list) : bool =  
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

(* let cons_uniq xs x = if List.mem x xs then xs else x :: xs
let remove_duplicateds_from_left xs = List.rev (List.fold_left cons_uniq [] xs)

type break_resource = {tl : int list; correct_list : int list; pending : int list}
exception BreakWithResource of  break_resource
let get_value_from_invalid_list lst =
  let rec order l correct_list pending =
    try
    match l with
      | [] -> 
        (match pending with
          | [] -> correct_list
          | h :: t ->
            (befores_of_v h)
            |> List.iter (fun before -> 
              if List.mem before pending then                 
                let filtered_pending = List.filter (fun a -> a <> before) pending in
                raise (BreakWithResource {tl = [] ; correct_list = (before :: correct_list); pending = filtered_pending})
            );
            order [] (h :: correct_list) t)      
      | h :: t ->
        (befores_of_v h)
          |> List.iter (fun before -> 
            if List.mem before lst then 
              raise (BreakWithResource {tl = t ; correct_list = (before :: correct_list); pending = (h :: pending)})
          );          
          order t (h :: correct_list) pending
    with
      | BreakWithResource rs -> order rs.tl rs.correct_list rs.pending
  in
  let get_result correct_list =
    let new_list = (remove_duplicateds_from_left correct_list) |> List.rev in 
    new_list |> List.iter (printf "%d - ");
    List.nth new_list ((List.length new_list) / 2) in
  

  let exception Done of int list in
  try
    let rec loop_til_valid l counter =
      if counter > 200 then raise (Done l) else
      let res = order l [] [] in
      let is_valid = not (is_list_invalid res) in
      if  is_valid then raise (Done res) else loop_til_valid res (counter + 1) 
    in
    loop_til_valid (order lst [] []) 0
  with 
    | Done v -> get_result v
  (* let res = order lst [] [] in
  if is_list_invalid res then 
    get_result (order res [] [])
  else get_result res *) *)

let rec shuffle = function
  | [] -> []
  | [single] -> [single]
  | list -> 
    let (before, after) = List.partition (fun _ -> Random.bool ()) list in 
    List.rev_append (shuffle before) (shuffle after)

let get_value_from_invalid_list2 lst =
  let exception Done of int list in
  let get_result l =
    let new_list = l |> List.rev in 
    new_list |> List.iter (printf "%d - ");
    List.nth new_list ((List.length new_list) / 2) in
  try 
    let rec loop l =
      let shuffled = shuffle l in
      let is_valid = not (is_list_invalid shuffled) in
      if is_valid then raise (Done shuffled) else loop shuffled
    in
    loop lst
  with
    | Done l -> get_result l


let puzzle1 =       
  let rec sum_middle_number pl sum =
    match pl with
      | [] -> sum
      | h :: t ->
        let middle_number = get_validated_middle_number h in
        sum_middle_number t (middle_number + sum)
  in
  sum_middle_number pages_list 0


let puzzle2 =          
  let rec sum_middle_number pl sum =
    match pl with
      | [] -> sum
      | h :: t ->        
        let is_invalid = is_list_invalid h in
        printf "is_invalid %b\n" is_invalid;
        if is_invalid then           
          let middle_number = get_value_from_invalid_list2 h in
          (printf "\nmiddle number %d\n" middle_number);
          sum_middle_number t (middle_number + sum)
        else
          sum_middle_number t sum
  in
  sum_middle_number pages_list 0
  

let _ =
  puzzle1 |> printf "\nresult1: %d\n";
  puzzle2 |> printf "\nresult2: %d\n";
  printf "\n"
