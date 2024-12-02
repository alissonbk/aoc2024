open Printf

let file_name = Sys.getcwd () ^ "/bin/input2.txt"


let parse_to_int_list (str: string) : int list =
  match str with
    | "" -> failwith "invalid string"
    | s -> s 
      |> String.split_on_char ' '
      |> List.map int_of_string


let check_two_elements a b is_increasing = 
  if is_increasing then 
    b > a && b - a <= 3 && b - a >= 1 
  else 
    a > b && a - b <= 3 && a - b >= 1

let is_record_safe1 record =  
  let is_increasing = 
    match record with
      | [] -> failwith "invalid input"
      | e1 :: e2 :: _ -> e2 > e1
      | _ -> failwith "invalid input"
  in  
  let rec loop lst is_valid prev  =
    if is_valid = false then 
      false 
    else (
      match lst with
      | [] -> is_valid
      | curr :: t ->
        match prev with
          | None -> loop t true (Some curr)
          | Some x -> loop t (check_two_elements x curr is_increasing) (Some curr)
    )    
  in
  loop record true None


let is_record_safe2 record =  
  let is_increasing = 
    match record with
      | [] -> failwith "invalid input"
      | e1 :: e2 :: _ -> e2 > e1
      | _ -> failwith "invalid input"
  in  
  let rec loop lst invalid_count prev  =
    if invalid_count > 1 then 
      false 
    else (
      match lst with
      | [] -> invalid_count <= 1
      | curr :: t ->
        match prev with
          | None -> loop t 0 (Some curr)
          | Some x -> 
            match (check_two_elements x curr is_increasing) with
              | false -> loop t (invalid_count + 1) (Some curr)
              | true -> loop t invalid_count (Some curr)
            
    )    
  in
  loop record 0 None

  

let puzzle is_record_safe =
  let chan = open_in file_name in
  let rec sum_safe_reports sum =
    try
      let str = input_line chan in
      let lst = parse_to_int_list str in
      let is_safe = is_record_safe lst in
      if is_safe then sum_safe_reports (sum + 1) else sum_safe_reports sum
    with
      | End_of_file -> sum
  in
  sum_safe_reports 0

let puzzle1 = puzzle is_record_safe1
let puzzle2 = puzzle is_record_safe2

let _ = 
  puzzle1 |> printf "puzzle 1: %d\n";
  puzzle2 |> printf "puzzle 2: %d\n";
  printf "\n"