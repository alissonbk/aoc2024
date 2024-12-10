open Printf

let file_name = Sys.getcwd () ^ "/bin/input10.txt"

let explode str = Array.init (String.length str) (fun i -> str.[i])

let string_of_char c = String.make 1 c

let expode_to_int str = explode str |> Array.map (fun c -> c |> string_of_char |> int_of_string)

type pos = { x : int; y : int}


let count_lines : int=
  let ic = open_in file_name in
  let rec sum s =
    try
      input_line ic |> ignore;      
      sum (s + 1)
    with
      | End_of_file -> close_in ic; s
  in
  sum 0

let create_matrix =
  let ic = open_in file_name in    
  let first_line = input_line ic in
  let matrix = Array.make_matrix count_lines (String.length first_line)  (-1) in
  matrix.(0) <- (expode_to_int first_line);
  try
    for i = 1 to Array.length matrix do 
      let str = input_line ic in
      matrix.(i) <- (expode_to_int str) 
    done;
    matrix
  with
      | End_of_file -> matrix  

let sum_paths matrix start_pos htbl =     
  let rec loop new_pos curr_value ht =
    try      
    match matrix.(new_pos.y).(new_pos.x) with
      | new_v when new_v = (curr_value + 1) && new_v = 9 ->                   
        (match (Hashtbl.find_opt ht new_pos) with
            | None -> 
              Hashtbl.add ht new_pos start_pos;               
              1
            | Some sp ->                
              if (sp.x = start_pos.x && sp.y = start_pos.y) then 0
              else (
                Hashtbl.add htbl new_pos start_pos;                
                1
            ))
      | new_v when new_v = (curr_value + 1) ->                
        loop {x = new_pos.x; y = new_pos.y - 1} new_v ht +        
        loop {x = new_pos.x + 1; y = new_pos.y} new_v ht +
        loop {x = new_pos.x; y = new_pos.y + 1} new_v ht +
        loop {x = new_pos.x - 1; y = new_pos.y} new_v ht
      | _ -> 0
    with
      | Invalid_argument _ -> 0
  in
  loop start_pos (-1) htbl

let puzzle1 =
  let mx = create_matrix in
  let sumr = ref 0 in
  let htbl = Hashtbl.create 1000 in
  (* mx |> Array.iter (fun x -> x |> Array.iter (printf "%d"); printf "\n"); *)
  for i = 0 to (Array.length mx) -1 do 
    for j = 0 to (Array.length mx) -1 do 
      if mx.(i).(j) = 0 then (
        sumr := !sumr + (sum_paths mx { x = j; y = i } htbl) 
      );
    done;
  done;
  (* Hashtbl.iter (fun key v -> printf "key: x: %d y: %d - value: x: %d y: %d\n" key.x key.y v.x v.y) htbl; *)
  !sumr

let () =
  puzzle1 |> printf "result %d";
  printf "\n";