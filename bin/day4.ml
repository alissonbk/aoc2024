open Printf

let file_name = Sys.getcwd () ^ "/bin/input4.txt"

let explode str = Array.init (String.length str) (fun i -> str.[i])

(* need to open new channel so it doesnt need to rewind with seek *)
let count_lines filename : int=
  let ic = open_in filename in
  let rec sum s =
    try
      input_line ic |> ignore;      
      sum (s + 1)
    with
      | End_of_file -> close_in ic; printf "total of lines: %d\n" s; s
  in
  sum 0

let create_matrix filename =
  let ic = open_in filename in    
  let first_line = input_line ic in
  let matrix = Array.make_matrix (count_lines file_name) (String.length first_line)  ' ' in
  matrix.(0) <- (explode first_line);
  try
    for i = 1 to Array.length matrix do 
      let str = input_line ic in
      matrix.(i) <- (explode str)
    done;
    matrix
  with
      | End_of_file -> matrix  
      
type pos = {x : int; y : int}

(* return matrix of positions valid in dim with curr excluded *)
(* walk size need to have curr excluded "if its a word should be word size - 1" *)
let get_valid_positions (curr: pos) (dim: pos) (walk_size : int) (only_diagonals: bool) : pos array array =
  (* let matrix_length = match only_diagonals with | true -> 4 | false -> 8 in *)
  let arr = Array.make_matrix 8 walk_size {x = (-1); y = (-1) } in
  (* printf "walk size: %d\ncurr : (x %d, y %d)\ndim: (x %d, y %d)\n" walk_size curr.x curr.y dim.x dim.y; *)
  let valid_left = (curr.x - walk_size) >= 0 in
  let valid_right = (curr.x + walk_size) < dim.x in  
  let valid_down = (curr.y + walk_size) < dim.y in
  let valid_up = (curr.y - walk_size) >= 0 in  
  if valid_left && not only_diagonals then    
    for i = 1 to walk_size do 
      arr.(0).(i-1) <- { x = (curr.x - i); y = curr.y }
    done;
  (* -> *)
  if valid_right && not only_diagonals then    
    for i = 1 to walk_size do 
      arr.(1).(i-1) <- { x = (curr.x + i); y = curr.y }
    done;
  (* \/ *)      
  if valid_down && not only_diagonals  then
    for i = 1 to walk_size do 
      arr.(2).(i-1) <- { x = curr.x; y = (curr.y + i) }
    done;
  (* /\ *)
  if valid_up && not only_diagonals then
    for i = 1 to walk_size do 
      arr.(3).(i-1) <- { x = curr.x; y = (curr.y - i) }
    done;
  (* <- /\ *)
  if valid_up && valid_left then
    for i = 1 to walk_size do 
      arr.(4).(i-1) <- { x = (curr.x - i); y = (curr.y - i) }
    done;
  (* /\ -> *)
  if valid_up && valid_right then
    for i = 1 to walk_size do 
      arr.(5).(i-1) <- { x = (curr.x + i); y = (curr.y - i) }
    done;
  (* <- \/ *)
  if valid_down && valid_left then
    for i = 1 to walk_size do 
      arr.(6).(i-1) <- { x = (curr.x - i); y = (curr.y + i) }
    done;
  (* \/ -> *)
  if valid_down && valid_right then     
    for i = 1 to walk_size do 
      arr.(7).(i-1) <- { x = (curr.x + i); y = (curr.y + i) }
    done;  
  arr


let string_to_match = "MAS"
let num_chars_to_find = String.length string_to_match

let puzzle1 =
  let matrix = create_matrix file_name in  
  let sum = ref 0 in
  for i = 0 to (Array.length matrix) - 1 do 
    let curr_arr = matrix.(i) in
    for j = 0 to (Array.length curr_arr) - 1 do 
      if curr_arr.(j) = 'X' then
        let vp_matrix = get_valid_positions {x = j; y = i} {x = (Array.length curr_arr); y = (Array.length matrix)} num_chars_to_find false in
        for i2 = 0 to (Array.length vp_matrix) - 1 do
          let pos_arr = vp_matrix.(i2) in
          let arr_of_chars = Array.make (Array.length pos_arr) ' ' in
          if pos_arr.(0).x <> (-1) then
          (            
            for j2 = 0 to (Array.length pos_arr) - 1 do                         
              try              
                arr_of_chars.(j2) <- matrix.(pos_arr.(j2).y).(pos_arr.(j2).x)                          
              with
                | Invalid_argument x -> printf "%s -> j2: %d ; arr_of_chars len: %d ; pos_arr len: %d (x: %d, y: %d) matrix len: (x: %d y: %d)\n" x j2 (Array.length arr_of_chars) (Array.length pos_arr) pos_arr.(j2).x pos_arr.(j2).y  (Array.length matrix.(0)) (Array.length matrix)
            done;            
            if arr_of_chars |> Array.to_seq |> String.of_seq = string_to_match then sum := (!sum + 1);                                  
          );
        done
      ;
    done
  done;
  !sum


let append_item_ref lst e = lst := !lst @ [e]

let validate_list lst : bool =
  match lst with
    | [] -> false
    | e1 :: e2 :: [] -> 
      if (e1.x <> e2.x) && (e1.y <> e2.y) then false else true
    | _ -> false
let puzzle2 =
  let matrix = create_matrix file_name in  
  let sum = ref 0 in
  for i = 0 to (Array.length matrix) - 1 do 
    let curr_arr = matrix.(i) in
    for j = 0 to (Array.length curr_arr) - 1 do 
      if curr_arr.(j) = 'A' then (
        let vp_matrix = get_valid_positions {x = j; y = i} {x = (Array.length curr_arr); y = (Array.length matrix)} 1 true in
        let mcount = ref [] in
        let scount = ref [] in
        for i2 = 0 to (Array.length vp_matrix) - 1 do          
          let pos_arr = vp_matrix.(i2) in                   
          if pos_arr.(0).x <> (-1) then
          (                  
            let curr_char = matrix.(pos_arr.(0).y).(pos_arr.(0).x) in            
            if curr_char = 'M' then append_item_ref mcount {x = (pos_arr.(0).x); y = pos_arr.(0).y};
            if curr_char = 'S' then append_item_ref scount {x = (pos_arr.(0).x); y = pos_arr.(0).y};
          );          
        done;
        if validate_list !mcount && validate_list !scount then sum := (!sum + 1);          
      );
    done
  done;
  !sum


let _ =
  puzzle1 |> printf "result 1: %d";
  puzzle2 |> printf "result 2: %d";
  printf "\n"