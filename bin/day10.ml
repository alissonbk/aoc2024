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
      | End_of_file -> close_in ic; printf "total of lines: %d\n" s; s
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



let sum_paths matrix start_pos = 
  let rec loop curr_pos curr_value =
    match matrix.(curr_pos.y).(curr_pos.x) with
      | v when v = 9 -> 1
      | v when v = (curr_value + 1) -> 
        loop {x = curr_pos.x; y = curr_pos.y - 1} v +
        loop {x = curr_pos.x + 1; y = curr_pos.y} v +
        loop {x = curr_pos.x; y = curr_pos.y + 1} v +
        loop {x = curr_pos.x - 1; y = curr_pos.y} v
      | _ -> 0
  in
  loop start_pos 0


let puzzle1 =
  let mx = create_matrix in
  let sumr = ref 0 in
  mx |> Array.iter (fun x -> x |> Array.iter (printf "%d"); printf "\n");
  for i = 0 to (Array.length mx) -1 do 
    for j = 0 to (Array.length mx) -1 do 
      if mx.(i).(j) = 0 then (
        sumr := !sumr + (sum_paths mx { x = j; y = i })
      );
    done;
  done;
  !sumr

let () =
  puzzle1 |> printf "result %d";
  printf "\n";