open Printf

let file_name = Sys.getcwd () ^ "/bin/input12.txt"

let explode str =   
  Array.init (String.length str) (fun i -> str.[i])  

type pos = {x : int; y : int}

let count_lines filename : int=
  let ic = open_in filename in
  let rec sum s =
    try
      input_line ic |> ignore;      
      sum (s + 1)
    with
      | End_of_file -> close_in ic; s
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


type htbl_key = { c : char; is_the_plant : bool }
let htbl = Hashtbl.create 100_000


let is_htbl_mem c pos = 
  try
    let pos_list = Hashtbl.find htbl c in
    if List.mem pos pos_list then true else (raise Not_found)
  with    
    | Not_found -> false

let htbl_add_or_join htbl_key pos =
  match (Hashtbl.find_opt htbl htbl_key) with
    | None -> Hashtbl.add htbl htbl_key [pos]
    | Some pos_list -> Hashtbl.replace htbl htbl_key (pos :: pos_list)

(* cannot exist on the input data *)
let out_of_bounds_char = '*'



let calculate_boundaries mx pos =   
  let plant_char = mx.(pos.y).(pos.x) in
  try
    let pos_list = Hashtbl.find htbl {c = plant_char; is_the_plant = true} in
    if List.mem pos pos_list then 0 else (raise Not_found)
  with    
    | Not_found ->
      let rec loop curr_pos area perimeter =
        try
          let cc = mx.(curr_pos.y).(curr_pos.x) in
          let is_plant_cached = is_htbl_mem {c= cc; is_the_plant= true} curr_pos in
          let is_outsider_cached = is_htbl_mem {c= cc; is_the_plant= false} curr_pos in
          if cc = plant_char then (
            if is_plant_cached then (
              (area, perimeter)
            ) else (
              htbl_add_or_join {c = cc; is_the_plant = true} curr_pos;              
              let a1, p1 = loop {x = curr_pos.x + 1; y = curr_pos.y} (area) perimeter in
              let a2, p2 = loop {x = curr_pos.x; y = curr_pos.y + 1} (area) perimeter in
              let a3, p3 = loop {x = curr_pos.x - 1; y = curr_pos.y} (area) perimeter in
              let a4, p4 = loop {x = curr_pos.x; y = curr_pos.y - 1} (area) perimeter in
              (a1 + a2 + a3 + a4) + 1, (p1 + p2 + p3 + p4)
            )
          ) else (
            if is_outsider_cached then 
              (area, (perimeter + 1))
            else (
              htbl_add_or_join {c = cc; is_the_plant = false} curr_pos;
              (area, (perimeter + 1))
            )              
          )          
        with
          | Invalid_argument _ -> 
            let is_outsider_cached = is_htbl_mem {c= out_of_bounds_char; is_the_plant= false} curr_pos in
            if is_outsider_cached then (
              (area, perimeter)
            ) else (
              htbl_add_or_join {c = out_of_bounds_char; is_the_plant = false} curr_pos;
              (area, (perimeter + 1))
            )
            
      in      
      let a, p = loop pos 0 0 in      
      a * p

let puzzle1 =
  let mx = create_matrix file_name in
  (* mx |> Array.iter (fun x -> Array.iter (printf "%c") x; printf "\n"); *)
  let fodase = { c = '0'; is_the_plant = false} in ignore fodase.is_the_plant; ignore fodase.c;
  let sumr = ref 0 in
  for y = 0 to (Array.length mx) - 1 do 
    for x = 0 to (Array.length mx) - 1 do 
      sumr := !sumr + (calculate_boundaries mx {x = x; y = y})
    done;
  done;
  !sumr


let () =
  puzzle1 |> printf "result1 : %d";
  printf "\n"