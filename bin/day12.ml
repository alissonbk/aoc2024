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


let is_htbl_mem htbl key pos = 
  try
    let pos_list = Hashtbl.find htbl key in
    if List.mem pos pos_list then true else (raise Not_found)
  with    
    | Not_found -> false

let htbl_add_or_join htbl htbl_key pos =
  match (Hashtbl.find_opt htbl htbl_key) with
    | None -> Hashtbl.add htbl htbl_key [pos]
    | Some pos_list -> Hashtbl.replace htbl htbl_key (pos :: pos_list)


(* cannot exist on the input data *)
let out_of_bounds_char = '*'



let calculate_boundaries1 mx pos =   
  let plant_char = mx.(pos.y).(pos.x) in
  try
    let pos_list = Hashtbl.find htbl {c = plant_char; is_the_plant = true} in
    if List.mem pos pos_list then 0 else (raise Not_found)
  with    
    | Not_found ->
      let rec loop curr_pos area perimeter =
        try
          let cc = mx.(curr_pos.y).(curr_pos.x) in
          let is_plant_cached = is_htbl_mem htbl {c= cc; is_the_plant= true} curr_pos in          
          if cc = plant_char then (
            if is_plant_cached then (
              (area, perimeter)
            ) else (
              htbl_add_or_join htbl {c = cc; is_the_plant = true} curr_pos;              
              let a1, p1 = loop {x = curr_pos.x + 1; y = curr_pos.y} (area) perimeter in
              let a2, p2 = loop {x = curr_pos.x; y = curr_pos.y + 1} (area) perimeter in
              let a3, p3 = loop {x = curr_pos.x - 1; y = curr_pos.y} (area) perimeter in
              let a4, p4 = loop {x = curr_pos.x; y = curr_pos.y - 1} (area) perimeter in
              (a1 + a2 + a3 + a4) + 1, (p1 + p2 + p3 + p4)
            )
          ) else (
            let is_outsider_cached = is_htbl_mem htbl {c= cc; is_the_plant= false} curr_pos in
            if is_outsider_cached then 
              (area, (perimeter + 1))
            else (
              htbl_add_or_join htbl {c = cc; is_the_plant = false} curr_pos;
              (area, (perimeter + 1))
            )              
          )          
        with
          | Invalid_argument _ -> 
            let is_outsider_cached = is_htbl_mem htbl {c= out_of_bounds_char; is_the_plant= false} curr_pos in
            if is_outsider_cached then (
              (area, perimeter)
            ) else (
              htbl_add_or_join htbl {c = out_of_bounds_char; is_the_plant = false} curr_pos;
              (area, (perimeter + 1))
            )
            
      in      
      let a, p = loop pos 0 0 in      
      a * p
      

(* missing middle jumps (e.g calculate how many times he jumped from 0 to 2 either x or y been y or x respectively equal)*)
let calc_sides char_list =
  let rec loop lst unique_x unique_y =
    match lst with
      | [] -> 
        let len_x = (List.length unique_x) in
        let len_y = (List.length unique_y) in
        if len_x = 1 then len_y else if len_y = 1 then len_x else len_x + len_y
      | h :: t ->
        let new_unique_x = if (List.mem h.x unique_x) then unique_x  else (h.x :: unique_x) in
        let new_unique_y = if (List.mem h.y unique_y) then unique_y  else (h.y :: unique_y) in
        loop t new_unique_x new_unique_y
  in
  loop char_list [] []


let calculate_boundaries2 mx pos =   
  let plant_char = mx.(pos.y).(pos.x) in
  try
    let pos_list = Hashtbl.find htbl {c = plant_char; is_the_plant = true} in
    if List.mem pos pos_list then 0 else (raise Not_found)
  with    
    | Not_found ->
      let rec loop curr_pos area char_list =
        try
          let cc = mx.(curr_pos.y).(curr_pos.x) in
          let is_plant_cached = is_htbl_mem htbl {c= cc; is_the_plant= true} curr_pos in          
          if cc = plant_char then (
            if is_plant_cached then (
              (area, char_list)
            ) else (
              htbl_add_or_join htbl {c = cc; is_the_plant = true} curr_pos;     
              let new_char_list = curr_pos :: char_list in         
              let a1, cl1 = loop {x = curr_pos.x + 1; y = curr_pos.y} (area) new_char_list in
              let a2, cl2 = loop {x = curr_pos.x; y = curr_pos.y + 1} (area) new_char_list in
              let a3, cl3 = loop {x = curr_pos.x - 1; y = curr_pos.y} (area) new_char_list in
              let a4, cl4 = loop {x = curr_pos.x; y = curr_pos.y - 1} (area) new_char_list in
              ((a1 + a2 + a3 + a4) + 1), cl1 @ cl2 @ cl3 @ cl4 @ new_char_list
            )
          ) else (                                                  
            (area, char_list)
          )          
        with
          | Invalid_argument _ -> (area, char_list) 
            
      in      
      let a, char_list = loop pos 0 [] in      
      (* printf "char_list: "; char_list |> List.iter (fun p -> printf "x %d y %d" p.x p.y); *)
      let sides = calc_sides char_list in
      printf "%c -> area %d sides %d\n" plant_char a sides;
      a * sides

let puzzle calculate_boundaries_fun =
  let mx = create_matrix file_name in
  (* mx |> Array.iter (fun x -> Array.iter (printf "%c") x; printf "\n"); *)
  let fodase = { c = '0'; is_the_plant = false} in ignore fodase.is_the_plant; ignore fodase.c;  
  let sumr = ref 0 in
  for y = 0 to (Array.length mx) - 1 do 
    for x = 0 to (Array.length mx) - 1 do 
      sumr := !sumr + (calculate_boundaries_fun mx {x = x; y = y})
    done;
  done;
  !sumr


let () =
  puzzle calculate_boundaries1 |> printf "result1 : %d\n";
  Hashtbl.clear htbl;
  puzzle calculate_boundaries2 |> printf "result2 : %d";
  printf "\n"