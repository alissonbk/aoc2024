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


(* could have conflicts *)
type border_htbl_key = { border_char : char; border_pos : pos}
let border_htbl = Hashtbl.create 100_000

let is_prev_border mx curr_pos border_pos =
  let border_char = mx.(border_pos.y).(border_pos.x) in
  let positions_to_try = [
    { x = border_pos.x + 1; y = border_pos.y };
    { x = border_pos.x; y = border_pos.y + 1 };
    { x = border_pos.x - 1; y = border_pos.y };
    { x = border_pos.x; y = border_pos.y - 1 };
  ] in
  printf "border char %c\n" border_char;
  let rec loop plist outsider_list = 
    match plist with
      | [] -> outsider_list
      | h :: t ->
        ignore curr_pos;
        (* if h = curr_pos then loop t outsider_list
        else ( *)
          try            
            if mx.(h.y).(h.x) <> border_char then loop t (h :: outsider_list)
            else loop t outsider_list
          with
            | Invalid_argument _ -> 
              loop t (h :: outsider_list)
        (* ) *)
  in
  let outsiders_list = loop positions_to_try [] in  
  let htkey = { border_char = border_char; border_pos = border_pos } in
  let outsiders_without_cache = List.fold_left (
      fun acc curr -> 
        if (is_htbl_mem border_htbl htkey curr) 
          then acc else curr :: acc
      )
    []
    outsiders_list 
  in    
  if (List.length outsiders_without_cache) >= 2 then (
    printf "\n";
    List.iter (fun curr -> 
      (* printf "added: x = %d y = %d - %!" curr.x curr.y;  *)
      printf "added border pos: x = %d y = %d - %!" border_pos.x border_pos.y; 
      htbl_add_or_join border_htbl htkey curr
    ) outsiders_without_cache;
    true
  ) else false


let calculate_boundaries2 mx pos =   
  let plant_char = mx.(pos.y).(pos.x) in
  try
    let pos_list = Hashtbl.find htbl {c = plant_char; is_the_plant = true} in
    if List.mem pos pos_list then 0 else (raise Not_found)
  with    
    | Not_found ->
      let rec loop curr_pos area perimeter prev_pos =
        try
          let cc = mx.(curr_pos.y).(curr_pos.x) in
          let is_plant_cached = is_htbl_mem htbl {c= cc; is_the_plant= true} curr_pos in          
          if cc = plant_char then (
            if is_plant_cached then (
              (area, perimeter)
            ) else (
              htbl_add_or_join htbl {c = cc; is_the_plant = true} curr_pos;              
              let a1, p1 = loop {x = curr_pos.x + 1; y = curr_pos.y} (area) perimeter curr_pos in
              let a2, p2 = loop {x = curr_pos.x; y = curr_pos.y + 1} (area) perimeter curr_pos in
              let a3, p3 = loop {x = curr_pos.x - 1; y = curr_pos.y} (area) perimeter curr_pos in
              let a4, p4 = loop {x = curr_pos.x; y = curr_pos.y - 1} (area) perimeter curr_pos in
              (a1 + a2 + a3 + a4) + 1, (p1 + p2 + p3 + p4)
            )
          ) else (            
            if (is_prev_border mx curr_pos prev_pos)  then 
              (area, (perimeter + 1))
            else (                            
              (area, perimeter)
            )              
          )          
        with
          | Invalid_argument _ -> 
            if (is_prev_border mx curr_pos prev_pos)  then 
              (area, (perimeter + 1))
            else (                            
              (area, perimeter)
            )    
            
      in      
      let a, p = loop pos 0 0 {x = -1; y = -1} in      
      printf "%c -> area %d p %d\n" plant_char a p;
      a * p

let puzzle calculate_boundaries_fun =
  let mx = create_matrix file_name in
  (* mx |> Array.iter (fun x -> Array.iter (printf "%c") x; printf "\n"); *)
  let fodase = { c = '0'; is_the_plant = false} in ignore fodase.is_the_plant; ignore fodase.c;
  let fodase2 = { border_char = '0'; border_pos = {x = -1; y = -1}} in ignore fodase2.border_char; ignore fodase2.border_pos;
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