open Printf

let file_name = Sys.getcwd () ^ "/bin/input12.txt"

let explode str =   
  Array.init (String.length str) (fun i -> str.[i])  

type pos = {x : int; y : int}

module PosComparable = struct
  type t = pos
  let compare p1 p2 =
    match Stdlib.compare p1.x p2.x with
    | 0 -> Stdlib.compare p1.y p2.y
    | c -> c
end

module PosSet = Set.Make(PosComparable)

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
      


type htbl_key_2 = { c : char; is_the_plant : bool; start_pos : pos }
let htbl2 : (htbl_key_2, pos list) Hashtbl.t = Hashtbl.create 100_000

let ( ^@ ) = PosSet.union


let get_duplicated_perimeters ht htbl_key outsider_duplicateds =  
  let diff_plants = 
    match Hashtbl.find_opt ht htbl_key with
      | None -> []
      | Some x -> x in
  let out_of_bounds = 
    match Hashtbl.find_opt ht { c = out_of_bounds_char; is_the_plant = false; start_pos = htbl_key.start_pos } with
      | None -> []
      | Some x -> x in
  let all_elements = 
    let rec loop odlst full_list =
      match odlst with
        | [] -> full_list
        | h :: t ->
          if List.mem h full_list then loop t full_list else loop t (h :: full_list)
    in
    loop outsider_duplicateds (diff_plants @ out_of_bounds)
  in  
  
  (* ammount of outsiders that are twice duplicated, was counting 3 times as an outsider in puzzle1 TODO handle 4 times cases *)
  let ammount_of_outsiders_duplicateds_twice =
    let rec loop lst sum =
      match lst with
        | [] -> sum
        | h :: t ->
          if List.mem h t then (
            let exception Break in            
            let is_next_on_all_elements cp =            
              let sumr = ref 0 in
              try
                let not_sure_how_much_should_be = 4 in
                for i = 1 to not_sure_how_much_should_be do 
                    if List.mem {x = cp.x + i; y = cp.y} all_elements && (i - !sumr = 1) then sumr := !sumr + 1;
                done;          
                if !sumr > 0 then raise Break;
                for i = 1 to not_sure_how_much_should_be do 
                    if List.mem {x = cp.x; y = cp.y + i} all_elements && (i - !sumr = 1) then sumr := !sumr + 1;
                done;              
                if !sumr > 0 then raise Break;
                for i = 1 to not_sure_how_much_should_be do 
                    if List.mem {x = cp.x - i; y = cp.y} all_elements && (i - !sumr = 1) then sumr := !sumr + 1;
                done;                         
                if !sumr > 0 then raise Break;                 
                for i = 1 to not_sure_how_much_should_be do 
                    if List.mem {x = cp.x; y = cp.y - i} all_elements && (i - !sumr = 1) then sumr := !sumr + 1;
                done;                            
                !sumr
              with
                | Break -> !sumr
            in
            loop t sum + is_next_on_all_elements h
            (* if List.mem {x = h.x + 1; y = h.y} all_elements ||
            List.mem {x = h.x; y = h.y + 1} all_elements ||
            List.mem {x = h.x - 1; y = h.y} all_elements ||
            List.mem {x = h.x; y = h.y - 1} all_elements then (
              (* needs to do this but only + 1 in the exactly same             *)
              
              loop t sum + 1
            ) 
              
            else
              loop t sum *)
          )
          else loop t sum
    in    
    loop outsider_duplicateds 0
  in

  (* printf "outsiders : "; List.iter (fun p -> printf "(x: %d, y: %d) " p.x p.y) all_elements; printf "\n"; *)
  let rec loop lst cached_list =
    match lst with
      | [] -> List.length cached_list
      | h :: t ->
        if List.mem h cached_list then loop t cached_list else
        let rec loop2 curr_pos (new_cached_set: PosSet.t) =
          (* printf "loop2"; *)
          if List.mem curr_pos lst && not (PosSet.mem curr_pos new_cached_set) then (
            let all_cached_sets = 
              loop2 {x = curr_pos.x + 1; y = curr_pos.y} (PosSet.add curr_pos new_cached_set) ^@
              loop2 {x = curr_pos.x; y = curr_pos.y + 1} (PosSet.add curr_pos new_cached_set) ^@
              loop2 {x = curr_pos.x - 1; y = curr_pos.y} (PosSet.add curr_pos new_cached_set) ^@
              loop2 {x = curr_pos.x; y = curr_pos.y - 1} (PosSet.add curr_pos new_cached_set) in            
            (* printf "\n\nset length: %d\n" (PosSet.cardinal all_cached_sets);                         *)
            all_cached_sets
          ) else (
            new_cached_set
          )        
        in
        let new_cached_set = loop2 h PosSet.empty in
        let new_cached_list_headless = (PosSet.to_list new_cached_set)|> List.tl in
        loop t (new_cached_list_headless @ cached_list)
  in
  if htbl_key.c = 'C' then printf "ammout duplicated twice: %d\n" ammount_of_outsiders_duplicateds_twice;
  printf "\n\nammount_of_outsiders_duplicateds_twice %d\n\n" ammount_of_outsiders_duplicateds_twice;
  loop all_elements [] - ammount_of_outsiders_duplicateds_twice



let calculate_boundaries2 mx pos =   
  let plant_char = mx.(pos.y).(pos.x) in
  try
    let pos_list = Hashtbl.find htbl2 {c = plant_char; is_the_plant = true; start_pos = { x= -1; y = -1 }} in
    if List.mem pos pos_list then 0 else (raise Not_found)
  with    
    | Not_found ->
      let rec loop curr_pos area perimeter (outside_duplicateds: pos list) =
        try
          let cc = mx.(curr_pos.y).(curr_pos.x) in
          let is_plant_cached = is_htbl_mem htbl2 {c= plant_char; is_the_plant= true; start_pos = { x= -1; y = -1 }} curr_pos in          
          if cc = plant_char then (
            if is_plant_cached then (
              (area, perimeter, outside_duplicateds)
            ) else (
              htbl_add_or_join htbl2 {c = plant_char; is_the_plant = true; start_pos = { x= -1; y = -1 }} curr_pos;              
              let a1, p1, od1 = loop {x = curr_pos.x + 1; y = curr_pos.y} (area) perimeter outside_duplicateds in
              let a2, p2, od2 = loop {x = curr_pos.x; y = curr_pos.y + 1} (area) perimeter outside_duplicateds in
              let a3, p3, od3 = loop {x = curr_pos.x - 1; y = curr_pos.y} (area) perimeter outside_duplicateds in
              let a4, p4, od4 = loop {x = curr_pos.x; y = curr_pos.y - 1} (area) perimeter outside_duplicateds in
              (a1 + a2 + a3 + a4) + 1, (p1 + p2 + p3 + p4), (od1 @ od2 @ od3 @ od4)
            )
          ) else (
            let is_outsider_cached = is_htbl_mem htbl2 {c= plant_char; is_the_plant= false; start_pos = pos} curr_pos in
            if is_outsider_cached then               
              (area, (perimeter + 1), (curr_pos :: outside_duplicateds))
            else (
              htbl_add_or_join htbl2 {c = plant_char; is_the_plant = false; start_pos = pos} curr_pos;
              (area, (perimeter + 1), outside_duplicateds)
            )              
          )          
        with
          | Invalid_argument _ -> 
            let is_outsider_cached = is_htbl_mem htbl2 {c= out_of_bounds_char; is_the_plant= false; start_pos = pos} curr_pos in
            if is_outsider_cached then (
              (area, perimeter, outside_duplicateds)
            ) else (
              htbl_add_or_join htbl2 {c = out_of_bounds_char; is_the_plant = false; start_pos = pos} curr_pos;
              (area, (perimeter + 1), outside_duplicateds)
            )
            
      in      
      let a, p, od = loop pos 0 0 [] in      
      let duplicated_perimeters = get_duplicated_perimeters htbl2 { c = plant_char; is_the_plant = false; start_pos = pos} od in
      printf "c %c a %d newP %d - duplicateds %d ods: %d\n" plant_char a (p - duplicated_perimeters) duplicated_perimeters (List.length od);      
      a * (p - duplicated_perimeters)

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
  puzzle calculate_boundaries1 |> printf "result1 : %d\n\n";
  Hashtbl.clear htbl;
  puzzle calculate_boundaries2 |> printf "result2 : %d";
  printf "\n"