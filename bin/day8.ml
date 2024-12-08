open Printf

let file_name = Sys.getcwd () ^ "/bin/input8.txt"


type pos = { x : int; y : int }

let explode str = Array.init (String.length str) (fun i -> str.[i])

let ht_find ht v = match Hashtbl.find_opt ht v with | None -> [] | Some v -> v

let read_input () =
  let ic = open_in file_name in
  let first_line = (input_line ic) in
  let length = String.length first_line in
  let ht = Hashtbl.create (length * length) in
  let matrix = Array.make_matrix length length ' ' in    
  let rec read str idx = 
    try
      let carr = explode str in
      matrix.(idx) <- carr;
      carr |> Array.iteri (fun i e -> 
        if e <> '.' then (          
          Hashtbl.replace ht e ({ x = i; y = idx} :: (ht_find ht e)))          
        );      
      read (input_line ic) (idx + 1)
    with
      | End_of_file -> (matrix, ht)
  in
  read first_line 0
  
(* returns 1 when added else 0 *)
let try_add_new_antinode pos matrix freq_char =
  try
    let c = matrix.(pos.y).(pos.x) in
    if c <> freq_char && c <> '#' then (
      matrix.(pos.y).(pos.x) <- '#'; 
      1
    ) else 0
  with
    | Invalid_argument _ -> 0


(*ignoring equals*)
let calculate_new_positions pa pb =   
  let diffx = abs(pa.x - pb.x) in
  let diffy = abs(pa.y - pb.y) in
  (* maybe should consider x also somehow *)
  let is_ax_bigger = pa.x > pb.x in
  let is_ay_bigger = pa.y > pb.y in

  (* refactor this shit *)
  if is_ay_bigger then (
    if is_ax_bigger then (
      [
        {x = (pa.x + diffx); y = (pa.y + diffy)}; 
        {x = (pb.x - diffx); y = (pb.y - diffy)}
      ]
    ) else (
      [
        {x = (pa.x - diffx); y = (pa.y + diffy)}; 
        {x = (pb.x + diffx); y = (pb.y - diffy)}
      ]
    )    
  )    
  else (
    if is_ax_bigger then (
      [
        {x = (pa.x + diffx); y = (pa.y - diffy)}; 
        {x = (pb.x - diffx); y = (pb.y + diffy)}
      ]
    ) else (
      [
        {x = (pa.x - diffx); y = (pa.y - diffy)}; 
        {x = (pb.x + diffx); y = (pb.y + diffy)}
      ]
    ) 
  )

let puzzle1 = 
  let matrix, ht = read_input () in  
  let sumr = ref 0 in
  Hashtbl.iter (fun key pos_list ->     
    let arr = pos_list |> Array.of_list in
     arr |> Array.iteri 
        (fun i comp_pos-> 
          arr |> Array.iteri (fun j pos -> 
              if j <> i then (
                (calculate_new_positions comp_pos pos)
                  |> List.iter (fun new_p -> 
                    sumr := !sumr + (try_add_new_antinode new_p matrix key)
                  )             
              )
            )
        )
  ) ht;  
  !sumr



let try_add_new_antinode_2 pos matrix curr_antenna antenna_list duplicateds_ht =
  try
    let c = matrix.(pos.y).(pos.x) in
    if c <> '#' then (      
      if not (List.mem c antenna_list) then (
        matrix.(pos.y).(pos.x) <- '#'; 
        1
      ) else (
        let opt = Hashtbl.find_opt duplicateds_ht pos in
        match opt with
          | None -> Hashtbl.add duplicateds_ht pos curr_antenna; 1
          | Some _ -> 0
      )           
    ) else 0
  with
    | Invalid_argument _ -> 0

(* returns  y, x *)
let get_operators is_ay_lower is_ax_lower = 
  if is_ay_lower then (
    if is_ax_lower then ( ( + ), ( + ) ) else ( ( + ), ( - ))
  ) else (
    if is_ax_lower then ( ( - ), ( + ) ) else ( ( - ), ( - ))
  )

(* ignore duplicate antinode in antenna *)
let calculate_new_positions_2 pa pb msize =   
  let diffx = abs(pa.x - pb.x) in
  let diffy = abs(pa.y - pb.y) in  
  let is_ax_lower = pa.x < pb.x in
  let is_ay_lower = pa.y < pb.y in

 let rec loop curr new_positions =
    if curr.x > (msize - 1) || curr.y > (msize - 1) || curr.x < 0 || curr.y < 0 then new_positions 
    else (      
      let yop, xop = get_operators is_ay_lower is_ax_lower in
      let new_p = { x = xop curr.x diffx; y = yop curr.y diffy } in
      loop new_p (new_p :: new_positions)
    )
    
 in
 loop pa []

let get_antenna_list ht = Hashtbl.fold (fun key _ acc -> key :: acc) ht []

let puzzle2 = 
  let matrix, ht = read_input () in
  let msize = Array.length matrix in  
  let sumr = ref 0 in
  let antenna_list = get_antenna_list ht in
  let duplicateds_ht = Hashtbl.create (List.length antenna_list) in
  Hashtbl.iter (fun key pos_list ->     
    let arr = pos_list |> Array.of_list in
     arr |> Array.iteri 
        (fun i comp_pos-> 
          arr |> Array.iteri (fun j pos -> 
              if j <> i then (
                (calculate_new_positions_2 comp_pos pos msize)
                  |> List.iter (fun new_p -> 
                    sumr := !sumr + (try_add_new_antinode_2 new_p matrix key antenna_list duplicateds_ht)
                  )             
              )
            )
        )
  ) ht;  
  !sumr


let () = 
  puzzle1 |> printf "result: %d\n";
  puzzle2 |> printf "result: %d";
  printf "\n"
