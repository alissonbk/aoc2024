open Printf

let filename = Sys.getcwd () ^ "/bin/input15.txt"


type pos = { x : int; y : int }

let default_pos = { x = (-1); y = (-1) }

let get_map_size =
  let ic = open_in filename in
  let rec loop x y =
    match input_line ic with
      | "" -> {x = x; y = y }
      | s -> 
        if x = 0 then 
          loop (String.length s) 1 
        else
          loop x (y + 1)
  in
  let pos = loop 0 0 in
  close_in ic;
  pos

let string_to_chararr str = Array.init (String.length str) (fun idx -> str.[idx])

let find_start_pos arr = 
  let resultr = ref (-1) in
  for i = 0 to (Array.length arr) - 1 do 
    if arr.(i) = '@' then resultr := i;
  done;
  !resultr

let read_input = 
  let map_size = get_map_size in
  let ic = open_in filename in  
  let map_matrix = Array.make_matrix map_size.y map_size.x ' ' in
  let rec loop_map count start_pos =
    match input_line ic with
      | "" -> start_pos     
      | s ->        
        let char_arr = string_to_chararr s in
        map_matrix.(count) <- char_arr;
        if start_pos.x = (-1) then
          loop_map (count + 1) { x = (find_start_pos char_arr); y = count }
        else
          loop_map (count + 1) start_pos        
  in
  let start_pos = loop_map 0 default_pos in
  let rec loop_moves lst =  
    try  
      match input_line ic with
        | s -> s |> string_to_chararr |> Array.to_list |> fun nl -> loop_moves (nl :: lst)
    with 
      | End_of_file -> lst
  in
  let moves_lst = loop_moves [] |> List.rev |> List.flatten in
  map_matrix, moves_lst, start_pos

let string_of_char c = String.make 1 c

let move_pos pos c =
  match c with
    | '^' -> { x = pos.x; y = pos.y - 1 }
    | '>' -> { x = pos.x + 1; y = pos.y }
    | 'v' -> { x = pos.x; y = pos.y + 1 }
    | '<' -> { x = pos.x - 1; y = pos.y }
    | _ -> failwith "invalid char"

let move_boxes map prev_pos first_box_pos direction_char () : bool =  
  let rec find_end_pos prev_pos =
    let cp = move_pos prev_pos direction_char in
    let cpchar = map.(cp.y).(cp.x) in
    match cpchar with
      | '#' -> default_pos
      | '.' -> cp
      | 'O' -> find_end_pos cp
      | s -> failwith ("invalid input " ^ string_of_char s)
  in
  let end_pos = find_end_pos first_box_pos in
  if end_pos.x != (-1) then (
    map.(end_pos.y).(end_pos.x) <- 'O';
    map.(first_box_pos.y).(first_box_pos.x) <- '@';
    map.(prev_pos.y).(prev_pos.x) <- '.';    
    true
  ) else false  

let calc_boxes_positions map =
  let sumr = ref 0 in
  for y = 0 to (Array.length map) - 1 do 
    for x = 0 to (Array.length map.(y)) - 1 do 
      if map.(y).(x) = 'O' then (
        sumr := !sumr + ((100 * y) + x)
      )
    done;
  done;
  !sumr

let puzzle1 =
  let map, move_lst, start_pos = read_input in
  (* printf "start_pos = x %d y %d\n" start_pos.x start_pos.y;
  move_lst |> List.iter (printf "%c "); printf "\n"; map |> Array.iter (fun a -> a |> Array.iter (printf "%c"); printf "\n"); *)
  let rec loop mvlst prev_pos =
    match mvlst with
      | [] -> ()
      | h :: t ->
        let cp = move_pos prev_pos h in
        let cpchar = map.(cp.y).(cp.x) in
        if cpchar = '.' then (
          map.(cp.y).(cp.x) <- '@';
          map.(prev_pos.y).(prev_pos.x) <- '.';
          loop t cp
        ) else if cpchar = '#' then (
          loop t prev_pos
        ) else (
          let moved = move_boxes map prev_pos cp h () in
          if moved then loop t cp else loop t prev_pos          
        )
  in
  loop move_lst start_pos;
  calc_boxes_positions map


let () = 
  puzzle1 |> printf "result : %d\n";
  printf "\n"