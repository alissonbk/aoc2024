open Printf

let filename = Sys.getcwd () ^ "/bin/input14.txt"

type pos = { x : int; y : int }
type pos_w_op = { x: int; y : int; xop : (int -> int -> int); yop : (int -> int -> int) }
type robot = { start_at : pos;  velocity : pos_w_op }
let max_x = 101
let max_y = 103
let mid_x = abs (max_x / 2)
let mid_y = abs (max_y / 2)

let get_operation n = if n >= 0 then   
  ( + )
  else     
  ( - )

let parse_x_y str =
  String.split_on_char ',' str 
    |> function 
      | [] -> failwith "fail to parse x and y" 
      | e1 :: e2 :: [] ->
        let x = String.split_on_char '=' e1 |> List.tl |> List.hd |> int_of_string in
        let y = int_of_string e2 in        
        {x = x; y = y}
      | _ -> failwith "fail to parse x and y" 

let read_input =
  let ic = open_in filename in
  let rec loop lst =
    try 
      let line = input_line ic in      
      match String.split_on_char ' ' line with      
        | [] -> failwith "failed to split input"
        | e1 :: e2 :: [] ->         
          let velocity = 
            let pos = parse_x_y e2 in                      
            { x = pos.x; y = pos.y; xop = (get_operation pos.x); yop = (get_operation pos.y) }
          in          
          loop ({ start_at = parse_x_y e1; velocity = velocity } :: lst)
        | _ -> failwith "failed to split input"
    with
      | End_of_file -> lst
  in
  loop []

let calc_robot_end_pos robot t =
  let rec loop (last_pos: pos) times =    
    match times with
      | x when x = t -> last_pos
      | _ -> 
        let new_x =           
          let nx = robot.velocity.xop last_pos.x (abs robot.velocity.x) in
          if nx < 0 then 
            max_x - abs(nx) 
          else if nx >= max_x then
            nx - max_x
          else
            nx
        in
        let new_y = 
          let ny = robot.velocity.yop last_pos.y (abs robot.velocity.y) in          
          if ny < 0 then 
            max_y - abs(ny) 
          else if ny >= max_y then
            ny - max_y
          else ny
        in
        loop { x = new_x; y = new_y } (times + 1)
          
  in
  loop robot.start_at 0


let is_middle_pos (pos: pos) = pos.x = mid_x || pos.y = mid_y
  
let is_quadrant_a (pos: pos) = pos.y < mid_y && pos.x < mid_x
let is_quadrant_b (pos: pos) = pos.y < mid_y && pos.x > mid_x
let is_quadrant_c (pos: pos) = pos.y > mid_y && pos.x < mid_x
let is_quadrant_d (pos: pos) = pos.y > mid_y && pos.x > mid_x


let puzzle1 = 
  let input = read_input in
  (* List.iter (fun r ->printf "start x %d y %d - vel x %d y %d\n" r.start_at.x r.start_at.y r.velocity.x r.velocity.y ) input; *)
  let sumr_qa = ref 0 in
  let sumr_qb = ref 0 in
  let sumr_qc = ref 0 in
  let sumr_qd = ref 0 in
  List.iter (
    fun x ->
      let end_pos = calc_robot_end_pos x 100 in      
      if not (is_middle_pos end_pos) then (
        (*should break*)
        if is_quadrant_a end_pos then sumr_qa := !sumr_qa + 1;
        if is_quadrant_b end_pos then sumr_qb := !sumr_qb + 1;
        if is_quadrant_c end_pos then sumr_qc := !sumr_qc + 1;
        if is_quadrant_d end_pos then sumr_qd := !sumr_qd + 1;
      );
  ) input;  
  !sumr_qa * !sumr_qb * !sumr_qc * !sumr_qd


let string_of_char c = String.make 1 c
let draw_robots (arr: pos Array.t) =
  let board = Array.make_matrix max_y max_x '.' in
  for i = 0 to (Array.length arr) - 1 do 
    let robot = arr.(i) in
    board.(robot.y).(robot.x) <- '1'
  done;
  board  


let sum_path_sequence (pos: pos) mx stop_point sum =
  let failed_sum = ref 0 in
  let rec loop (pos: pos) mx stop_point sum = 
  if !failed_sum = (stop_point) then 
    sum 
  else if sum = stop_point then 
    sum 
  else (
      try
      (
        match mx.(pos.y).(pos.x) with      
          | '.' -> failed_sum := !failed_sum + 1; sum
          | '1' ->
            let up = loop { x = pos.x; y = (pos.y - 1) } mx stop_point sum in
            let right = loop { x = pos.x + 1; y = pos.y } mx stop_point sum in
            let bot = loop { x = pos.x; y = pos.y + 1 } mx stop_point sum in
            let left = loop { x = pos.x - 1; y = pos.y } mx stop_point sum in            
            (up + right + bot + left) + 1
          | _ -> failwith "shouldnt happen"
      )
      with 
        | Invalid_argument _ -> failed_sum := !failed_sum + 1; sum
    )
    in
  loop (pos: pos) mx stop_point sum
  



(* there is many or more similars in the pos array *)
let there_is_many_similar_in_sequence (robots_arr : pos array) (mx : char array array) : bool =
  let min_seq = 15 in
  let min_qty = 80 in
  let sumr = ref 0 in    
  for i = 0 to (Array.length robots_arr) - 1 do     
    if (sum_path_sequence robots_arr.(i) mx min_seq 0) >= min_seq then sumr := !sumr + 1;
  done;
  (* printf "sumr: %d" !sumr; *)
  (* 2 times because of duplications *)
  !sumr > (min_qty * 2)

let puzzle2 = 
  let input = read_input in
  (* List.iter (fun r ->printf "start x %d y %d - vel x %d y %d\n" r.start_at.x r.start_at.y r.velocity.x r.velocity.y ) input; *)  
  for i = 0 to 10000 do     
    let robots_array = Array.make (List.length input) {x = -1; y = -1} in
    let arr_counter = ref 0 in
    List.iter (
      fun x ->
        let end_pos = calc_robot_end_pos x i in
        robots_array.(!arr_counter) <- end_pos;      
        arr_counter := !arr_counter + 1;        
    ) input;        
    let draw = draw_robots robots_array in     
    if there_is_many_similar_in_sequence robots_array draw then
    (      
      let draw_string_arr = Array.map (fun a -> Array.fold_left (fun acc curr -> (string_of_char curr) ^ acc) "" a) draw in  
      let new_file_name = Sys.getcwd () ^ "/bin/day14-files/" ^ (string_of_int i) ^ ".txt" in
      let oc = open_out new_file_name in
      Array.iter (fun s -> fprintf oc "%s\n" s) draw_string_arr;  
      Stdlib.close_out oc
    )
  done


let () =  
  puzzle1 |> printf "result: %d \n";
  puzzle2;
  printf "\n"