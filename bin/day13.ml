open Printf

let file_name = Sys.getcwd () ^ "/bin/input13.txt"


type button_t = { x : int; y : int}
type claw_machine = { button_a : button_t; button_b : button_t; prize : button_t; }

let blank_button_t = { x = -1; y = -1}
let blank_claw_machine = { button_a = blank_button_t; button_b = blank_button_t; prize = blank_button_t;}

let get_x_y str char_to_split add_10000000000000 =
  match String.split_on_char ',' str with
    | [] -> failwith "invalid input2"
    | e1 :: e2 :: [] ->
      let get_after_plus_sign s = String.split_on_char char_to_split s
        |> List.tl |> List.hd |> (fun x -> if add_10000000000000 then int_of_string ("10000000000000" ^ x) else int_of_string x) in
      let x = get_after_plus_sign e1 in
      let y = get_after_plus_sign e2 in
      (x, y)     
    | _ -> failwith "invalid input3"  


let red_input is_puzzle_2 =
  ignore is_puzzle_2;
  let ic = open_in file_name in
  let rec loop machines curr_machine =
    try      
      match input_line ic with
        | "" -> loop (curr_machine :: machines) blank_claw_machine     
        | str when (String.contains str 'A') ->
          let x, y = (get_x_y str '+' false) in
          loop machines { 
            button_a = { x = x; y = y}; 
            button_b = curr_machine.button_b; 
            prize = curr_machine.prize }
        | str when (String.contains str 'B') ->
          let x, y = (get_x_y str '+' false) in
          loop machines { 
            button_a = curr_machine.button_a; 
            button_b = {x = x; y = y}; 
            prize = curr_machine.prize }
        | str when (String.contains str 'P') ->
          let x, y = (get_x_y str '=' is_puzzle_2) in          
          loop machines { 
            button_a = curr_machine.button_a; 
            button_b = curr_machine.button_b; 
            prize = { x = x; y = y}}
        | _ -> failwith "invalid input1"
    with
      | End_of_file -> curr_machine :: machines
  in
  loop [] blank_claw_machine   


  
let calc_min_tokens_used cmachine =
  let xpart_a = cmachine.button_a.x * cmachine.button_a.y in
  let xpart_b = cmachine.button_b.x * cmachine.button_a.y in
  let xpart_prize = cmachine.prize.x * cmachine.button_a.y in

  let ypart_a = cmachine.button_a.y * cmachine.button_a.x in
  let ypart_b = cmachine.button_b.y * cmachine.button_a.x in
  let ypart_prize = cmachine.prize.y * cmachine.button_a.x in

  
  let (a: int option), (b: int option) = 
    (* will be b *)
    if xpart_a = ypart_a then
      None, Some (abs(xpart_prize - ypart_prize) / abs(xpart_b - ypart_b))
    else
      (* will be a *)
      Some (abs(xpart_prize - ypart_prize) / abs(xpart_a - ypart_a)), None
  in  
  let (a: int), (b: int) = 
    match a with
      | None ->
        (* solve a *)
        (
          match b with 
            | None -> failwith "b shouldnt be none"
            | Some b -> abs (abs(cmachine.prize.x - (cmachine.button_b.x * b)) / cmachine.button_a.x), b
        )
      (* solve b *)
      | Some a -> a, abs( abs(cmachine.prize.y - (cmachine.button_a.y * a)) / cmachine.button_b.y) 
  in  
  printf "a : %d, b: %d\n" a b;
  if (a * cmachine.button_a.x) + (b * cmachine.button_b.x) = cmachine.prize.x && 
    (a * cmachine.button_a.y) + (b * cmachine.button_b.y) = cmachine.prize.y 
  then
    a * 3 + b 
  else 
    0

let puzzle ispuzzle2 =
  let claw_machines = red_input ispuzzle2 in
  let rec loop cmachines tokens_used =
    match cmachines with
      | [] -> tokens_used
      | h :: t -> 
        loop t ((calc_min_tokens_used h) + tokens_used)
  in
  loop claw_machines 0  

let () =
  puzzle false |> printf "result: %d\n";
  puzzle true |> printf "result: %d\n";
  printf "\n"