open Printf

let file_name = Sys.getcwd () ^ "/bin/input13.txt"


type button_t = { x : int; y : int}
type button_name = A | B | None
type claw_machine = { button_a : button_t; button_b : button_t; prize : button_t; cheaper_x : button_name; cheaper_y : button_name }

let blank_button_t = { x = -1; y = -1}
let blank_claw_machine = { button_a = blank_button_t; button_b = blank_button_t; prize = blank_button_t; cheaper_x = None; cheaper_y = None}

let get_x_y str char_to_split =
  match String.split_on_char ',' str with
    | [] -> failwith "invalid input2"
    | e1 :: e2 :: [] ->
      let get_after_plus_sign s = String.split_on_char char_to_split s
        |> List.tl |> List.hd |> int_of_string in
      let x = get_after_plus_sign e1 in
      let y = get_after_plus_sign e2 in
      (x, y)     
    | _ -> failwith "invalid input3"  

let get_cheaper_x_y button_a button_b =
  let cp_x =     
    if button_a.x > (button_b.x * 3) then
      A
    else
      B 
  in
  let cp_y = 
    if button_a.y > (button_b.y * 3) then
      A
    else
      B
  in
  (cp_x, cp_y)


let red_input =
  let ic = open_in file_name in
  let rec loop machines curr_machine =
    try      
      match input_line ic with
        | "" -> loop (curr_machine :: machines) blank_claw_machine     
        | str when (String.contains str 'A') ->
          let x, y = get_x_y str '+' in
          loop machines { 
            button_a = { x = x; y = y}; 
            button_b = curr_machine.button_b; 
            prize = curr_machine.prize; 
            cheaper_x = None; 
            cheaper_y = None }
        | str when (String.contains str 'B') ->
          let x, y = get_x_y str '+' in
          loop machines { 
            button_a = curr_machine.button_a; 
            button_b = {x = x; y = y}; 
            prize = curr_machine.prize; 
            cheaper_x = None; 
            cheaper_y = None }
        | str when (String.contains str 'P') ->
          let x, y = get_x_y str '=' in
          let cheaper_x, cheaper_y = get_cheaper_x_y curr_machine.button_a curr_machine.button_b in
          loop machines { 
            button_a = curr_machine.button_a; 
            button_b = curr_machine.button_b; 
            prize = { x = x; y = y}; 
            cheaper_x = cheaper_x; 
            cheaper_y = cheaper_y }
        | _ -> failwith "invalid input1"
    with
      | End_of_file -> curr_machine :: machines
  in
  loop [] blank_claw_machine


(* everything is x or everything is y *)
let calc_tokens_used_a div_xory a_btn_xory b_btn_xory prize_xory =
  let div_xory_result = div_xory * a_btn_xory in
  (* perfect match *)
  if div_xory_result = prize_xory then (
    div_xory * 3
  ) else (
    (* using button b will be equal or more expensive*)
    if (b_btn_xory * 3) + div_xory_result <= prize_xory then (
      (div_xory + 1) * 3
    (* using button b will be cheaper *)
    ) else (                  
      (* each b token costs 1 *)
      if div_xory_result + b_btn_xory >= prize_xory then (div_xory * 3) + 1 else (div_xory * 3) + 2
    )
  )

(* everything is x or everything is y *)
let calc_tokens_used_b div_xory b_btn_xory  prize_xory =
  let div_xory_result = div_xory * b_btn_xory in
  (* perfect match *)
  if div_xory_result = prize_xory then (
    div_xory
  (* using B three times is cheaper than A so A will never be used *)
  ) else (
    let rec loop count =
      if (b_btn_xory * count) + div_xory_result >=  prize_xory then (div_xory + count) else loop (count + 1)
    in
    loop 1    
  )

let calc_min_tokens_used cmachine =
  match cmachine.cheaper_x with
    | None -> failwith "invalid input"
    | A ->
      let divx = cmachine.prize.x / cmachine.button_a.x in
      let divy = cmachine.prize.y / cmachine.button_a.y in 
      if divx >= divy then (
        calc_tokens_used_a divx cmachine.button_a.x cmachine.button_b.x cmachine.prize.x
      ) else (
        calc_tokens_used_a divy cmachine.button_a.y cmachine.button_b.y cmachine.prize.y
      )            
    | B ->
      let divx = cmachine.prize.x / cmachine.button_b.x in
      let divy = cmachine.prize.y / cmachine.button_b.y in
      if divx >= divy then (
        calc_tokens_used_b divx cmachine.button_b.x cmachine.prize.x
      ) else (
        calc_tokens_used_b divy cmachine.button_b.y cmachine.prize.y
      )


let puzzle1 =
  let claw_machines = red_input in
  let pbn = function | None -> '0' | A -> 'A' | B -> 'B' in
  claw_machines |> List.iter 
    (fun m -> printf "claw machine A (x = %d a y = %d) B(x = %d a y = %d) Prize (x = %d y = %d) Cheapers ( x = %c y = %c)\n" 
      m.button_a.x m.button_a.y m.button_b.x m.button_b.y m.prize.x m.prize.y (pbn m.cheaper_x) (pbn m.cheaper_y));
  
  let rec loop cmachines tokens_used =
    match cmachines with
      | [] -> tokens_used
      | h :: t -> 
        loop t (calc_min_tokens_used h)
  in
  loop claw_machines 0  

let _ =
  puzzle1 |> printf "result: %d\n";
  printf "\n"