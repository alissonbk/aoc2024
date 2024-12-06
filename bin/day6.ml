open Printf

let file_name = Sys.getcwd () ^ "/bin/input6.txt"


let explode str = Array.init (String.length str) (fun i -> str.[i])

type pointing_side = | UP | DOWN | LEFT | RIGHT
type pos = { x : int; y : int }
type pointer = { side : pointing_side; pos : pos }


let invalid_pos = { x = (-1); y = (-1)}
let get_pointing_side : char -> pointing_side option =
  function
    | '^' -> (Some UP)
    | '>' -> (Some RIGHT)
    | '<' -> (Some LEFT)
    | 'v' -> (Some DOWN)
    | _ -> None    

let read_input =
  let ic = open_in file_name in    
  let pointer_loc = ref { side = LEFT; pos = invalid_pos } in
  let rec loop lst curr =
    try       
      match input_line ic with
        | "" -> failwith "invalid str"
        | s -> 
          let exploded = (explode s) in
          Array.iteri 
            (fun idx x  ->               
              match get_pointing_side x with
                | None -> ()
                | Some p -> pointer_loc := { side = p; pos = {x=idx; y= curr}}
            ) 
            exploded;
          loop (exploded :: lst) (curr + 1)
    with
      | End_of_file -> (Array.of_list (List.rev lst), !pointer_loc)
  in
  loop [] 0


let rotate_ptr_side (ps : pointing_side) =
  match ps with
    | UP -> RIGHT
    | RIGHT -> DOWN
    | DOWN -> LEFT
    | LEFT -> UP

let move_based_on_pointer pointer move_forward =
  match pointer.side with
    | UP -> if move_forward then {x= pointer.pos.x; y = (pointer.pos.y - 1)} else {x= pointer.pos.x; y = (pointer.pos.y + 1)}
    | RIGHT -> if move_forward then {x= (pointer.pos.x + 1); y = pointer.pos.y} else {x= (pointer.pos.x - 1); y = pointer.pos.y}
    | DOWN -> if move_forward then {x= pointer.pos.x; y = (pointer.pos.y + 1)} else {x= pointer.pos.x; y = (pointer.pos.y - 1)}
    | LEFT -> if move_forward then {x= (pointer.pos.x - 1); y = pointer.pos.y} else {x= (pointer.pos.x + 1); y = pointer.pos.y}    


let puzzle1 = 
  let input, pointer_location = read_input in
  input |> Array.iter (fun x -> Array.iter (printf "%c") x; printf "\n");
  ignore pointer_location.side;
  printf "pointer location: %d %d \n" pointer_location.pos.x pointer_location.pos.y;
  let visited_table = Hashtbl.create 5000 in
  let sumr = ref 1 in  
  Hashtbl.add visited_table pointer_location.pos true;
  let rec loop pointer_location =
    try
      let new_pos = move_based_on_pointer pointer_location true in
      if input.(new_pos.y).(new_pos.x) = '#' then 
        loop { side = (rotate_ptr_side pointer_location.side); pos = pointer_location.pos}      
      else (
        if Hashtbl.mem visited_table new_pos then 
          loop {side = pointer_location.side; pos = new_pos} 
        else(
          Hashtbl.add visited_table new_pos true;
          sumr := (!sumr + 1);
          loop {side = pointer_location.side; pos = new_pos}
        )
      )
    with
      | Invalid_argument _ -> printf "\nout of bounds\n"; !sumr
  in
  loop pointer_location

let () =
  puzzle1 |> printf "result: %d\n";
  printf "\n"

