open Printf

let file_name = Sys.getcwd () ^ "/bin/input9.txt"


let explode str = Array.init (String.length str) (fun i -> str.[i])

let real_int_of_char c =  int_of_string (String.make 1 c)

type id_array_el = Dot | Number of int

let puzzle1 =
  let ic = open_in file_name in
  let input = (input_line ic) |> explode in
  let id_arr: id_array_el Dynarray.t = Dynarray.create () in  
  let curr_id = ref 0 in
  for i = 0 to (Array.length input) -1 do 
    if (i mod 2) = 0 then (
      let number = real_int_of_char input.(i) in
      for _ = 0 to number - 1 do
        Dynarray.add_last id_arr (Number !curr_id)
      done;
      curr_id := (!curr_id + 1)
    ) else (
      let number = real_int_of_char input.(i) in            
      for _ = 0 to number - 1 do        
        Dynarray.add_last id_arr Dot
      done;      
    )    
  done;      
  printf "\n";
  let dot_count = Dynarray.filter_map (fun a -> if a = Dot then Some a else None) id_arr |> Dynarray.length in  
  for i = (Dynarray.length id_arr) - 1 downto (Dynarray.length id_arr) - dot_count do     
    if Dynarray.get id_arr i <> Dot then (
      for j = 0 to (Dynarray.length id_arr) - 1 do 
        if Dynarray.get id_arr j = Dot then (          
          Dynarray.set id_arr j (Dynarray.get id_arr i);
          Dynarray.set id_arr i Dot;        
        )
      done;      
    )
  done;    
  (* Dynarray.iter (function | Dot -> printf "."; | Number n -> printf "(%d)" n) id_arr;  *)    
  let sum = ref Big_int.zero_big_int in
  Dynarray.iteri (fun idx v ->                
    match v with
      | Dot -> ()
      | Number v -> sum := Big_int.add_big_int !sum (Big_int.mult_int_big_int v (Big_int.big_int_of_int idx))
    ) id_arr;
  !sum

let get_id_qty darr curr_idx num =
  let exception Break of int in
  try
    let sum = ref 0 in
    for i = curr_idx downto 0 do 
      if Dynarray.get darr i <> (Number num) then 
        raise (Break !sum)
      else (
        sum := !sum + 1
      )
    done;
    !sum
  with
    | Break sum -> sum

let find_dots_valid_position darr qty =
  let exception Break of int in
  let exception Continue of { sum : int; start_at_idx : int} in
  try
    (* let idx_starts_at = ref 0 in *)
    for i = 0 to (Dynarray.length darr) - 1 do       
      match Dynarray.get darr i with
        | Number _ -> ()
        | Dot -> 
          try
            let sumr = ref 0 in          
            for j = i to (Dynarray.length darr) -1 do 
              match Dynarray.get darr j with
                | Dot -> sumr := !sumr + 1
                | Number _ -> 
                  raise (Continue { sum = !sumr; start_at_idx = i})
            done
          with
            | Continue e -> if e.sum >= qty then raise (Break e.start_at_idx) else ()
    done;
    None
  with
    | Break idx -> Some idx

    
let puzzle2 =
    let ic = open_in file_name in
  let input = (input_line ic) |> explode in
  let id_arr: id_array_el Dynarray.t = Dynarray.create () in  
  let curr_id = ref 0 in
  for i = 0 to (Array.length input) -1 do 
    if (i mod 2) = 0 then (
      let number = real_int_of_char input.(i) in
      for _ = 0 to number - 1 do
        Dynarray.add_last id_arr (Number !curr_id)
      done;
      curr_id := (!curr_id + 1)
    ) else (
      let number = real_int_of_char input.(i) in            
      for _ = 0 to number - 1 do        
        Dynarray.add_last id_arr Dot
      done;      
    )    
  done;      
  printf "\n";
  (* let dot_count = Dynarray.filter_map (fun a -> if a = Dot then Some a else None) id_arr |> Dynarray.length in   *)

  let htbl = Hashtbl.create 10000 in
  for i = (Dynarray.length id_arr) - 1 downto 0 do         
    match Dynarray.get id_arr i with
      | Dot -> ()
      | Number num ->              
        if Hashtbl.mem htbl num then 
          () 
        else (
          Hashtbl.add htbl num 0;
          let qty = get_id_qty id_arr i num in          
          let dots_start_idx = find_dots_valid_position id_arr qty in          
          match dots_start_idx with
            | None -> ()
            | Some idx ->                               
              if idx < i then (
                for j = idx to (idx + qty) - 1 do                  
                  Dynarray.set id_arr j (Number num);                                
                done;  
                for k = (i - (qty - 1)) to i  do                   
                    Dynarray.set id_arr k Dot
                done;                
              )
              
        );
    (* Dynarray.iter (function | Dot -> printf ".%!"; | Number n -> printf "(%d%!)" n) id_arr; printf "\n";           *)
  done;        

  let sum = ref Big_int.zero_big_int in
  Dynarray.iteri (fun idx v ->                
    match v with
      | Dot -> ()
      | Number v -> sum := Big_int.add_big_int !sum (Big_int.mult_int_big_int v (Big_int.big_int_of_int idx))
    ) id_arr;
  !sum

let () =
  puzzle1 |> Big_int.string_of_big_int |> printf "\nresult: %s\n";
  puzzle2 |> Big_int.string_of_big_int |> printf "\nresult 2: %s\n";
  printf "\n"