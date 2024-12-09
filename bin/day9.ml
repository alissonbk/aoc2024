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
  let dot_count = Dynarray.filter_map (fun a -> if a = Dot then Some a else None) id_arr |> Dynarray.length in  
  for i = (Dynarray.length id_arr) - 1 downto (Dynarray.length id_arr) - dot_count do     
    if Dynarray.get id_arr i <> Dot && i <> Dynarray.length id_arr then (
      for j = 0 to (Dynarray.length id_arr) - 1 do 
        if Dynarray.get id_arr j = Dot then (          
          Dynarray.set id_arr j (Dynarray.get id_arr i);
          Dynarray.set id_arr i Dot;        
        )
      done;      
    )
  done;    
  (* Dynarray.iter (printf "%c") id_arr; printf "\n"; *)    
  let sum = ref Big_int.zero_big_int in
  Dynarray.iteri (fun idx v ->                
    match v with
      | Dot -> ()
      | Number v -> sum := Big_int.add_big_int !sum (Big_int.mult_int_big_int v (Big_int.big_int_of_int idx))
    ) id_arr;
  !sum

let () =
  puzzle1 |> Big_int.string_of_big_int |> printf "\nresult: %s\n";
  printf "\n"