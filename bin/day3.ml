open Printf

let file_name = Sys.getcwd () ^ "/bin/input3.txt"

let read_all_as_string filename =
  let ic = open_in filename in
  let length = in_channel_length ic in
  let content = really_input_string ic length in
  close_in ic;
  content

let extract_result s = 
  match s with
    | "" -> failwith "invalid string"
    | s -> s 
      |> String.split_on_char '(' 
      |> List.filter (fun s -> s <> "mul") 
      |> List.hd
      |> String.split_on_char ','
      |> function
        | [] -> failwith "invalid input sintrg"
        | e1 :: e2 :: [] -> 
          (int_of_string e1) * (e2 |> String.split_on_char ')' |> List.hd |> int_of_string)
        | _ -> failwith "fodase"

let regexp = Str.regexp "mul(\\([0-9][0-9]?[0-9]?\\),\\([0-9][0-9]?[0-9]?\\))"
let regexp2 = Str.regexp "\\(mul(\\([0-9][0-9]?[0-9]?\\),\\([0-9][0-9]?[0-9]?\\))\\)\\|\\(do()\\)\\|\\(don't()\\)"
let puzzle1 = 
  let input = read_all_as_string file_name in      
  let rec sum_valids remaining_str sum =
      try
        let _ = Str.search_forward regexp remaining_str 0 in        
        let matched_string = Str.matched_string remaining_str in      
        let new_remaining = String.sub remaining_str (Str.match_end ()) (String.length remaining_str - Str.match_end ()) in                
        let result = extract_result matched_string in
        sum_valids new_remaining (sum + result)
      with
        | Not_found -> printf "not found\n"; sum
  in
  sum_valids input 0

let puzzle2 = 
  let input = read_all_as_string file_name in      
  let rec sum_valids remaining_str sum is_enabled =
      try
        let _ = Str.search_forward regexp2 remaining_str 0 in     
        let matched_string = Str.matched_string remaining_str in      
        let new_remaining = String.sub remaining_str (Str.match_end ()) (String.length remaining_str - Str.match_end ()) in                        

        match matched_string with
          | "do()" -> sum_valids new_remaining sum true
          | "don't()" -> sum_valids new_remaining sum false
          | _ ->
            let result = extract_result matched_string in                
            if is_enabled then
              sum_valids new_remaining (sum + result) true
            else
              sum_valids new_remaining sum false
      with
        | Not_found -> printf "not found\n"; sum
  in
  sum_valids input 0 true

let _ = 
  puzzle1 |> printf "result 1: %d\n";
  puzzle2 |> printf "result 2: %d\n";
  printf "\n"
