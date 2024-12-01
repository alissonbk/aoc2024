open Printf

let file_name = Sys.getcwd () ^ "/bin/input1.txt"

let create_lists chan_in =
  let rec loop ll rl =
    try      
      match input_line chan_in with
        | "" -> failwith "invliad string"
        | s ->
          String.split_on_char ' ' s             
            |> List.filter (fun x -> x <> "") 
            |> function
              | [] -> failwith "empty list"
              | e1 :: e2 :: [] -> 
                let n1 = String.trim e1 |> int_of_string in
                let n2 = String.trim e2 |> int_of_string in
                loop (n1 :: ll) (n2 :: rl)
              | _ :: _ -> failwith "more than 2 elements"
    with
      | End_of_file -> ((List.sort (fun a b -> a - b) ll), (List.sort (fun a b -> a - b) rl))
  in
  loop [] []

let calc_similarity l v =
  let rec loop l sum =
    match l with
      | [] -> v * sum
      | h :: t ->
        if h = v then loop t (sum + 1) else loop t sum
  in  
  loop l 0

let puzzle1 =
  let list_tuple = create_lists (open_in file_name) in
  let rec sum_lists lt sum =
    match lt with
      | ([], []) -> sum
      | (le :: t1, re :: t2) ->
        if le <= re then sum_lists (t1, t2) (sum + (re - le)) else sum_lists (t1, t2) (sum + (le - re))
      | _ -> failwith "invalid pattern"      
  in
  sum_lists list_tuple 0

let puzzle2 =
  let list_tuple = create_lists (open_in file_name) in
  let rec sum_similarity lt sum =
    match lt with
      | ([], _) -> sum
      | (el :: tl, lr) -> sum_similarity (tl, lr) (sum + (calc_similarity lr el))      
  in
  sum_similarity list_tuple 0

let _ = 
  puzzle1 |> printf "result 1: %d\n";
  puzzle2 |> printf "result 2: %d";
  printf "\n"