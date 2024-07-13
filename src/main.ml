type status =
  | Pending
  | Interview_Request
  | Accepted
  | Declined

type interest =
  | Not_Interested
  | Interested
  | Very_Interested

type entry =
  { job_title : string
  ; company : string
  ; desc : string
  ; status : status
  ; interest : interest
  }

let status_tostr = function
  | Pending -> "Pending"
  | Interview_Request -> "Interview_Request"
  | Accepted -> "Accepted"
  | Declined -> "Declined"

let interest_tostr = function
  | Not_Interested -> "Not_Interested"
  | Interested -> "Interested"
  | Very_Interested -> "Very_Interested"

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write_to_file fp = failwith "todo"

let () =
  let s = "hello,world,test\njohn,doe,foobar" in

  let rec create_csv lst =
    match lst with
    | [] -> []
    | hd :: tl ->
       let split = String.split_on_char ',' hd in
       [split] @ create_csv tl in

  let csv = String.split_on_char '\n' s |> create_csv in

  List.iter (fun sublst -> print_endline "entry"; List.iter (fun s -> Printf.printf "item: %s\n" s) sublst) csv
