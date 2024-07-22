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
  ; pay : string
  ; desc : string
  ; status : status
  ; interest : interest
  ; link : string
  ; extra_info : string
  }

let status_tostr = function
  | Pending -> "Pending"
  | Interview_Request -> "Interview_Request"
  | Accepted -> "Accepted"
  | Declined -> "Declined"

let str_tostatus = function
  | "Pending" -> Pending
  | "Interview_Request" -> Interview_Request
  | "Accepted" -> Accepted
  | "Declined" -> Declined
  | _ -> failwith "invalid str to convert to status"

let interest_tostr = function
  | Not_Interested -> "Not Interested"
  | Interested -> "Interested"
  | Very_Interested -> "Very Interested"

let str_tointerest = function
  | "Not_Interested" -> Not_Interested
  | "Interested" -> Interested
  | "Very_Interested" -> Very_Interested
  | _ -> failwith "invalid str to convert to interest"

let display_entry e =
  Printf.printf "  Job Title: %s\n" e.job_title;
  Printf.printf "  Company: %s\n" e.company;
  Printf.printf "  Pay: %s\n" e.pay;
  Printf.printf "  Description: %s\n" e.desc;
  Printf.printf "  Status: %s\n" @@ status_tostr e.status;
  Printf.printf "  Interest: %s\n" @@ interest_tostr e.interest;
  Printf.printf "  Link: %s\n" e.link;
  Printf.printf "  Extra information: %s\n" e.extra_info

let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let write_to_file fp = failwith "todo"

let get_info file_path =
  if Sys.file_exists file_path then
    let _ = Printf.printf "Found info file: %s\n" file_path in
    let ic = open_in file_path in
    let content = really_input_string ic (in_channel_length ic) in
    let _ = close_in ic in
    Some content
  else
    let _ = Printf.printf "File does not exist. Creating...\n" in
    let oc = open_out file_path in
    let _ = close_out oc in
    None

let rec ls = function
  | [] -> ()
  | hd :: tl ->
     display_entry hd;
     ls tl

let rec repl entries =
  Printf.printf "New Entry    (n) [0]\n";
  Printf.printf "List Entries (l) [1]\n";
  Printf.printf "Remove Entry (r) [2]\n";
  Printf.printf "View Entry   (v) [3]\n";

  let inp = read_line () in
  match inp with
  | "n"|"0" -> failwith "todo"
  | "l"|"1" -> ls entries
  | "r"|"2" -> failwith "todo"
  | "v"|"3" -> failwith "todo"
  | _ ->
     Printf.printf "invalid command %s\n" inp;
     repl entries

let jt_info_variable = "JT_INFO_FILE"

let info_fp = match Sys.getenv_opt jt_info_variable with
  | Some p -> p
  | None ->
     let fp = Filename.concat (Sys.getenv "HOME") ".jt_info.csv" in
     Printf.printf "%s environment variable has not been set, using path: %s\n" jt_info_variable fp;
     fp

let rec csv_to_entries csv =
  let rec aux = function
    | [] -> []
    | hd :: tl ->
       let job_title, company, pay, desc, status, interest, link, extra_info =
         match hd with
         | [jt; cm; py; de; st; intrst; lnk; ei] -> jt, cm, py, de, st, intrst, lnk, ei
         | _ -> failwith "invalid csv line to convert to entry" in
       [{job_title; company; pay; desc;
         status = str_tostatus status; interest = str_tointerest interest;
         link; extra_info}] @ aux tl in
  aux csv

let () =
  let info = match get_info info_fp with
    | Some k -> k
    | None -> "" in

  let rec create_csv lst =
    match lst with
    | [] -> []
    | hd :: tl ->
       let split = String.split_on_char ',' hd in
       [split] @ create_csv tl in

  let entries = String.split_on_char '\n' info |> create_csv |> csv_to_entries in
  repl entries

  (* List.iter (fun sublst -> print_endline "entry"; List.iter (fun s -> Printf.printf "item: %s\n" s) sublst) csv *)
