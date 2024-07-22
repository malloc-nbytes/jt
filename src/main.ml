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

type ctx =
  { entries : entry list
  ; last_saved_entries : entry list
  }

(* Default environment variable for the jt_info filepath *)
let jt_info_variable = "JT_INFO_FILE"

(* The jt info filepath based on either the
 * environment variable `jt_info_variable` or
 * if it does not exist, the default dotfile
 * in the home directory *)
let info_fp = match Sys.getenv_opt jt_info_variable with
  | Some p -> p
  | None ->
     let fp = Filename.concat (Sys.getenv "HOME") ".jt_info.csv" in
     Printf.printf "%s environment variable has not been set, using path: %s\n" jt_info_variable fp;
     fp

(* Convert jt status to a string *)
let status_tostr = function
  | Pending -> "Pending"
  | Interview_Request -> "Interview_Request"
  | Accepted -> "Accepted"
  | Declined -> "Declined"

(* Convert a string to jt status *)
let str_tostatus = function
  | "Pending" -> Pending
  | "Interview_Request" -> Interview_Request
  | "Accepted" -> Accepted
  | "Declined" -> Declined
  | _ -> failwith "invalid str to convert to status"

(* Convert jt interest to a string *)
let interest_tostr = function
  | Not_Interested -> "Not Interested"
  | Interested -> "Interested"
  | Very_Interested -> "Very Interested"

(* Convert a string to jt interest *)
let str_tointerest = function
  | "Not_Interested" -> Not_Interested
  | "Interested" -> Interested
  | "Very_Interested" -> Very_Interested
  | _ -> failwith "invalid str to convert to interest"

(* Read the file `filename` and get its contents *)
let read_whole_file filename =
  let ch = open_in_bin filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(* Convert a CSV file to valid jt entries *)
let rec csv_to_jt_entries csv =
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

(* Write `content` to the filepath `fp` *)
let write_to_file fp content =
  failwith "todo"

(* Get the stored info from the jt_info filepath *)
let get_info_from_jt_fp file_path =
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

(* Convert jt entries to valid csv *)
let rec entries_to_csv entries =
  failwith "todo"

(* Begin the repl for continuous user querying *)
let rec repl ctx =
  (* Display an entry to stdout *)
  let display_entry e =
    Printf.printf "  Job Title: %s\n" e.job_title;
    Printf.printf "  Company: %s\n" e.company;
    Printf.printf "  Pay: %s\n" e.pay;
    Printf.printf "  Description: %s\n" e.desc;
    Printf.printf "  Status: %s\n" @@ status_tostr e.status;
    Printf.printf "  Interest: %s\n" @@ interest_tostr e.interest;
    Printf.printf "  Link: %s\n" e.link;
    Printf.printf "  Extra information: %s\n" e.extra_info in

  (* List all available entries *)
  let ls entries =
    List.iteri (fun i e ->
        Printf.printf "Entry %d\n" i;
        display_entry e) entries in

  (* View a specific entry based on user input *)
  let view_specific_entry entries =
    failwith "todo" in

  let write_entries entries =
    let content = entries_to_csv entries in
    write_to_file info_fp content in

  let quit ctx =
    failwith "todo" in

  (* Begin repl main loop *)
  Printf.printf "New Entry     (n) [0]\n";
  Printf.printf "List Entries  (l) [1]\n";
  Printf.printf "Remove Entry  (r) [2]\n";
  Printf.printf "View Entry    (v) [3]\n";
  Printf.printf "Write Entries (w) [4]\n";
  Printf.printf "Quit          (q) [5]\n";
  let inp = read_line () in
  match inp with
  | "n"|"0" -> failwith "todo"
  | "l"|"1" ->
     ls ctx.entries;
     repl ctx;
  | "r"|"2" -> failwith "todo"
  | "v"|"3" ->
     view_specific_entry ctx.entries;
     repl ctx
  | "w"|"4" ->
     write_entries ctx.entries;
     repl {ctx with last_saved_entries = ctx.entries}
  | "q"|"5" ->
     quit ctx;
     repl ctx
  | _ ->
     Printf.printf "invalid command %s\n" inp;
     repl ctx

let () =
  let info = match get_info_from_jt_fp info_fp with
    | Some k -> k
    | None -> "" in

  let rec create_csv lines =
    match lines with
    | [] -> []
    | hd :: tl ->
       let split = String.split_on_char ',' hd in
       [split] @ create_csv tl in

  let entries = String.split_on_char '\n' info |> create_csv |> csv_to_jt_entries in
  let ctx = {entries; last_saved_entries = entries } in
  repl ctx
