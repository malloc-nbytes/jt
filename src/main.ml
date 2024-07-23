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
  let display_entry e : unit =
    Printf.printf "  Job Title: %s\n" e.job_title;
    Printf.printf "  Company: %s\n" e.company;
    Printf.printf "  Pay: %s\n" e.pay;
    Printf.printf "  Description: %s\n" e.desc;
    Printf.printf "  Status: %s\n" @@ status_tostr e.status;
    Printf.printf "  Interest: %s\n" @@ interest_tostr e.interest;
    Printf.printf "  Link: %s\n" e.link;
    Printf.printf "  Extra information: %s\n" e.extra_info in

  (* List all available entries *)
  let list_all_entries entries : unit =
    List.iteri (fun i e ->
        Printf.printf "Entry %d\n" i;
        display_entry e) entries in

  (* Write the entries to the jt info file *)
  let write_entries entries : unit =
    let content = entries_to_csv entries in
    write_to_file info_fp content in

  (* Quit the application *)
  let quit ctx =
    (if ctx.entries != ctx.last_saved_entries then
       let rec loop () =
         Printf.printf "There are unsaved changes, would you like to save them? [y/n] ";
         match read_line () with
         | "Y"|"y"|"Yes"|"yes" -> write_entries ctx.entries
         | "N"|"n"|"No"|"no" -> ()
         | invalid -> Printf.printf "invalid option `%s`" invalid; loop () in
       loop ());
    exit 0 in

  (* Remove a specific entry *)
  let remove_entry entries =
    let rec remove entries i del =
      match entries with
      | [] -> Printf.printf "index `%d` is out of range" del; []
      | hd :: tl when i = del -> tl
      | hd :: tl -> [hd] @ remove tl (i+1) del in

    let rec loop () =
      list_all_entries entries;
      Printf.printf "Select a number to remove: ";
      let inp = read_line () in
      try
        let del_idx = int_of_string inp in
        remove entries 0 del_idx
      with
      | Failure _ ->
         Printf.printf "`%s` is not a valid option" inp;
         loop ()
    in

    loop ()
  in

  (* Edit a specific entry *)
  let edit_entry entries =
    let rec edit_entry_loop entry =
      let get_input msg prev =
        Printf.printf "%s (previous: %s): " msg prev;
        match read_line () with
        | "" -> "none"
        | k -> k in

      let rec get_status prev =
        Printf.printf "Status: [0: Pending][1: Interview Request][2: Accepted][3: Declined] (previous: %s): " prev;
        match read_line () with
        | "0"|"Pending" -> Pending
        | "1"|"Interview Request" -> Interview_Request
        | "2"|"Accepted" -> Accepted
        | "3"|"Declined" -> Declined
        | k ->
          Printf.printf "Invalid Status type: `%s`\n" k;
          get_status prev in

      let rec get_interest prev =
        Printf.printf "Interest: [0 : Not Interested][1 : Interested][2 : Very Interested] (previous: %s): " prev;
        match read_line () with
        | "0"|"Not Interested" -> Not_Interested
        | "1"|"Interested" -> Interested
        | "2"|"Very Interested" -> Very_Interested
        | k ->
          Printf.printf "Invalid Interest type: `%s`\n" k;
          get_interest prev in

      Printf.printf "Enter the number to edit:\n";
      Printf.printf "Select one of:\n  [0 : Job Title][1 : Company][2 : Pay][3 : Description][4 : Status][5 : Interest][6 : Link][7 : Extra Information] (or `q` to quit): ";
      match read_line () with
      | "0"|"Job Title" -> edit_entry_loop {entry with job_title = get_input "Job Title" entry.job_title}
      | "1"|"Company" -> edit_entry_loop {entry with company = get_input "Company" entry.company}
      | "2"|"Pay" -> edit_entry_loop {entry with pay = get_input "Pay" entry.pay}
      | "3"|"Description" -> edit_entry_loop {entry with desc = get_input "Description" entry.desc}
      | "4"|"Status" -> edit_entry_loop {entry with status = get_status @@ status_tostr entry.status}
      | "5"|"Interest" -> edit_entry_loop {entry with interest = get_interest @@ interest_tostr entry.interest}
      | "6"|"Link" -> edit_entry_loop {entry with link = get_input "Link" entry.link}
      | "7"|"Extra Information" -> edit_entry_loop {entry with extra_info = get_input "Extra Information" entry.extra_info}
      | "q"|"quit" -> entry
      | invalid -> Printf.printf "invalid choice `%s`\n" invalid; edit_entry_loop entry in

    let rec edit entries i idx =
      match entries with
      | [] -> []
      | hd :: tl when i = idx -> [edit_entry_loop hd] @ tl
      | hd :: tl -> [hd] @ edit tl (i+1) idx in

    let rec loop () =
      list_all_entries entries;
      Printf.printf "Select a number to edit: ";
      let inp = read_line () in
      try
        let edit_idx = int_of_string inp in
        edit entries 0 edit_idx
      with
      | Failure _ ->
         Printf.printf "`%s` is not a valid option" inp;
         loop () in
    loop () in

  (* Create a new entry *)
  let create_new_entry entries =
    let get_input msg =
      Printf.printf "%s: " msg;
      match read_line () with
      | "" -> "none"
      | k -> k in

    let rec get_status () =
      Printf.printf "Status: [0: Pending][1: Interview Request][2: Accepted][3: Declined] ";
      match read_line () with
      | "0"|"Pending" -> Pending
      | "1"|"Interview Request" -> Interview_Request
      | "2"|"Accepted" -> Accepted
      | "3"|"Declined" -> Declined
      | k ->
        Printf.printf "Invalid Status type: `%s`\n" k;
        get_status () in

    let rec get_interest () =
      Printf.printf "Interest: [0 : Not Interested][1 : Interested][2 : Very Interested] ";
      match read_line () with
      | "0"|"Not Interested" -> Not_Interested
      | "1"|"Interested" -> Interested
      | "2"|"Very Interested" -> Very_Interested
      | k ->
        Printf.printf "Invalid Interest type: `%s`\n" k;
        get_interest () in

    let job_title = get_input "Job Title" in
    let company = get_input "Company Name" in
    let pay = get_input "Pay" in
    let desc = get_input "Description" in
    let status = get_status () in
    let interest = get_interest () in
    let link = get_input "Link" in
    let extra_info = get_input "Extra Information" in
    entries @ [{job_title; company; pay; desc; status; interest; link; extra_info}]
  in

  (* Begin repl main loop *)
  Printf.printf "New Entry     (n)[0]\n";
  Printf.printf "Remove Entry  (r)[1]\n";
  Printf.printf "Edit Entry    (e)[2]\n";
  Printf.printf "List Entries  (l)[3]\n";
  Printf.printf "Write Entries (w)[4]\n";
  Printf.printf "Quit          (q)[5]\n";

  match read_line () with
  | "New Entry"     |"n"|"0" -> repl {ctx with entries = create_new_entry ctx.entries}
  | "Remove Entry"  |"r"|"1" -> repl {ctx with entries = remove_entry ctx.entries}
  | "Edit Entry"    |"e"|"2" -> repl {ctx with entries = edit_entry ctx.entries}
  | "List Entries"  |"l"|"3" -> list_all_entries ctx.entries; repl ctx
  | "Write Entries" |"w"|"4" -> write_entries ctx.entries; repl {ctx with last_saved_entries = ctx.entries}
  | "Quit"          |"q"|"5" -> quit ctx
  | invalid ->
     Printf.printf "invalid command `%s`\n" invalid;
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
