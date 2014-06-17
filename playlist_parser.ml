open Core.Std
open Yojson
open Csv

type playlist_entry =
  | Divider of string
  | Video of string
  | Exercise of string
  | Quiz

type playlist = { title: string;
                  id: string;
                  tag: string;
                  entries: playlist_entry list;
                }

type exam = { title: string;
              id: int;
              seed: int;
              playlist_ids: string list;
              ids: string list;
            }

exception Unrecognized_line of string list
exception Invalid_csv of string

(* Substring check. Gahd i can't believe this isn't in the std lib *)
let contains s1 s2 =
  let re = Str.regexp_string s2
  in try ignore (Str.search_forward re s1 0); true
     with Not_found -> false

(*  *)
(* Functions for serializing playlists to JSON *)
(*  *)
let playlist_entry_to_json sort_order = function
  | Exercise s -> `Assoc [("entity_id", `String s);
                          ("sort_order",  `Int sort_order);
                          ("entity_kind", `String "Exercise")]
  | Divider s  -> `Assoc [("entity_id", `String s);
                          ("sort_order", `Int sort_order);
                          ("entity_kind", `String "Divider")]
  | Video s    -> `Assoc [("entity_id", `String s);
                          ("sort_order", `Int sort_order);
                          ("entity_kind", `String "Video")]
  | Quiz       -> `Assoc [("entity_id", `String "Quiz");
                          ("sort_order", `Int sort_order);
                          ("entity_kind", `String "Quiz")]

(* Convert a playlist to a JSON data structure that Yojson can accept. Add in the sort_order here too *)
let playlist_to_json {title; entries; id; tag} =
  let entries_to_json = `List (List.mapi ~f:playlist_entry_to_json entries)
  in `Assoc [("title", `String title);
             ("id", `String id);
             ("tag", `String tag);
             ("entries", entries_to_json)]

let playlists_to_json_list l = List.map ~f:playlist_to_json l

(*  *)
(* Functions for reading the CSV into a list of playlists *)
(*  *)
let parse_broken_line_to_ir kind title id =
  if contains title "Playlist" then `Playlist (id, title)
  else if contains kind "Playlist" then `Playlist (id, kind)
  else if contains kind "Video" then `Video title
  else if contains kind "Exercise" then `Exercise title
  else if contains title "Quiz" then `Quiz id
  else if contains title "Subtitle" then `Divider title
  else if contains title "Unit Test" then `Exam (title, id)
  else `Blank

let parse_csv_line_to_ir (line: string list) =
  match line with
  | kind :: title:: id :: _ -> parse_broken_line_to_ir kind title id
  | _ -> raise (Unrecognized_line line)

let parse_csv_to_ir (csv: Csv.t) = List.map ~f:parse_csv_line_to_ir csv
                                   |> List.filter ~f:(fun x -> not (x = `Blank))
(* Now we turn our IR into the playlist data structure *)
let parse_ir_list_to_playlists tag ir_list =
  let parse_first_entry_as_playlist (pl :: ir_list) =
    match pl with
    | `Playlist (id, title) -> ({ id = id; title = title; entries = []; tag = tag } :: [], ir_list)
    | _                     -> raise (Invalid_csv "First non-blank entry is not a playlist description") in
  let parse_ir_to_playlist (current_playlist :: playlists) = function
    | `Playlist (id, title) -> { id = id; title = title; entries = []; tag = tag } :: current_playlist :: playlists
    | `Video title          -> { current_playlist with entries = (Video title :: current_playlist.entries) } :: playlists
    | `Exercise title       -> { current_playlist with entries = (Exercise title :: current_playlist.entries) } :: playlists
    | `Divider title        -> { current_playlist with entries = (Divider title :: current_playlist.entries) } :: playlists
    | `Quiz _               -> { current_playlist with entries = (Quiz :: current_playlist.entries) } :: playlists
    | `Blank                -> current_playlist :: playlists
    | `Exam _               -> current_playlist :: playlists in
  let reverse_entries_to_proper_order pl = { pl with entries = List.rev pl.entries } in
  let reverse_playlists_to_proper_order pl_list = List.map ~f:reverse_entries_to_proper_order pl_list |> List.rev in
  let accum, rest = parse_first_entry_as_playlist ir_list
  in

  List.fold ~init:accum ~f:parse_ir_to_playlist rest |> reverse_playlists_to_proper_order

let parse_ir_list_to_exams ir_list =
  let parse_exam_ids_to_string_list ids_str = Str.split (Str.regexp ",") ids_str |> List.map ~f:String.strip in
  let parse_ir_to_exam exam_list = function
    | `Exam (title, ids) -> { title = title;
                              id = Random.int 2000;
                              seed = Random.int 10000;
                              playlist_ids = parse_exam_ids_to_string_list ids;
                              ids = [];
                            } :: exam_list
    (* ignore everything else *)
    | `Playlist _        -> exam_list
    | `Video _           -> exam_list
    | `Exercise _        -> exam_list
    | `Divider _         -> exam_list
    | `Quiz _            -> exam_list
    | `Blank             -> exam_list
  in

  List.fold_left ~init:[] ~f:parse_ir_to_exam ir_list

let populate_exam_ids (exam: exam) (playlists: playlist list) : exam =
  let find_relevant_playlist id = try List.find_exn ~f:(fun p -> p.id = id) playlists with Not_found -> print_endline id; raise Not_found in
  let get_playlist_exercise_ids pl = List.filter ~f:(function Exercise _ -> true | _ -> false ) pl.entries
                                     |> List.map ~f:(function Exercise slug -> slug) in
  let ids = List.map ~f:(fun pl_id -> (find_relevant_playlist pl_id |> get_playlist_exercise_ids)) exam.playlist_ids
            |> List.concat
  in

  {exam with ids = ids}

let exams_to_json_list exams =
  let string_list_to_json_list l = `List (List.map ~f:(fun s -> `String s) l) in
  let exam_to_json {title; id; seed; playlist_ids; ids} = `Assoc [("title", `String title);
                                                                  ("id", `Int id);
                                                                  ("seed", `Int seed);
                                                                  ("playlist_ids", string_list_to_json_list playlist_ids);
                                                                  ("ids", string_list_to_json_list ids);
                                                                 ]
  in

  List.map ~f:exam_to_json exams

let parse_csv_to_json csv_path tag =
  let ir = Csv.load csv_path |> parse_csv_to_ir in
  let playlists = ir |> parse_ir_list_to_playlists tag in
  let playlist_json = playlists |> playlists_to_json_list in
  let exams = ir |> parse_ir_list_to_exams |> List.map ~f:(fun e -> populate_exam_ids e playlists) in
  let exam_json = exams |> exams_to_json_list in
  (* let exam_json = [] in *)

  [playlist_json; exam_json]

(*  *)
(* Main functions to kick things off and bring it all together *)
(*  *)
let main () =
  let [grade3_playlist_json; grade3_exam_json] = parse_csv_to_json "data/grade3.csv" "Grade 3" in
  let [grade4_playlist_json; grade4_exam_json] = parse_csv_to_json "data/grade4.csv" "Grade 4" in
  let [grade5_playlist_json; grade5_exam_json] = parse_csv_to_json "data/grade5.csv" "Grade 5" in
  let [grade6_playlist_json; grade6_exam_json] = parse_csv_to_json "data/grade6.csv" "Grade 6"
  in List.concat [grade3_playlist_json;
                  grade4_playlist_json;
                  grade5_playlist_json;
                  grade6_playlist_json;]
     |> (fun l -> `List l)
     |> Yojson.Basic.pretty_to_string
     |> print_string
     |> print_newline


let () = if !Sys.interactive then () else main ()
