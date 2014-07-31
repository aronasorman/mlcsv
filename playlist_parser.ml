open Core.Std
open Yojson
open Csv

type playlist_entry =
  | Divider of string
  | Video of string * bool
  | Exercise of string * bool
  | Quiz

type playlist = { title: string;
                  id: string;
                  tag: string;
                  entries: playlist_entry list;
                }

type exam = { title: string;
              id: string;
              seed: int;
              playlist_ids: string list;
              repeats: string;
              ids: string list;
              is_practice: bool;
            }

exception Unrecognized_line of string list
exception Invalid_csv of string
exception Invalid_json_format of Yojson.Basic.json

(* Substring check. Gahd i can't believe this isn't in the std lib *)
let contains s1 s2 =
  let re = Str.regexp_string s2
  in try ignore (Str.search_forward re s1 0); true
     with Not_found -> false

let sanitize_slug id = Str.split (Str.regexp "/") id |> List.last_exn

let add_seed_from_hashed_id exam =
  let hash_title = Digest.string exam.id |> Digest.to_hex |> Fn.flip Str.first_chars 3 |> Printf.sprintf "0x%s" |> int_of_string in

  { exam with seed = hash_title }

(*  *)
(* Functions for serializing playlists to JSON *)
(*  *)
let playlist_entry_to_json sort_order = function
  (* | Exercise (s, e) -> `Assoc [("entity_id", `String (sanitize_slug s)); *)
  | Exercise (s, e) -> `Assoc [("entity_id", `String s);
                          ("sort_order",  `Int sort_order);
                          ("entity_kind", `String "Exercise");
                          ("is_essential", `Bool e)]
  | Divider s       -> `Assoc [("entity_id", `String s);
                          ("sort_order", `Int sort_order);
                          ("entity_kind", `String "Divider")]
  (* | Video (s, e)    -> `Assoc [("entity_id", `String (sanitize_slug s)); *)
  | Video (s, e)    -> `Assoc [("entity_id", `String s);
                          ("sort_order", `Int sort_order);
                          ("entity_kind", `String "Video");
                          ("is_essential", `Bool e)]
  | Quiz            -> `Assoc [("entity_id", `String "Quiz");
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
let parse_broken_line_to_ir kind title playlist_id exam_id repeats essentiality =
  if contains title "Playlist" then `Playlist (playlist_id, title)
  else if contains title "Baseline Exam" then `Exam (title, playlist_id, exam_id, repeats)
  else if contains kind "Playlist" then `Playlist (playlist_id, kind)
  else if contains kind "Video" then `Video (title, essentiality)
  else if contains kind "Exercise" then `Exercise (title, essentiality)
  else if contains title "Quiz" then `Quiz playlist_id
  else if contains title "Subtitle" then `Divider title
  else if contains title "Unit Test" then
    if repeats <> "" then `Exam (title, playlist_id, exam_id, repeats)
    else raise (Invalid_csv (Printf.sprintf "Empty repeat field for %s\n" exam_id))
  else `Blank

let parse_csv_line_to_ir (line: string list) =
  match line with
  | kind :: title:: playlist_id :: exam_id :: essentiality :: repeats :: _ -> parse_broken_line_to_ir kind title playlist_id exam_id repeats essentiality
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
    | `Playlist (id, title)           -> { id = id; title = title; entries = []; tag = tag } :: current_playlist :: playlists
    | `Video (title, essentiality)    ->
       let is_essential = essentiality = "E" in
       { current_playlist with entries = (Video (title, is_essential) :: current_playlist.entries) } :: playlists
    | `Exercise (title, essentiality) ->
       let is_essential = essentiality = "E" in
       { current_playlist with entries = (Exercise (title, is_essential) :: current_playlist.entries) } :: playlists
    | `Divider title                  -> { current_playlist with entries = (Divider title :: current_playlist.entries) } :: playlists
    | `Quiz _                         -> { current_playlist with entries = (Quiz :: current_playlist.entries) } :: playlists
    | `Blank                          -> current_playlist :: playlists
    | `Exam _                         -> current_playlist :: playlists in
  let reverse_entries_to_proper_order pl = { pl with entries = List.rev pl.entries } in
  let reverse_playlists_to_proper_order pl_list = List.map ~f:reverse_entries_to_proper_order pl_list |> List.rev in
  let accum, rest = parse_first_entry_as_playlist ir_list
  in

  List.fold ~init:accum ~f:parse_ir_to_playlist rest |> reverse_playlists_to_proper_order

let parse_ir_list_to_exams ir_list =
  let parse_exam_ids_to_string_list ids_str = Str.split (Str.regexp ",") ids_str |> List.map ~f:String.strip in
  let parse_ir_to_exam exam_list = function
    | `Exam (title, ids, exam_id, repeats) -> { title = title;
                              id = exam_id;
                              seed = 0;
                              repeats = repeats;
                              playlist_ids = parse_exam_ids_to_string_list ids;
                              ids = [];
                              is_practice = false;
                            } :: exam_list
    (* ignore everything else *)
    | `Playlist _                          -> exam_list
    | `Video _                             -> exam_list
    | `Exercise _                          -> exam_list
    | `Divider _                           -> exam_list
    | `Quiz _                              -> exam_list
    | `Blank                               -> exam_list
  in

  List.fold_left ~init:[] ~f:parse_ir_to_exam ir_list |> List.map ~f:add_seed_from_hashed_id

let populate_exam_ids (exam: exam) (playlists: playlist list) : exam =
  let find_relevant_playlist id = try List.find_exn ~f:(fun p -> p.id = id) playlists with Not_found -> Printf.printf "Exam: %s; playlist id: %s\n" exam.title id; raise Not_found in
  let get_playlist_exercise_ids pl = List.filter ~f:(function Exercise _ -> true | _ -> false ) pl.entries
                                     |> List.map ~f:(function Exercise (slug, e) -> slug) in
  let remove_subtitle_ids ids = List.filter ~f:(fun id -> contains id "Subtitle" |> not) ids in
  let sanitize_exam_ids ids = List.map ~f:sanitize_slug ids |> List.dedup ~compare:String.compare |> remove_subtitle_ids in
  let ids = List.map ~f:(fun pl_id -> (find_relevant_playlist pl_id |> get_playlist_exercise_ids)) exam.playlist_ids
            |> List.concat in

  {exam with ids = sanitize_exam_ids ids}

let exams_to_json_list exams =
  let string_list_to_json_list l = `List (List.map ~f:(fun s -> `String s) l) in
  let exam_to_json {title; id; seed; playlist_ids; ids; repeats} = `Assoc [("id", `String id);
                                                                           ("title", `String title);
                                                                           ("seed", `Int seed);
                                                                           ("playlist_ids", string_list_to_json_list playlist_ids);
                                                                           ("repeats", `String repeats);
                                                                           ("ids", string_list_to_json_list ids);
                                                                          ]
  in

  List.map ~f:exam_to_json exams

let add_practice_exams exams =
  let make_practice_exam exam = {exam with id = Printf.sprintf "%s_practice" exam.id;
                                           is_practice = true;
                                           title = Printf.sprintf "%s - Practice" exam.title} in
  let practice_exams = List.filter ~f:(fun e -> contains e.title "Unit Test") exams |> List.map ~f:(fun x -> make_practice_exam x |> add_seed_from_hashed_id) in

  exams @ practice_exams

let parse_csv_to_json csv_path tag other_playlists =
  let ir = Csv.load csv_path |> parse_csv_to_ir in
  let playlists = ir |> parse_ir_list_to_playlists tag |> List.append other_playlists in
  let playlist_json = playlists |> playlists_to_json_list in
  let exams = ir |> parse_ir_list_to_exams |> List.map ~f:(fun e -> populate_exam_ids e playlists) |> add_practice_exams in
  let exam_json = exams |> exams_to_json_list in

  (playlists, exams, playlist_json, exam_json)

let print_jsons l = `List l
                    |> Yojson.Basic.pretty_to_string
                    |> print_string
                    |> print_newline


let save_json_to_file j =
  let save_json id =
    let out = Printf.sprintf "out/%s.json" id |> open_out in
    Yojson.Basic.pretty_to_channel out j in

  match j with
  | `Assoc (("id", `String id) :: _) -> save_json id
  | _ -> raise (Invalid_json_format j)

let save_jsons_to_file js = List.iter ~f:save_json_to_file js

let save_to_playlist_file j =
  let out = open_out "out/playlists.json" in

  Yojson.Basic.pretty_to_channel out (`List j)

(*  *)
(* Main functions to kick things off and bring it all together *)
(*  *)
let main () =
  let (grade3_playlists, grade3_exams, grade3_playlist_json, grade3_exam_json) = parse_csv_to_json "data/grade3.csv" "Grade 3" [] in
  let (grade4_playlists, grade4_exams, grade4_playlist_json, grade4_exam_json) = parse_csv_to_json "data/grade4.csv" "Grade 4" grade3_playlists in
  let (grade5_playlists, grade5_exams, grade5_playlist_json, grade5_exam_json) = parse_csv_to_json "data/grade5.csv" "Grade 5" grade4_playlists in
  let (grade6_playlists, grade6_exams, grade6_playlist_json, grade6_exam_json) = parse_csv_to_json "data/grade6.csv" "Grade 6" grade5_playlists in
  let playlist_jsons = List.concat [grade3_playlist_json; grade4_playlist_json; grade5_playlist_json; grade6_playlist_json;] in
  let exam_jsons = List.concat [grade3_exam_json; grade4_exam_json; grade5_exam_json; grade6_exam_json;] in

  save_to_playlist_file playlist_jsons;
  save_jsons_to_file exam_jsons


let () = if !Sys.interactive then () else main ()
