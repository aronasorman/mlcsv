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
    | `Blank                -> current_playlist :: playlists in
  let reverse_entries_to_proper_order pl = { pl with entries = List.rev pl.entries } in
  let reverse_playlists_to_proper_order pl_list = List.map ~f:reverse_entries_to_proper_order pl_list |> List.rev in
  let accum, rest = parse_first_entry_as_playlist ir_list
  in List.fold ~init:accum ~f:parse_ir_to_playlist rest |> reverse_playlists_to_proper_order

let parse_csv_to_json csv_path tag =
  Csv.load csv_path |> parse_csv_to_ir |> parse_ir_list_to_playlists tag |> playlists_to_json_list

(*  *)
(* Main functions to kick things off and bring it all together *)
(*  *)
let main () =
  let grade3_playlist_json = parse_csv_to_json "data/grade3.csv" "Grade 3" in
  let grade4_playlist_json = parse_csv_to_json "data/grade4.csv" "Grade 4" in
  let grade5_playlist_json = parse_csv_to_json "data/grade5.csv" "Grade 5" in
  let grade6_playlist_json = parse_csv_to_json "data/grade6.csv" "Grade 6"
  in List.concat [grade3_playlist_json;
                  grade4_playlist_json;
                  grade5_playlist_json;
                  grade6_playlist_json;]
     |> (fun l -> `List l)
     |> Yojson.Basic.pretty_to_string
     |> print_string
     |> print_newline


let () = if !Sys.interactive then () else main ()
