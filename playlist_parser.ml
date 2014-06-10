open Core.Std
open Yojson
open Csv

type slug = string
type title = string

type playlist_entry =
  | Divider of title
  | Video of slug
  | Exercise of slug
  | Quiz

type playlist = { title: string;
                  entries: playlist_entry list;
                }

(* Functions for serializing playlists to JSON *)
let playlist_entry_to_json = function
  | Exercise s -> `Assoc [("entity_id", `String s); ("entity_kind", `String "Exercise")]
  | Divider s -> `Assoc [("entity_id", `String s); ("entity_kind", `String "Divider")]
  | Video s -> `Assoc [("entity_id", `String s); ("entity_kind", `String "Video")]
  | Quiz -> `Assoc [("entity_id", `String "Quiz"); ("entity_kind", `String "Quiz")]

let playlist_to_json {title; entries} =
  let entries_to_json = List.map ~f:playlist_entry_to_json entries |> `List
  in `Assoc [("title", `String title);
             ("entries", entries_to_json)]

let csvlines_to_one_playlist (csv: Csv.t) : playlist = {title =  "hi"; entries = []}

(* let data () = Csv.load "data/grade3.csv" *)

(* let process_data () = data *)
