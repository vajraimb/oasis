open Std
open Std.Collections

module Json = Std.Data.Json

(** Local agent sessions, read from each agent's own files. This is the
    reverse of a task Oasis dispatched: Cursor, DeepSeek Harness, Claude,
    Codex, ZCode, WorkBuddy, and Grok sessions already on this machine.
    Auth files and tool-result dumps are not opened, and none of these
    processes are signalled. *)

type session_summary = {
  agent: string;
  project: string;
  session_id: string;
  last_ts: float;
  turns: int;
  tools: int;
  last_excerpt: string;
  running: bool;
  resumable: bool;
}

(** Session files sometimes contain a stray byte. Drop those so the
    activity JSON stays valid. Valid multibyte text is kept. *)
let utf8_clean = fun s ->
  let byte = fun i ->
    match String.get s ~at:i with
    | None -> 0
    | Some c -> Char.to_int c
  in
  let cont = fun b -> b >= 128 && b < 192 in
  let len = String.length s in
  let buf = ref "" in
  let rec go i =
    if i >= len then !buf
    else
      let b0 = byte i in
      let n =
        if b0 < 128 then 1
        else if b0 >= 194 && b0 <= 223 then 2
        else if b0 >= 224 && b0 <= 239 then 3
        else if b0 >= 240 && b0 <= 244 then 4
        else 0
      in
      let ok =
        if n = 0 || i + n > len then false
        else if n = 1 then true
        else
          let rec follows k =
            if k >= n then true else if cont (byte (i + k)) then follows (k + 1) else false
          in
          follows 1
      in
      if ok then (
        buf := !buf ^ String.sub s ~offset:i ~len:n;
        go (i + n)
      ) else
        go (i + 1)
  in
  go 0

let summary_json = fun (s : session_summary) ->
  Json.obj
    [
      ("agent", Json.string (utf8_clean s.agent));
      ("project", Json.string (utf8_clean s.project));
      ("session_id", Json.string (utf8_clean s.session_id));
      ("last_ts", Json.float s.last_ts);
      ("turns", Json.int s.turns);
      ("tools", Json.int s.tools);
      ("last_excerpt", Json.string (utf8_clean s.last_excerpt));
      ("running", Json.bool s.running);
      ("resumable", Json.bool s.resumable);
    ]

let per_agent = 8

let recent_s = 180.0

let now_s = fun () -> Time.SystemTime.now () |> Time.SystemTime.secs_float

let recently = fun ~now ts -> ts > 0.0 && now -. ts < recent_s

let path_of = fun s -> Path.from_string s |> Result.unwrap

let read_text = fun path ->
  match Fs.read_to_string (path_of path) with
  | Error _ -> None
  | Ok s -> Some s

let read_json = fun path ->
  match read_text path with
  | None -> None
  | Some s -> (
      match Json.from_string s with
      | Ok j -> Some j
      | Error _ -> None
    )

let mtime = fun path ->
  match Fs.metadata (path_of path) with
  | Error _ -> None
  | Ok m -> Some (Fs.Metadata.modified m)

let is_dir = fun path ->
  match Fs.metadata (path_of path) with
  | Ok m -> Fs.Metadata.is_dir m
  | Error _ -> false

let is_file = fun path ->
  match Fs.metadata (path_of path) with
  | Ok m -> Fs.Metadata.is_file m
  | Error _ -> false

let dir_names = fun path ->
  if not (is_dir path) then []
  else
    match Fs.read_dir (path_of path) with
    | Error _ -> []
    | Ok entries ->
        Iter.MutIterator.to_list entries |> List.map ~fn:Path.basename

let read_prefix = fun path ~n ->
  match Fs.File.open_read (path_of path) with
  | Error _ -> ""
  | Ok file ->
      let raw = IO.Bytes.create ~size:n in
      let got =
        match Fs.File.read file raw ~offset:0 ~len:n with
        | Ok k when k > 0 ->
            let s = IO.Bytes.to_string raw in
            let len = String.length s in
            if k < len then String.sub s ~offset:0 ~len:k else s
        | _ -> ""
      in
      let _ = Fs.File.close file in
      got

let file_tail = fun path ~n ->
  match read_text path with
  | None -> ""
  | Some content ->
      let len = String.length content in
      if len <= n then content
      else String.sub content ~offset:(len - n) ~len:n

let json_str = fun key j ->
  Json.get_field key j
  |> Option.and_then ~fn:Json.get_string
  |> Option.unwrap_or ~default:""

let millis = fun j ->
  match j with
  | Json.Int n -> Some (float_of_int n /. 1000.0)
  | Json.Float f -> Some (f /. 1000.0)
  | _ -> None

let excerpt_of = fun text ->
  let trimmed = String.trim text in
  let len = String.length trimmed in
  if len <= 160 then trimmed
  else String.sub trimmed ~offset:0 ~len:160 ^ "…"

let strip_suffix = fun s ~suffix ->
  if String.ends_with ~suffix s then
    let len = String.length s - String.length suffix in
    if len >= 0 then String.sub s ~offset:0 ~len else s
  else s

let quoted_after = fun body key ->
  let parts = String.split ~by:("\"" ^ key ^ "\":\"") body in
  match parts with
  | _ :: value :: _ -> (
      match String.split ~by:"\"" value with
      | hit :: _ when not (String.is_empty hit) -> Some hit
      | _ -> None
    )
  | _ -> None

let decode_slashes = fun s ->
  let s = String.split ~by:"%2F" s |> String.concat "/" in
  String.split ~by:"%2f" s |> String.concat "/"

let by_recent = fun items ->
  List.sort items ~compare:(fun a b ->
    if a.last_ts > b.last_ts then LT
    else if a.last_ts < b.last_ts then GT
    else EQ)

let cap = fun items -> by_recent items |> List.take ~len:per_agent

let row = fun ~agent ~project ~session_id ~last_ts ~turns ~tools ~excerpt ~running ->
  {
    agent;
    project;
    session_id;
    last_ts;
    turns;
    tools;
    last_excerpt = excerpt_of excerpt;
    running;
    resumable = true;
  }

let merge = fun ~limit groups ->
  let heads = ref [] in
  let rest = ref [] in
  List.for_each groups ~fn:(fun group ->
    match group with
    | [] -> ()
    | head :: tail ->
        heads := head :: !heads;
        rest := List.fold_left tail ~init:!rest ~fn:(fun acc item -> item :: acc));
  let heads = by_recent !heads in
  let rest = by_recent !rest in
  let n = List.length heads in
  let picked =
    if n >= limit then List.take heads ~len:limit
    else heads @ List.take rest ~len:(limit - n)
  in
  by_recent picked

let message_text = fun (msg : Json.t) ->
  match Json.get_field "content" msg with
  | None -> ""
  | Some (Json.String s) -> s
  | Some (Json.Array blocks) ->
      List.filter_map blocks ~fn:(fun b ->
        match Json.get_field "type" b |> Option.and_then ~fn:Json.get_string with
        | Some "text" ->
            Json.get_field "text" b |> Option.and_then ~fn:Json.get_string
        | Some "tool_use" ->
            Json.get_field "name" b
            |> Option.and_then ~fn:Json.get_string
            |> Option.map ~fn:(fun n -> "[tool:" ^ n ^ "]")
        | _ -> None)
      |> String.concat " "
  | _ -> ""

let summarize_jsonl = fun ~(project : string) ~(last_ts : float) ~(running : bool) (path : string) ->
  let tail = file_tail path ~n:131072 in
  let lines = String.split ~by:"\n" tail in
  let session_id = ref "" in
  let turns = ref 0 in
  let tools = ref 0 in
  let last_excerpt = ref "" in
  let last_cwd = ref "" in
  List.for_each lines ~fn:(fun line ->
    if not (String.is_empty line) then
      match Json.from_string line with
      | Error _ -> ()
      | Ok j ->
          let sid = json_str "sessionId" j in
          if not (String.is_empty sid) then session_id := sid;
          (match Json.get_field "cwd" j |> Option.and_then ~fn:Json.get_string with
           | Some c when not (String.is_empty c) -> last_cwd := c
           | _ -> ());
          let typ = json_str "type" j in
          if String.equal typ "user" || String.equal typ "assistant" then (
            turns := !turns + 1;
            (match Json.get_field "message" j with
             | None -> ()
             | Some msg ->
                 let text = message_text msg in
                 if String.contains text "[tool:" then tools := !tools + 1;
                 if not (String.is_empty (String.trim text)) then last_excerpt := text
            )
          ));
  let project' = if String.is_empty !last_cwd then project else !last_cwd in
  if !turns = 0 then None
  else
    Some
      (row
         ~agent:"claude"
         ~project:project'
         ~session_id:!session_id
         ~last_ts
         ~turns:!turns
         ~tools:!tools
         ~excerpt:!last_excerpt
         ~running)

let scan_claude = fun ~home ~now ->
  let root = home ^ "/.claude/projects" in
  let found = ref [] in
  List.for_each (dir_names root) ~fn:(fun dir_name ->
    let project_path = root ^ "/" ^ dir_name in
    let jsonls =
      List.filter (dir_names project_path) ~fn:(fun name ->
        String.ends_with ~suffix:".jsonl" name)
    in
    let newest =
      List.fold_left jsonls ~init:None ~fn:(fun best name ->
        let full = project_path ^ "/" ^ name in
        match mtime full with
        | None -> best
        | Some t -> (
            match best with
            | None -> Some (t, full)
            | Some (bt, _) -> if t > bt then Some (t, full) else best
          ))
    in
    match newest with
    | None -> ()
    | Some (t, full) -> (
        match summarize_jsonl ~project:dir_name ~last_ts:t ~running:(recently ~now t) full with
        | None -> ()
        | Some s -> found := s :: !found
      ));
  cap !found

let last_message = fun path ->
  let last = ref "" in
  List.for_each (String.split ~by:"\n" (file_tail path ~n:16384)) ~fn:(fun raw ->
    if not (String.is_empty raw) then
      match Json.from_string raw with
      | Error _ -> ()
      | Ok j ->
          let role = json_str "role" j in
          if String.equal role "user" || String.equal role "assistant" then
            match Json.get_field "message" j with
            | None -> ()
            | Some msg ->
                let text = String.trim (message_text msg) in
                if not (String.is_empty text) then last := text);
  !last

let workspace_path = fun project_dir ->
  match read_json (project_dir ^ "/.workspace-trusted") with
  | None -> ""
  | Some j ->
      let path = json_str "workspacePath" j in
      if String.starts_with ~prefix:"/" path then path else ""

(** Cursor IDE sessions live under projects/.../agent-transcripts. The
    older ~/.cursor/chats entries are the CLI and are included too. *)
let scan_cursor = fun ~home ~now ->
  let found = ref [] in
  let projects = home ^ "/.cursor/projects" in
  List.for_each (dir_names projects) ~fn:(fun proj ->
    let project_dir = projects ^ "/" ^ proj in
    let cwd = workspace_path project_dir in
    let transcripts = project_dir ^ "/agent-transcripts" in
    List.for_each (dir_names transcripts) ~fn:(fun sid ->
      let full = transcripts ^ "/" ^ sid ^ "/" ^ sid ^ ".jsonl" in
      if is_file full then
        match mtime full with
        | None -> ()
        | Some t ->
            let project = if String.is_empty cwd then proj else cwd in
            found :=
              row
                ~agent:"cursor"
                ~project
                ~session_id:sid
                ~last_ts:t
                ~turns:1
                ~tools:0
                ~excerpt:(last_message full)
                ~running:(recently ~now t)
              :: !found));
  let chats = home ^ "/.cursor/chats" in
  List.for_each (dir_names chats) ~fn:(fun hash ->
    let bucket = chats ^ "/" ^ hash in
    List.for_each (dir_names bucket) ~fn:(fun sid ->
      let meta = bucket ^ "/" ^ sid ^ "/meta.json" in
      match read_json meta with
      | None -> ()
      | Some j ->
          let cwd = json_str "cwd" j in
          let talking =
            Json.get_field "hasConversation" j
            |> Option.and_then ~fn:Json.get_bool
            |> Option.unwrap_or ~default:false
          in
          match mtime meta with
          | None -> ()
          | Some t ->
              if talking && not (String.is_empty cwd) then
                found :=
                  row
                    ~agent:"cursor"
                    ~project:cwd
                    ~session_id:sid
                    ~last_ts:t
                    ~turns:1
                    ~tools:0
                    ~excerpt:""
                    ~running:(recently ~now t)
                  :: !found));
  cap !found

let codex_names = fun home ->
  match read_text (home ^ "/.codex/session_index.jsonl") with
  | None -> []
  | Some body ->
      List.filter_map (String.split ~by:"\n" body) ~fn:(fun line ->
        if String.is_empty line then None
        else
          match Json.from_string line with
          | Error _ -> None
          | Ok j ->
              let id = json_str "id" j in
              if String.is_empty id then None else Some (id, json_str "thread_name" j))

let name_of = fun names id ->
  match List.find names ~fn:(fun (sid, _) -> String.equal sid id) with
  | Some (_, name) -> name
  | None -> ""

let codex_procs = fun home ->
  match read_json (home ^ "/.codex/process_manager/chat_processes.json") with
  | None -> []
  | Some (Json.Array items) -> items
  | Some _ -> []

let scan_codex = fun ~home ~now ->
  let names = codex_names home in
  let procs = codex_procs home in
  let found = ref [] in
  let root = home ^ "/.codex/sessions" in
  List.for_each (dir_names root) ~fn:(fun year ->
    let year_path = root ^ "/" ^ year in
    List.for_each (dir_names year_path) ~fn:(fun month ->
      let month_path = year_path ^ "/" ^ month in
      List.for_each (dir_names month_path) ~fn:(fun day ->
        let day_path = month_path ^ "/" ^ day in
        List.for_each (dir_names day_path) ~fn:(fun name ->
          if String.ends_with ~suffix:".jsonl" name then
            let full = day_path ^ "/" ^ name in
            let head = read_prefix full ~n:4096 in
            match quoted_after head "session_id" with
            | None -> ()
            | Some sid -> (
                match mtime full with
                | None -> ()
                | Some t ->
                    let cwd = quoted_after head "cwd" |> Option.unwrap_or ~default:"" in
                    let project = if String.is_empty cwd then name else cwd in
                    found :=
                      row
                        ~agent:"codex"
                        ~project
                        ~session_id:sid
                        ~last_ts:t
                        ~turns:0
                        ~tools:0
                        ~excerpt:(name_of names sid)
                        ~running:(recently ~now t)
                      :: !found
              )))));
  let seen = !found in
  List.for_each procs ~fn:(fun item ->
    let id = json_str "conversationId" item in
    let known = List.any seen ~fn:(fun s -> String.equal s.session_id id) in
    if (not (String.is_empty id)) && not known then
      let cwd = json_str "cwd" item in
      if not (String.is_empty cwd) then
        let ts =
          Json.get_field "updatedAtMs" item
          |> Option.and_then ~fn:millis
          |> Option.unwrap_or ~default:now
        in
        found :=
          row
            ~agent:"codex"
            ~project:cwd
            ~session_id:id
            ~last_ts:ts
            ~turns:0
            ~tools:0
            ~excerpt:(json_str "chatTitle" item)
            ~running:(recently ~now ts)
          :: !found);
  cap !found

(** A lock file is left behind after the task ends. Only a lock written
    in the last few minutes means the session is still going. *)
let dsh_locks = fun ~now home ->
  let root = home ^ "/.dsh/sessions" in
  let found = ref [] in
  List.for_each (dir_names root) ~fn:(fun enc ->
    let bucket = root ^ "/" ^ enc in
    List.for_each (dir_names bucket) ~fn:(fun sid ->
      let lock = bucket ^ "/" ^ sid ^ "/session.lock" in
      match mtime lock with
      | Some t when recently ~now t -> found := sid :: !found
      | _ -> ()));
  !found

let dsh_cwd = fun j ->
  Json.get_field "record" j
  |> Option.and_then ~fn:(fun r -> Json.get_field "identity" r)
  |> Option.and_then ~fn:(fun id -> Json.get_field "cwd" id)
  |> Option.and_then ~fn:Json.get_string
  |> Option.unwrap_or ~default:""

let dsh_title = fun j ->
  Json.get_field "record" j
  |> Option.and_then ~fn:(fun r -> Json.get_field "rows" r)
  |> Option.and_then ~fn:(fun rows -> Json.get_field "title" rows)
  |> Option.and_then ~fn:(fun title -> Json.get_field "val" title)
  |> Option.and_then ~fn:Json.get_string
  |> Option.unwrap_or ~default:""

let scan_dsh = fun ~home ~now ->
  let locks = dsh_locks ~now home in
  let root = home ^ "/.dsh/storages/session_projcache/sessions" in
  let found = ref [] in
  List.for_each (dir_names root) ~fn:(fun name ->
    if String.ends_with ~suffix:".json" name then
      let full = root ^ "/" ^ name in
      match read_json full with
      | None -> ()
      | Some j -> (
          match mtime full with
          | None -> ()
          | Some t ->
              let cwd = dsh_cwd j in
              if not (String.is_empty cwd) then
                let sid = strip_suffix name ~suffix:".json" in
                let locked = List.any locks ~fn:(fun id -> String.equal id sid) in
                found :=
                  row
                    ~agent:"deepseek"
                    ~project:cwd
                    ~session_id:sid
                    ~last_ts:t
                    ~turns:0
                    ~tools:0
                    ~excerpt:(dsh_title j)
                    ~running:(locked || recently ~now t)
                  :: !found
        ));
  cap !found

let reap_close = fun proc ->
  let rec go n =
    match Process.try_wait proc with
    | Ok (Some (Process.Exited _ | Process.Signaled _)) | Error _ -> ()
    | Ok None | Ok (Some (Process.Running | Process.Stopped _)) ->
        if n <= 0 then ()
        else (
          sleep (Time.Duration.from_millis 10);
          go (n - 1)
        )
  in
  go 200;
  let _ = Process.close proc in
  ()

let capture = fun ~program ~args ->
  let stdio =
    { Process.stdin = Process.Stdin.Null; stdout = Process.Stdout.Pipe; stderr = Process.Stderr.Null }
  in
  match Process.spawn ~program ~args ~stdio () with
  | Error _ -> None
  | Ok proc ->
      let deadline = now_s () +. 3.0 in
      let buf = ref "" in
      let raw = IO.Bytes.create ~size:8192 in
      let rec read_some () =
        match Process.stdout proc with
        | None -> false
        | Some file -> (
            match Fs.File.read file raw ~offset:0 ~len:8192 with
            | Ok n when n > 0 ->
                let chunk = IO.Bytes.to_string raw in
                let len = String.length chunk in
                let piece = if n < len then String.sub chunk ~offset:0 ~len:n else chunk in
                buf := !buf ^ piece;
                true
            | _ -> false
          )
      in
      let exited () =
        match Process.try_wait proc with
        | Ok (Some (Process.Exited _ | Process.Signaled _)) -> true
        | _ -> false
      in
      let rec loop () =
        if String.length !buf > 200000 || now_s () > deadline then
          let _ = Process.kill proc ~signal:9 in
          ()
        else if read_some () then loop ()
        else if exited () then
          let _ = read_some () in
          ()
        else (
          sleep (Time.Duration.from_millis 20);
          loop ()
        )
      in
      loop ();
      reap_close proc;
      Some !buf

let scan_zcode = fun ~home ~now ->
  let db = home ^ "/.zcode/v2/tasks-index.sqlite" in
  if not (is_file db) then []
  else
    let sql =
      "SELECT workspace_path, task_id, ifnull(title,'') AS title, ifnull(task_status,'') AS task_status, updated_at AS last_ms FROM tasks WHERE ifnull(deleted,0) = 0 ORDER BY updated_at DESC LIMIT 8"
    in
    match capture ~program:"/usr/bin/sqlite3" ~args:[| "-json"; db; sql |] with
    | None -> []
    | Some body -> (
        match Json.from_string (String.trim body) with
        | Error _ -> []
        | Ok (Json.Array items) ->
            cap
              (List.filter_map items ~fn:(fun item ->
                 let project = json_str "workspace_path" item in
                 let sid = json_str "task_id" item in
                 match Json.get_field "last_ms" item |> Option.and_then ~fn:millis with
                 | None -> None
                 | Some t ->
                     if String.is_empty project || String.is_empty sid then None
                     else
                       let status = json_str "task_status" item in
                       let live =
                         String.equal status "running" || String.equal status "in_progress"
                       in
                       Some
                         (row
                            ~agent:"zcode"
                            ~project
                            ~session_id:sid
                            ~last_ts:t
                            ~turns:0
                            ~tools:0
                            ~excerpt:(json_str "title" item)
                            ~running:(live || recently ~now t))
               ))
        | Ok _ -> []
      )

(** Current WorkBuddy sessions are a jsonl with no sibling meta file.
    cwd and the title sit on the event lines. Older sessions only have meta. *)
let workbuddy_jsonl = fun path ->
  let cwd = ref "" in
  let title = ref "" in
  let excerpt = ref "" in
  let take = fun raw ->
    if not (String.is_empty raw) then
      match Json.from_string raw with
      | Error _ -> ()
      | Ok j ->
          let dir = json_str "cwd" j in
          if not (String.is_empty dir) then cwd := dir;
          if String.equal (json_str "type" j) "ai-title" then
            let t = json_str "aiTitle" j in
            if not (String.is_empty t) then title := t;
          if String.equal (json_str "type" j) "message" then
            let role = json_str "role" j in
            if String.equal role "user" || String.equal role "assistant" then
              let text = String.trim (message_text j) in
              if not (String.is_empty text) then excerpt := text
  in
  List.for_each (String.split ~by:"\n" (read_prefix path ~n:16384)) ~fn:take;
  List.for_each (String.split ~by:"\n" (file_tail path ~n:16384)) ~fn:take;
  let label = if String.is_empty !title then !excerpt else !title in
  (!cwd, label)

let scan_workbuddy = fun ~home ~now ->
  let root = home ^ "/.workbuddy/projects" in
  let found = ref [] in
  let seen = ref [] in
  List.for_each (dir_names root) ~fn:(fun proj ->
    let bucket = root ^ "/" ^ proj in
    List.for_each (dir_names bucket) ~fn:(fun name ->
      if String.ends_with ~suffix:".jsonl" name then
        let full = bucket ^ "/" ^ name in
        let sid = strip_suffix name ~suffix:".jsonl" in
        match mtime full with
        | None -> ()
        | Some t ->
            let (cwd, excerpt) = workbuddy_jsonl full in
            let project = if String.is_empty cwd then proj else cwd in
            seen := sid :: !seen;
            found :=
              row
                ~agent:"workbuddy"
                ~project
                ~session_id:sid
                ~last_ts:t
                ~turns:0
                ~tools:0
                ~excerpt
                ~running:(recently ~now t)
              :: !found));
  List.for_each (dir_names root) ~fn:(fun proj ->
    let bucket = root ^ "/" ^ proj in
    List.for_each (dir_names bucket) ~fn:(fun name ->
      if String.ends_with ~suffix:".meta.json" name then
        let sid = strip_suffix name ~suffix:".meta.json" in
        let known = List.any !seen ~fn:(fun id -> String.equal id sid) in
        if not known then
          let full = bucket ^ "/" ^ name in
          match read_json full with
          | None -> ()
          | Some j -> (
              match mtime full with
              | None -> ()
              | Some t ->
                  let cwd = json_str "cwd" j in
                  if not (String.is_empty cwd) then
                    found :=
                      row
                        ~agent:"workbuddy"
                        ~project:cwd
                        ~session_id:sid
                        ~last_ts:t
                        ~turns:0
                        ~tools:0
                        ~excerpt:""
                        ~running:(recently ~now t)
                      :: !found
            )));
  cap !found

let scan_grok = fun ~home ~now ->
  let root = home ^ "/.grok/sessions" in
  let found = ref [] in
  List.for_each (dir_names root) ~fn:(fun encoded ->
    let project = decode_slashes encoded in
    let bucket = root ^ "/" ^ encoded in
    List.for_each (dir_names bucket) ~fn:(fun sid ->
      let summary = bucket ^ "/" ^ sid ^ "/summary.json" in
      if is_file summary then
        match mtime summary with
        | None -> ()
        | Some t ->
            let title =
              match read_json summary with
              | None -> ""
              | Some j -> json_str "generated_title" j
            in
            if not (String.is_empty project) then
              found :=
                row
                  ~agent:"grok"
                  ~project
                  ~session_id:sid
                  ~last_ts:t
                  ~turns:0
                  ~tools:0
                  ~excerpt:title
                  ~running:(recently ~now t)
                :: !found
    ));
  cap !found

type observation = {
  agent: string;
  project: string;
  session_id: string;
  running: bool;
  text: string;
  truncated: bool;
}

let observation_json = fun (o : observation) ->
  Json.obj
    [
      ("agent", Json.string (utf8_clean o.agent));
      ("project", Json.string (utf8_clean o.project));
      ("session_id", Json.string (utf8_clean o.session_id));
      ("running", Json.bool o.running);
      ("text", Json.string (utf8_clean o.text));
      ("truncated", Json.bool o.truncated);
    ]

let safe_id = fun s ->
  (not (String.is_empty s))
  && not (String.contains s "/")
  && not (String.contains s "\\")
  && not (String.contains s "'")
  && not (String.contains s "\"")
  && not (String.contains s " ")
  && not (String.contains s ";")
  && not (String.contains s "..")

let clip_text = fun text ->
  let text = String.trim text in
  let len = String.length text in
  if len <= 8000 then (text, false)
  else (String.sub text ~offset:(len - 8000) ~len:8000, true)

let texts_of = fun path ~line ->
  let bits = ref [] in
  List.for_each (String.split ~by:"\n" (file_tail path ~n:65536)) ~fn:(fun raw ->
    if not (String.is_empty raw) then
      match Json.from_string raw with
      | Error _ -> ()
      | Ok j -> (
          match line j with
          | None -> ()
          | Some t ->
              let t = String.trim t in
              if not (String.is_empty t) then bits := t :: !bits
        ));
  clip_text (String.concat "\n\n" (List.rev !bits))

let said = fun who text ->
  let text = String.trim text in
  if String.is_empty text then None else Some (who ^ "\n" ^ text)

let claude_line = fun j ->
  let typ = json_str "type" j in
  if String.equal typ "user" || String.equal typ "assistant" then
    match Json.get_field "message" j with
    | None -> None
    | Some msg ->
        said (if String.equal typ "user" then "你" else "Claude") (message_text msg)
  else None

let cursor_line = fun j ->
  let role = json_str "role" j in
  if String.equal role "user" || String.equal role "assistant" then
    match Json.get_field "message" j with
    | None -> None
    | Some msg -> said (if String.equal role "user" then "你" else "Cursor") (message_text msg)
  else None

let codex_line = fun j ->
  if not (String.equal (json_str "type" j) "response_item") then None
  else
    match Json.get_field "payload" j with
    | None -> None
    | Some payload ->
        let role = json_str "role" payload in
        if String.equal role "user" || String.equal role "assistant" then
          said (if String.equal role "user" then "你" else "Codex") (message_text payload)
        else None

let workbuddy_line = fun j ->
  let typ = json_str "type" j in
  if String.equal typ "custom-title" then
    let title = json_str "customTitle" j in
    if String.is_empty title then None else Some title
  else if String.equal typ "message" then
    let role = json_str "role" j in
    said (if String.equal role "user" then "你" else "WorkBuddy") (message_text j)
  else None

let grok_line = fun j ->
  let typ = json_str "type" j in
  if String.equal typ "user" || String.equal typ "assistant" then
    said (if String.equal typ "user" then "你" else "Grok") (message_text j)
  else None

let first_file = fun root name ->
  let found = ref None in
  List.for_each (dir_names root) ~fn:(fun dir ->
    match !found with
    | Some _ -> ()
    | None ->
        let full = root ^ "/" ^ dir ^ "/" ^ name in
        if is_file full then found := Some full);
  !found

let find_cursor = fun home sid ->
  let root = home ^ "/.cursor/projects" in
  let found = ref None in
  List.for_each (dir_names root) ~fn:(fun proj ->
    match !found with
    | Some _ -> ()
    | None ->
        let full = root ^ "/" ^ proj ^ "/agent-transcripts/" ^ sid ^ "/" ^ sid ^ ".jsonl" in
        if is_file full then found := Some full);
  !found

let find_codex = fun home sid ->
  let root = home ^ "/.codex/sessions" in
  let found = ref None in
  List.for_each (dir_names root) ~fn:(fun year ->
    let year_path = root ^ "/" ^ year in
    List.for_each (dir_names year_path) ~fn:(fun month ->
      let month_path = year_path ^ "/" ^ month in
      List.for_each (dir_names month_path) ~fn:(fun day ->
        let day_path = month_path ^ "/" ^ day in
        List.for_each (dir_names day_path) ~fn:(fun name ->
          if String.ends_with ~suffix:".jsonl" name then
            let full = day_path ^ "/" ^ name in
            match !found with
            | Some _ -> ()
            | None -> (
                match quoted_after (read_prefix full ~n:4096) "session_id" with
                | Some id when String.equal id sid -> found := Some full
                | _ -> ()
              )))));
  !found

let find_dsh = fun home sid ->
  let root = home ^ "/.dsh/sessions" in
  let found = ref None in
  List.for_each (dir_names root) ~fn:(fun enc ->
    match !found with
    | Some _ -> ()
    | None ->
        let full = root ^ "/" ^ enc ^ "/" ^ sid ^ "/session.v4.jsonl.zstd" in
        if is_file full then found := Some full);
  !found

let find_grok = fun home sid ->
  let root = home ^ "/.grok/sessions" in
  let found = ref None in
  List.for_each (dir_names root) ~fn:(fun enc ->
    match !found with
    | Some _ -> ()
    | None ->
        let chat = root ^ "/" ^ enc ^ "/" ^ sid ^ "/chat_history.jsonl" in
        let summary = root ^ "/" ^ enc ^ "/" ^ sid ^ "/summary.json" in
        if is_file chat then found := Some chat
        else if is_file summary then found := Some summary);
  !found

let capture_tail = fun ~program ~args ->
  let stdio =
    { Process.stdin = Process.Stdin.Null; stdout = Process.Stdout.Pipe; stderr = Process.Stderr.Null }
  in
  match Process.spawn ~program ~args ~stdio () with
  | Error _ -> None
  | Ok proc ->
      let deadline = now_s () +. 8.0 in
      let buf = ref "" in
      let raw = IO.Bytes.create ~size:8192 in
      let rec read_some () =
        match Process.stdout proc with
        | None -> false
        | Some file -> (
            match Fs.File.read file raw ~offset:0 ~len:8192 with
            | Ok n when n > 0 ->
                let chunk = IO.Bytes.to_string raw in
                let len = String.length chunk in
                let piece = if n < len then String.sub chunk ~offset:0 ~len:n else chunk in
                buf := !buf ^ piece;
                let kept = String.length !buf in
                if kept > 180000 then buf := String.sub !buf ~offset:(kept - 160000) ~len:160000;
                true
            | _ -> false
          )
      in
      let exited () =
        match Process.try_wait proc with
        | Ok (Some (Process.Exited _ | Process.Signaled _)) -> true
        | _ -> false
      in
      let rec loop () =
        if now_s () > deadline then
          let _ = Process.kill proc ~signal:9 in
          ()
        else if read_some () then loop ()
        else if exited () then
          let _ = read_some () in
          ()
        else (
          sleep (Time.Duration.from_millis 20);
          loop ()
        )
      in
      loop ();
      reap_close proc;
      Some !buf

let zstd_text = fun path ->
  let rec go = fun rest ->
    match rest with
    | [] -> None
    | program :: tail ->
        if is_file program then capture_tail ~program ~args:[| "-dc"; "--quiet"; path |]
        else go tail
  in
  go [ "/opt/homebrew/bin/zstd"; "/usr/local/bin/zstd"; "/usr/bin/zstd" ]

(** The last turn in a DeepSeek transcript has finished, so the desktop is
    waiting for the next message. A file written a moment ago is not enough
    to call that running. *)
let dsh_turn_closed = fun body ->
  let state = ref None in
  List.for_each (String.split ~by:"\n" body) ~fn:(fun raw ->
    if not (String.is_empty raw) then
      match Json.from_string raw with
      | Error _ -> ()
      | Ok j ->
          let typ = json_str "type" j in
          if String.equal typ "turn/start" then state := Some false
          else if String.equal typ "turn/end" then state := Some true);
  match !state with Some closed -> closed | None -> false

let dsh_closed_cache = ref ("", 0.0, false)

let dsh_is_waiting = fun home sid ->
  match find_dsh home sid with
  | None -> false
  | Some path -> (
      match mtime path with
      | None -> false
      | Some t ->
          let (cached, mt, closed) = !dsh_closed_cache in
          if String.equal cached path && mt = t then closed
          else
            let closed =
              match zstd_text path with None -> false | Some body -> dsh_turn_closed body
            in
            dsh_closed_cache := (path, t, closed);
            closed
    )

(** A desktop session records an agent preset. The headless runner rejects
    every preset, so Oasis cannot deliver a reply into that session. *)
let dsh_preset_locked = fun line ->
  match Json.from_string line with
  | Error _ -> false
  | Ok j -> not (String.is_empty (json_str "agentPreset" j))

let zstd_programs = [ "/opt/homebrew/bin/zstd"; "/usr/local/bin/zstd"; "/usr/bin/zstd" ]

let zstd_head = fun path ->
  let rec go = fun rest ->
    match rest with
    | [] -> None
    | program :: tail -> if is_file program then capture ~program ~args:[| "-dc"; "--quiet"; path |] else go tail
  in
  go zstd_programs

let dsh_preset_cache = ref ("", 0.0, false)

let dsh_desktop_session = fun path ->
  match mtime path with
  | None -> false
  | Some t ->
      let (cached, mt, locked) = !dsh_preset_cache in
      if String.equal cached path && mt = t then locked
      else
        let locked =
          match zstd_head path with
          | None -> false
          | Some body ->
              let line = match String.split ~by:"\n" body with first :: _ -> first | [] -> "" in
              dsh_preset_locked line
        in
        dsh_preset_cache := (path, t, locked);
        locked

(** A fresh cache file still looks active for a few minutes after the turn
    ends. A live lock wins; otherwise a closed transcript can take a reply.
    A desktop preset cannot be resumed from here. *)
let settle_dsh = fun ~home ~now (rows : session_summary list) ->
  let locks = dsh_locks ~now home in
  List.map rows ~fn:(fun s ->
    let locked = List.any locks ~fn:(fun id -> String.equal id s.session_id) in
    let desktop =
      match find_dsh home s.session_id with None -> false | Some path -> dsh_desktop_session path
    in
    let s = if desktop then { s with resumable = false } else s in
    if (not locked) && s.running && dsh_is_waiting home s.session_id then { s with running = false } else s)

(** Newest sessions across the local agents. Each agent keeps a seat even
    when another agent has more recent files. *)
let scan = fun ~home ~limit ->
  let now = now_s () in
  merge ~limit
    [
      scan_claude ~home ~now;
      scan_cursor ~home ~now;
      scan_codex ~home ~now;
      settle_dsh ~home ~now (scan_dsh ~home ~now);
      scan_zcode ~home ~now;
      scan_workbuddy ~home ~now;
      scan_grok ~home ~now;
    ]

let dsh_bits = fun body ->
  let bits = ref [] in
  List.for_each (String.split ~by:"\n" body) ~fn:(fun raw ->
    if not (String.is_empty raw) then
      match Json.from_string raw with
      | Error _ -> ()
      | Ok j ->
          let typ = json_str "type" j in
          let piece =
            if String.equal typ "user/message" then
              match Json.get_field "data" j with
              | None -> None
              | Some data -> said "你" (message_text data)
            else if String.equal typ "assistant/message" then
              match Json.get_field "data" j |> Option.and_then ~fn:(Json.get_field "message") with
              | None -> None
              | Some msg -> said "DeepSeek" (message_text msg)
            else if String.equal typ "session/title" then
              match Json.get_field "data" j with
              | None -> None
              | Some data ->
                  let t = json_str "title" data in
                  if String.is_empty (String.trim t) then None else Some t
            else None
          in
          match piece with
          | None -> ()
          | Some t -> bits := t :: !bits);
  clip_text (String.concat "\n\n" (List.rev !bits))

let noted = fun ~agent ~project ~session_id ~running text ->
  let (text, truncated) = clip_text text in
  { agent; project; session_id; running; text; truncated }

(** Read one local session for the workbench. Tool arguments, request
    headers, and auth files are skipped. *)
let observe = fun ~home ~agent ~session_id ->
  if not (safe_id session_id) then None
  else
    let known =
      List.find (scan ~home ~limit:60) ~fn:(fun s ->
        String.equal s.agent agent && String.equal s.session_id session_id)
    in
    let project = match known with Some s -> s.project | None -> "" in
    let running = match known with Some s -> s.running | None -> false in
    match agent with
    | "claude" -> (
        match first_file (home ^ "/.claude/projects") (session_id ^ ".jsonl") with
        | None -> None
        | Some path ->
            let (text, truncated) = texts_of path ~line:claude_line in
            Some { agent; project; session_id; running; text; truncated }
      )
    | "cursor" -> (
        match find_cursor home session_id with
        | None -> None
        | Some path ->
            let (text, truncated) = texts_of path ~line:cursor_line in
            Some { agent; project; session_id; running; text; truncated }
      )
    | "codex" -> (
        match find_codex home session_id with
        | None -> None
        | Some path ->
            let (text, truncated) = texts_of path ~line:codex_line in
            Some { agent; project; session_id; running; text; truncated }
      )
    | "workbuddy" -> (
        match first_file (home ^ "/.workbuddy/projects") (session_id ^ ".jsonl") with
        | None -> None
        | Some path ->
            let (text, truncated) = texts_of path ~line:workbuddy_line in
            Some { agent; project; session_id; running; text; truncated }
      )
    | "grok" -> (
        match find_grok home session_id with
        | None -> None
        | Some path ->
            if String.ends_with ~suffix:".json" path then
              match read_json path with
              | None -> None
              | Some j ->
                  Some (noted ~agent ~project ~session_id ~running (json_str "generated_title" j ^ "\n" ^ json_str "session_summary" j))
            else
              let (text, truncated) = texts_of path ~line:grok_line in
              Some { agent; project; session_id; running; text; truncated }
      )
    | "deepseek" -> (
        let cache = home ^ "/.dsh/storages/session_projcache/sessions/" ^ session_id ^ ".json" in
        let cached =
          match read_json cache with
          | None -> ("", "")
          | Some j -> (dsh_cwd j, dsh_title j)
        in
        let (cwd, title) = cached in
        let project = if String.is_empty project then cwd else project in
        match find_dsh home session_id with
        | None ->
            if String.is_empty project && String.is_empty title then None
            else Some (noted ~agent ~project ~session_id ~running title)
        | Some path -> (
            match zstd_text path with
            | None -> Some (noted ~agent ~project ~session_id ~running title)
            | Some body ->
                let (text, truncated) = dsh_bits body in
                let text = if String.is_empty text then title else text in
                Some { agent; project; session_id; running; text; truncated }
          )
      )
    | "zcode" -> (
        let db = home ^ "/.zcode/v2/tasks-index.sqlite" in
        if not (is_file db) then None
        else
          let sql =
            "SELECT workspace_path, ifnull(title,'') AS title, ifnull(task_status,'') AS task_status FROM tasks WHERE task_id = '"
            ^ session_id ^ "' LIMIT 1"
          in
          match capture ~program:"/usr/bin/sqlite3" ~args:[| "-json"; db; sql |] with
          | None -> None
          | Some body -> (
              match Json.from_string (String.trim body) with
              | Ok (Json.Array (item :: _)) ->
                  let project' = json_str "workspace_path" item in
                  let project = if String.is_empty project then project' else project in
                  let status = json_str "task_status" item in
                  let running =
                    running || String.equal status "running" || String.equal status "in_progress"
                  in
                  Some (noted ~agent ~project ~session_id ~running (json_str "title" item))
              | _ -> None
            )
      )
    | _ -> None
