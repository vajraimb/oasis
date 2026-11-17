open Std

(** Shared child-process bookkeeping: spawn, wait, kill, reap.

    Why this exists (zombie leak, 2026-11):
    - [Process.close] only closes pipes; it never [waitpid]s.
    - [Process.try_wait] keeps the exit status in a plain mutable field and
      calls [waitpid] whenever that field still says running. Two lanes
      polling the same child race: one collects it, the other gets ECHILD,
      and a lane that has not yet seen the cached status may [kill] a pid
      that has already been reaped (and possibly reused).
    - The owner of a child can stall forever before its cleanup runs: an
      actor parked in [sleep] whose wakeup is lost, or one that is exited
      without unwinding. Its [protect ~finally] never runs, the child is
      never waited for, and it stays a zombie for the life of the gateway.
    - The old [reap_close] also gave up for good after its budget.

    So every child is tracked here. All [try_wait]/[kill] calls on a tracked
    child go through one lock, so only one lane ever [waitpid]s at a time and
    a kill never targets a child already collected. A dedicated system
    thread (a domain sleeping in [nanosleep], independent of the actor
    scheduler) polls the tracked children every couple of seconds with
    [WNOHANG] and collects any that have exited, whatever their owner is
    doing. It only waits on pids it tracked, never [waitpid(-1)], so it does
    not steal a status another caller needs: the status is cached in the
    same [Process.t] the owner holds. *)

let pause_ms = fun ms ->
  Kernel.Thread.sleep_ns (Time.Duration.to_nanos (Time.Duration.from_millis ms))

type entry = {
  proc: Process.t;
  mutable errors: int;
}

let lock = Kernel.Sync.Mutex.create ()

let entries = ref ([] : entry list)

let with_lock = fun f ->
  Kernel.Sync.Mutex.lock lock;
  match f () with
  | value ->
      Kernel.Sync.Mutex.unlock lock;
      value
  | exception exn ->
      Kernel.Sync.Mutex.unlock lock;
      raise exn

let is_done = fun r ->
  match r with
  | Ok (Some (Process.Exited _ | Process.Signaled _)) -> true
  | _ -> false

(** Collect every tracked child that has exited. Children whose waitpid
    keeps failing (ECHILD: collected by code that bypassed this module)
    are dropped after a few tries. Caller holds [lock]. *)
let sweep_locked = fun () ->
  entries :=
    List.filter !entries ~fn:(fun e ->
      match Process.try_wait e.proc with
      | Ok (Some (Process.Exited _ | Process.Signaled _)) -> false
      | Ok _ ->
          e.errors <- 0;
          true
      | Error _ ->
          e.errors <- e.errors + 1;
          e.errors < 5)

let sweep = fun () -> with_lock sweep_locked

(** Number of children still tracked (alive, or exited and not yet swept). *)
let tracked = fun () -> with_lock (fun () -> List.length !entries)

let sweep_every_ms = 2000

let reaper_started = Sync.Atomic.make false

let start_reaper = fun () ->
  if Sync.Atomic.compare_and_set reaper_started false true then
    let _ =
      Kernel.Thread.spawn (fun () ->
        let rec loop () =
          pause_ms sweep_every_ms;
          (try sweep () with _ -> ());
          loop ()
        in
        loop ())
    in
    ()

let track = fun (proc : Process.t) ->
  with_lock (fun () -> entries := { proc; errors = 0 } :: !entries);
  start_reaper ()

(** [Process.spawn], with the child tracked for reaping. *)
let spawn = fun ~program ~args ?env ?current_dir ~stdio () ->
  match Process.spawn ~program ~args ?env ?current_dir ~stdio () with
  | Ok proc ->
      track proc;
      Ok proc
  | Error e -> Error e

(** [Process.try_wait] under the shared lock: the only lane calling
    [waitpid] for this child right now, and the status cached by whichever
    lane collected it is visible here. *)
let try_wait = fun (proc : Process.t) -> with_lock (fun () -> Process.try_wait proc)

let exited = fun proc -> is_done (try_wait proc)

(** Signal [proc] unless it has already been collected; never signals a pid
    that may have been reused. *)
let kill = fun (proc : Process.t) ~signal ->
  with_lock (fun () ->
    if is_done (Process.try_wait proc) then Ok ()
    else Process.kill proc ~signal)

(** SIGKILL [proc]'s whole tree. [tree_kill] receives the leader pid and must
    kill the group and every descendant. The exit check, the tree kill and
    the collection all run under the shared lock: without that, the child
    could be reaped here between the check and [tree_kill], its pid reused
    by an unrelated process, and the tree walk would kill the wrong tree.
    [tree_kill] must not call back into this module (the mutex is not
    recursive); wait for any helper it spawns with raw [Process] calls. *)
let kill_tree = fun (proc : Process.t) ~tree_kill ->
  with_lock (fun () ->
    if is_done (Process.try_wait proc) then ()
    else (
      tree_kill (Process.pid proc);
      let _ = Process.kill proc ~signal:9 in
      let _ = Process.try_wait proc in
      ()))

let rec wait_loop = fun proc n ->
  if exited proc then true
  else if n <= 0 then false
  else (
    pause_ms 10;
    wait_loop proc (n - 1)
  )

(** Wait briefly for a voluntary exit, then SIGKILL and wait again. Safe to
    call from several lanes at once. Never closes pipes. A child not
    collected within the budget stays tracked; the background reaper
    collects it once it exits. Returns whether it was collected here. *)
let reap = fun ?(grace_n = 50) ?(after_kill_n = 500) (proc : Process.t) ->
  if wait_loop proc grace_n then true
  else (
    let _ = kill proc ~signal:9 in
    wait_loop proc after_kill_n
  )

(** [reap], then close the pipes. Only the lane that owns the child (and
    reads its pipes) may call this, exactly once. Other lanes use [reap]. *)
let reap_close = fun ?grace_n ?after_kill_n (proc : Process.t) ->
  let finished = reap ?grace_n ?after_kill_n proc in
  let _ = Process.close proc in
  finished

let protect = fun ~finally f ->
  match f () with
  | value ->
      finally ();
      value
  | exception exn ->
      finally ();
      raise exn
