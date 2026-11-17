#!/bin/sh
# Start / stop the Oasis gateway in the background.
#   scripts/oasis.sh start|stop|restart|status|logs
#   scripts/oasis.sh <anything else>   runs the oasis binary with those args
# Runtime files live in $OASIS_HOME (default ~/.oasis): oasis.toml, data/,
# workspaces/, logs/oasis.log, oasis.pid. API keys come from the calling
# shell's environment and from export lines in the user's shell rc files.
set -e

OASIS_HOME="${OASIS_HOME:-$HOME/.oasis}"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
PID_FILE="$OASIS_HOME/oasis.pid"
LOG="$OASIS_HOME/logs/oasis.log"
PORT="${OASIS_PORT:-9899}"

bin() {
  if [ -n "$OASIS_BIN" ]; then echo "$OASIS_BIN"; return; fi
  ls -t "$ROOT"/_build/*/*/out/oasis/oasis 2>/dev/null | head -1
}

# The pid of whatever actually listens on the gateway port, if any.
# The pidfile alone is not trustworthy: a failed start can leave it stale
# or pointing at the wrong pid, and then stop/status miss the real gateway
# while start spawns a duplicate that dies on "address already in use".
listener_pid() {
  lsof -tiTCP:"$PORT" -sTCP:LISTEN 2>/dev/null | head -1
}

current_pid() {
  lp="$(listener_pid)"
  if [ -n "$lp" ]; then echo "$lp"; return; fi
  [ -f "$PID_FILE" ] && cat "$PID_FILE"
}

running() {
  pid="$(current_pid)" && [ -n "$pid" ] && kill -0 "$pid" 2>/dev/null
}

start() {
  if running; then echo "Oasis 已在运行 (pid $(current_pid))"; return; fi
  lp="$(listener_pid)"
  if [ -n "$lp" ]; then
    echo "端口 $PORT 已被 pid $lp 占用，但它不是本脚本管理的 oasis；先处理它再启动" >&2
    exit 1
  fi
  BIN="$(bin)"
  if [ -z "$BIN" ]; then echo "找不到 oasis 可执行文件，先运行 riot build" >&2; exit 1; fi
  mkdir -p "$OASIS_HOME/logs"
  # own process group, detached from this terminal
  nohup /usr/bin/perl -e 'setpgrp(0,0); exec @ARGV' -- "$BIN" "$@" >> "$LOG" 2>&1 < /dev/null &
  echo $! > "$PID_FILE"
  sleep 1
  if running; then echo "Oasis 已启动 (pid $(current_pid))，日志 $LOG"; else echo "启动失败，见 $LOG" >&2; tail -20 "$LOG" >&2; rm -f "$PID_FILE"; exit 1; fi
}

stop() {
  if ! running; then echo "Oasis 没有在运行"; rm -f "$PID_FILE"; return; fi
  pid="$(current_pid)"
  # || true: a race where the process dies first must not abort the script
  # under set -e and leave a stale pidfile behind.
  kill "$pid" || true
  for _ in 1 2 3 4 5 6 7 8 9 10; do kill -0 "$pid" 2>/dev/null || break; sleep 0.3; done
  kill -0 "$pid" 2>/dev/null && kill -9 "$pid" || true
  rm -f "$PID_FILE"
  echo "Oasis 已停止"
}

cmd="${1:-status}"
[ $# -gt 0 ] && shift
case "$cmd" in
  start) start "$@" ;;
  stop) stop ;;
  restart) stop; start "$@" ;;
  status) if running; then echo "运行中 (pid $(current_pid))"; else echo "未运行"; fi ;;
  logs) tail -f "$LOG" ;;
  help|-h|--help) echo "用法: $0 start|stop|restart|status|logs，其余参数原样交给 oasis（如 --self-test、ask \"你好\"、models）" ;;
  *)
    BIN="$(bin)"
    if [ -z "$BIN" ]; then echo "找不到 oasis 可执行文件，先运行 riot build" >&2; exit 1; fi
    exec "$BIN" "$cmd" "$@" ;;
esac
