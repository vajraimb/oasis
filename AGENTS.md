# Oasis

Local gateway: one OpenAI-compatible endpoint for the API accounts and coding-agent CLIs already on this machine. State stays in `~/.oasis`. Spec: `docs/oasis-spec.md`.

Reply in the user's language.

## Installing for someone

If the task is to install Oasis and open the console, follow `docs/agent-setup.md` from the top. Do not clone or install until that file says so. Stop when it says the console is open. The user ticks accounts and presses Save.

## This checkout

Build and test from the repository root. `riot` is `~/.riot/bin/riot`.

```bash
export PATH="$HOME/.riot/bin:$PATH"
riot toolchain install
riot plan > /dev/null && scripts/patch-riot.sh
riot build
riot test -p oasis -- --concurrency 1
scripts/oasis.sh --self-test
scripts/oasis.sh start
```

Console: `http://127.0.0.1:9899/`. First start probes keys and agents, then writes `~/.oasis/oasis.toml` only when that file does not exist. If a toml is already there, leave it. Do not delete one that already lists accounts.

Run gateway tests serially. Parallel `riot test` can crash the runtime.

## Config and secrets

- `~/.oasis/oasis.toml` is config. Keys do not go in it.
- Keys live in the process environment, `export` lines in the shell rc files, agent config files, then `~/.oasis/secrets.env` (mode `0600`), which wins.
- Do not print key values, tokens, or `secrets.env`. Do not commit them.
- Child agents are started with a clean environment. They do not receive provider API keys.
- Loopback needs no bearer token. A public bind requires `auth_token_env`. Do not put that token in a shell rc file.

## Calling it

`POST /v1/chat/completions`. Omit `model`, or send `auto`, to let Oasis route. Pin an agent with `X-Oasis-Mode: agent` and `X-Oasis-Agent: <name>`.

## Editing this code

Riot here has no `<>` operator. Write `not (a = b)`. Do not trust `&&` to guard an index; branch before any unchecked read. `String.contains` is a substring search.
