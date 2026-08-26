http://google.com

# Shell + multiplexer

## Script control

Foreground scripts can request changes to their containing shell through the
authenticated `shellctl` helper:

```bash
cd "$(git rev-parse --show-toplevel)" || exit
shellctl set-cwd "$PWD"
shellctl rename-window "$(basename "$PWD")"
```

The equivalent long form is `shell control ...`. Requests are written directly
to the controlling terminal, are scoped to the current foreground process, and
work in connected panes; unknown or copied control sequences are ignored.

Things to do:

 - [x] When copying text from selection mode, we should automatically exit selection mode when the copy completes
 - [x] Completions can get messed up when you start completing and then backspace and then finish completing (results in extra appended stuff after completion)
 - [x] When connected to a remote, when creating a split, we should set CWD to the CWD within the remote split. Instead, we end up on $HOME in the remote usually
 - [x] When doing a reconnect, we should set the pane title to the name of the reconnect pane
 - [x] Can we do predictive typeahead for remote connections? Maybe when we detect that the terminal is in an echo mode?
 - [ ] When creating a new pane, initially prompt to name it? Escape --> give it a default name?
 - [x] When exiting a remote connection, kill the pane. Right now, when exiting, we exit back to the non-remote shell, so to kill a remote pane, we have to do CTRL+D multiple times.
