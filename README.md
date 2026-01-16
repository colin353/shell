http://google.com

# Shell + multiplexer

Things to do:

 - [x] CTRL+P for fuzzy file selection
 - [ ] Change the directory name in the prompt to a tasteful color
 - [ ] Tilde is not expanded into $HOME, env vars are not expanded during completions/fuzzy finding
 - [ ] Environment variable configuration
 - [ ] Pressing CTRL+C immediately kills a process but should actually send sigterm and wait instead
 - [ ] Seems to be a bug with losing track of cursor highlighting occasionally (with copilot-cli)
 - [ ] Seems to be possible to desync the terminal state with fuzzy finder occasionally (hard to reproduce)
 - [ ] The history search should strongly prioritize commands which have been run many times and commands which have been run recently (right now, shorter commands are preferred by the ranking)
 - [ ] Remote connection plan/design
 - [ ] Integrated agent?

Stuff that was already done:

 - [x] Resizing the terminal seems to cause a lot of weird behaviour
 - [x] Opening a long-ish code file in vim doesn't render correctly
 - [x] I think delta rendering does not accurately track the attribute state of the current cursor. Sometimes, regions are re-rendered with the wrong attributes.
 - [x] When working on the project for a while, I end up running into ENXIO a lot due to resource exhaustion. I'm not cleaning up ptys or something (Fixed: PTY Drop now properly reaps zombie processes with waitpid)
 - [x] Certain tests are unreliable/flaky
 - [x] Should render grid boundaries around the pane cells
 - [x] Actually implement the shell part
 - [x] Synchronized output support in delta rendering
 - [x] Implement multi-screen support and a status bar
 - [x] When in search mode, after pressing enter, freeze the search input, and let `n` and `p` advance the search position (rather than modifying the input).
 - [x] Make URL search incremental (if one is found, stop searching, and only continue searching when seeking the next one)
 - [x] If a program prints something but it doesn't end in a newline, print a % character with a white background, then emit a newline.