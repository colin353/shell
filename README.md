# Shell + multiplexer

Some bugs:

 - [x] Resizing the terminal seems to cause a lot of weird behaviour
 - [x] Opening a long-ish code file in vim doesn't render correctly
 - [x] I think delta rendering does not accurately track the attribute state of the current cursor. Sometimes, regions are re-rendered with the wrong attributes.
 - [x] When working on the project for a while, I end up running into ENXIO a lot due to resource exhaustion. I'm not cleaning up ptys or something (Fixed: PTY Drop now properly reaps zombie processes with waitpid)
 - [ ] Certain tests are unreliable/flaky

Some features to build:
 - [x] Should render grid boundaries around the pane cells
 - [ ] Actually implement the shell part
 - [x] Synchronized output support in delta rendering
 - [ ] Implement multi-screen support and a status bar