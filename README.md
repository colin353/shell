http://google.com

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
 - [x] Implement multi-screen support and a status bar
 - [x] When in search mode, after pressing enter, freeze the search input, and let `n` and `p` advance the search position (rather than modifying the input).
 - [x] Make URL search incremental (if one is found, stop searching, and only continue searching when seeking the next one)
 - [x] If a program prints something but it doesn't end in a newline, print a % character with a white background, then emit a newline.