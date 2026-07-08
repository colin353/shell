http://google.com

# Shell + multiplexer

Things to do:

 - [ ] When copying text from selection mode, we should automatically exit selection mode when the copy completes
 - [ ] Completions can get messed up when you start completing and then backspace and then finish completing (results in extra appended stuff after completion)
 - [ ] When connected to a remote, when creating a split, we should set CWD to the CWD within the remote split. Instead, we end up on $HOME in the remote usually
 - [ ] When doing a reconnect, we should set the pane title to the name of the reconnect pane
 - [ ] Can we do predictive typeahead for remote connections? Maybe when we detect that the terminal is in an echo mode?
 - [ ] When creating a new pane, initially prompt to name it? Escape --> give it a default name?
