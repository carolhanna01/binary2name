/*
 * Automatically generated file: DO NOT EDIT!
 * Zile command to C function bindings and docstrings.
 * Generated from C sources.
 */

X("beginning-of-line", beginning_of_line, true, "\
Move point to beginning of current line.\n\
")
X("end-of-line", end_of_line, true, "\
Move point to end of current line.\n\
")
X("previous-line", previous_line, true, "\
Move cursor vertically up one line.\n\
If there is no character in the target line exactly over the current column,\n\
the cursor is positioned after the character in that line which spans this\n\
column, or at the end of the line if it is not long enough.\n\
")
X("next-line", next_line, true, "\
Move cursor vertically down one line.\n\
If there is no character in the target line exactly under the current column,\n\
the cursor is positioned after the character in that line which spans this\n\
column, or at the end of the line if it is not long enough.\n\
")
X("goto-char", goto_char, true, "\
Set point to POSITION, a number.\n\
Beginning of buffer is position 1.\n\
")
X("goto-line", goto_line, true, "\
Goto LINE, counting from line 1 at beginning of buffer.\n\
")
X("beginning-of-buffer", beginning_of_buffer, true, "\
Move point to the beginning of the buffer; leave mark at previous position.\n\
")
X("end-of-buffer", end_of_buffer, true, "\
Move point to the end of the buffer; leave mark at previous position.\n\
")
X("backward-char", backward_char, true, "\
Move point left N characters (right if N is negative).\n\
On attempt to pass beginning or end of buffer, stop and signal error.\n\
")
X("forward-char", forward_char, true, "\
Move point right N characters (left if N is negative).\n\
On reaching end of buffer, stop and signal error.\n\
")
X("scroll-down", scroll_down, true, "\
Scroll text of current window downward near full screen.\n\
")
X("scroll-up", scroll_up, true, "\
Scroll text of current window upward near full screen.\n\
")
X("self-insert-command", self_insert_command, true, "\
Insert the character you type.\n\
Whichever character you type to run this command is inserted.\n\
")
X("global-set-key", global_set_key, true, "\
Bind a command to a key sequence.\n\
Read key sequence and function name, and bind the function to the key\n\
sequence.\n\
")
X("where-is", where_is, true, "\
Print message listing key sequences that invoke the command DEFINITION.\n\
Argument is a command name.\n\
")
X("describe-bindings", describe_bindings, true, "\
Show a list of all defined keys, and their definitions.\n\
")
X("kill-buffer", kill_buffer, true, "\
Kill buffer BUFFER.\n\
With a nil argument, kill the current buffer.\n\
")
X("setq", setq, false, "\
(setq [sym val]...)\n\
\n\
Set each sym to the value of its val.\n\
The symbols sym are variables; they are literal (not evaluated).\n\
The values val are expressions; they are evaluated.\n\
")
X("execute-extended-command", execute_extended_command, true, "\
Read function name, then read its arguments and call it.\n\
")
X("find-file", find_file, true, "\
Edit file FILENAME.\n\
Switch to a buffer visiting file FILENAME,\n\
creating one if none already exists.\n\
")
X("find-file-read-only", find_file_read_only, true, "\
Edit file FILENAME but don't allow changes.\n\
Like `find-file' but marks buffer as read-only.\n\
Use M-x toggle-read-only to permit editing.\n\
")
X("find-alternate-file", find_alternate_file, true, "\
Find the file specified by the user, select its buffer, kill previous buffer.\n\
If the current buffer now contains an empty file that you just visited\n\
(presumably by mistake), use this command to visit the file you really want.\n\
")
X("switch-to-buffer", switch_to_buffer, true, "\
Select buffer BUFFER in the current window.\n\
")
X("insert-buffer", insert_buffer, true, "\
Insert after point the contents of BUFFER.\n\
Puts mark after the inserted text.\n\
")
X("insert-file", insert_file, true, "\
Insert contents of file FILENAME into buffer after point.\n\
Set mark after the inserted text.\n\
")
X("save-buffer", save_buffer, true, "\
Save current buffer in visited file if modified.  By default, makes the\n\
previous version into a backup file if this is the first save.\n\
")
X("write-file", write_file, true, "\
Write current buffer into file FILENAME.\n\
This makes the buffer visit that file, and marks it as not modified.\n\
\n\
Interactively, confirmation is required unless you supply a prefix argument.\n\
")
X("save-some-buffers", save_some_buffers, true, "\
Save some modified file-visiting buffers.  Asks user about each one.\n\
")
X("save-buffers-kill-emacs", save_buffers_kill_emacs, true, "\
Offer to save each buffer, then kill this Zile process.\n\
")
X("cd", cd, true, "\
Make DIR become the current buffer's default directory.\n\
")
X("suspend-emacs", suspend_emacs, true, "\
Stop Zile and return to superior process.\n\
")
X("keyboard-quit", keyboard_quit, true, "\
Cancel current command.\n\
")
X("list-buffers", list_buffers, true, "\
Display a list of names of existing buffers.\n\
The list is displayed in a buffer named `*Buffer List*'.\n\
Note that buffers with names starting with spaces are omitted.\n\
\n\
@itemize -\n\
The M column contains a * for buffers that are modified.\n\
The R column contains a % for buffers that are read-only.\n\
@end itemize\n\
")
X("toggle-read-only", toggle_read_only, true, "\
Change whether this buffer is visiting its file read-only.\n\
")
X("auto-fill-mode", auto_fill_mode, true, "\
Toggle Auto Fill mode.\n\
In Auto Fill mode, inserting a space at a column beyond `fill-column'\n\
automatically breaks the line at a previous space.\n\
")
X("set-fill-column", set_fill_column, true, "\
Set `fill-column' to specified argument.\n\
Use C-u followed by a number to specify a column.\n\
Just C-u as argument means to use the current column.\n\
")
X("set-mark", set_mark, false, "\
Set this buffer's mark to point.\n\
")
X("set-mark-command", set_mark_command, true, "\
Set the mark where point is.\n\
")
X("exchange-point-and-mark", exchange_point_and_mark, true, "\
Put the mark where point is now, and point where the mark is now.\n\
")
X("mark-whole-buffer", mark_whole_buffer, true, "\
Put point at beginning and mark at end of buffer.\n\
")
X("quoted-insert", quoted_insert, true, "\
Read next input character and insert it.\n\
This is useful for inserting control characters.\n\
")
X("universal-argument", universal_argument, true, "\
Begin a numeric argument for the following command.\n\
Digits or minus sign following C-u make up the numeric argument.\n\
C-u following the digits or minus sign ends the argument.\n\
C-u without digits or minus sign provides 4 as argument.\n\
Repeating C-u without digits or minus sign multiplies the argument\n\
by 4 each time.\n\
")
X("back-to-indentation", back_to_indentation, true, "\
Move point to the first non-whitespace character on this line.\n\
")
X("forward-word", forward_word, true, "\
Move point forward one word (backward if the argument is negative).\n\
With argument, do this that many times.\n\
")
X("backward-word", backward_word, true, "\
Move backward until encountering the end of a word (forward if the\n\
argument is negative).\n\
With argument, do this that many times.\n\
")
X("forward-sexp", forward_sexp, true, "\
Move forward across one balanced expression (sexp).\n\
With argument, do it that many times.  Negative arg -N means\n\
move backward across N balanced expressions.\n\
")
X("backward-sexp", backward_sexp, true, "\
Move backward across one balanced expression (sexp).\n\
With argument, do it that many times.  Negative arg -N means\n\
move forward across N balanced expressions.\n\
")
X("transpose-chars", transpose_chars, true, "\
Interchange characters around point, moving forward one character.\n\
With prefix arg ARG, effect is to take character before point\n\
and drag it forward past ARG other characters (backward if ARG negative).\n\
If no argument and at end of line, the previous two chars are exchanged.\n\
")
X("transpose-words", transpose_words, true, "\
Interchange words around point, leaving point at end of them.\n\
With prefix arg ARG, effect is to take word before or around point\n\
and drag it forward past ARG other words (backward if ARG negative).\n\
If ARG is zero, the words around or after point and around or after mark\n\
are interchanged.\n\
")
X("transpose-sexps", transpose_sexps, true, "\
Like M-x transpose-words but applies to sexps.\n\
")
X("transpose-lines", transpose_lines, true, "\
Exchange current line and previous line, leaving point after both.\n\
With argument ARG, takes previous line and moves it past ARG lines.\n\
With argument 0, interchanges line point is in with line mark is in.\n\
")
X("mark-word", mark_word, true, "\
Set mark argument words away from point.\n\
")
X("mark-sexp", mark_sexp, true, "\
Set mark ARG sexps from point.\n\
The place mark goes is the same place C-M-f would\n\
move to with the same argument.\n\
")
X("forward-line", forward_line, true, "\
Move N lines forward (backward if N is negative).\n\
Precisely, if point is on line I, move to the start of line I + N.\n\
")
X("backward-paragraph", backward_paragraph, true, "\
Move backward to start of paragraph.  With argument N, do it N times.\n\
")
X("forward-paragraph", forward_paragraph, true, "\
Move forward to end of paragraph.  With argument N, do it N times.\n\
")
X("mark-paragraph", mark_paragraph, true, "\
Put point at beginning of this paragraph, mark at end.\n\
The paragraph marked is the one that contains point or follows point.\n\
")
X("fill-paragraph", fill_paragraph, true, "\
Fill paragraph at or after point.\n\
")
X("downcase-word", downcase_word, true, "\
Convert following word (or ARG words) to lower case, moving over.\n\
")
X("upcase-word", upcase_word, true, "\
Convert following word (or ARG words) to upper case, moving over.\n\
")
X("capitalize-word", capitalize_word, true, "\
Capitalize the following word (or ARG words), moving over.\n\
This gives the word(s) a first character in upper case\n\
and the rest lower case.\n\
")
X("upcase-region", upcase_region, true, "\
Convert the region to upper case.\n\
")
X("downcase-region", downcase_region, true, "\
Convert the region to lower case.\n\
")
X("shell-command", shell_command, true, "\
Execute string COMMAND in inferior shell; display output, if any.\n\
With prefix argument, insert the command's output at point.\n\
\n\
Command is executed synchronously.  The output appears in the buffer\n\
`*Shell Command Output*'.  If the output is short enough to display\n\
in the echo area, it is shown there, but it is nonetheless available\n\
in buffer `*Shell Command Output*' even though that buffer is not\n\
automatically displayed.\n\
\n\
The optional second argument OUTPUT-BUFFER, if non-nil,\n\
says to insert the output in the current buffer.\n\
")
X("shell-command-on-region", shell_command_on_region, true, "\
Execute string command in inferior shell with region as input.\n\
Normally display output (if any) in temp buffer `*Shell Command Output*';\n\
Prefix arg means replace the region with it.  Return the exit code of\n\
command.\n\
\n\
If the command generates output, the output may be displayed\n\
in the echo area or in a buffer.\n\
If the output is short enough to display in the echo area, it is shown\n\
there.  Otherwise it is displayed in the buffer `*Shell Command Output*'.\n\
The output is available in that buffer in both cases.\n\
")
X("delete-region", delete_region, true, "\
Delete the text between point and mark.\n\
")
X("delete-blank-lines", delete_blank_lines, true, "\
On blank line, delete all surrounding blank lines, leaving just one.\n\
On isolated blank line, delete that one.\n\
On nonblank line, delete any immediately following blank lines.\n\
")
X("describe-function", describe_function, true, "\
Display the full documentation of a function.\n\
")
X("describe-variable", describe_variable, true, "\
Display the full documentation of a variable.\n\
")
X("describe-key", describe_key, true, "\
Display documentation of the command invoked by a key sequence.\n\
")
X("kill-line", kill_line, true, "\
Kill the rest of the current line; if no nonblanks there, kill thru newline.\n\
With prefix argument ARG, kill that many lines from point.\n\
Negative arguments kill lines backward.\n\
With zero argument, kills the text before point on the current line.\n\
\n\
If `kill-whole-line' is non-nil, then this command kills the whole line\n\
including its terminating newline, when used at the beginning of a line\n\
with no argument.\n\
")
X("kill-region", kill_region, true, "\
Kill between point and mark.\n\
The text is deleted but saved in the kill ring.\n\
The command C-y (yank) can retrieve it from there.\n\
If the buffer is read-only, Zile will beep and refrain from deleting\n\
the text, but put the text in the kill ring anyway.  This means that\n\
you can use the killing commands to copy text from a read-only buffer.\n\
If the previous command was also a kill command,\n\
the text killed this time appends to the text killed last time\n\
to make one entry in the kill ring.\n\
")
X("copy-region-as-kill", copy_region_as_kill, true, "\
Save the region as if killed, but don't kill it.\n\
")
X("kill-word", kill_word, true, "\
Kill characters forward until encountering the end of a word.\n\
With argument ARG, do this that many times.\n\
")
X("backward-kill-word", backward_kill_word, true, "\
Kill characters backward until encountering the end of a word.\n\
With argument ARG, do this that many times.\n\
")
X("kill-sexp", kill_sexp, true, "\
Kill the sexp (balanced expression) following the cursor.\n\
With ARG, kill that many sexps after the cursor.\n\
Negative arg -N means kill N sexps before the cursor.\n\
")
X("yank", yank, true, "\
Reinsert the last stretch of killed text.\n\
More precisely, reinsert the stretch of killed text most recently\n\
killed OR yanked.  Put point at end, and set mark at beginning.\n\
")
X("tab-to-tab-stop", tab_to_tab_stop, true, "\
Insert a tabulation at the current point position into the current\n\
buffer.\n\
")
X("newline", newline, true, "\
Insert a newline at the current point position into\n\
the current buffer.\n\
")
X("open-line", open_line, true, "\
Insert a newline and leave point before it.\n\
")
X("insert", insert, false, "\
Insert the argument at point.\n\
")
X("delete-char", delete_char, true, "\
Delete the following N characters (previous if N is negative).\n\
")
X("backward-delete-char", backward_delete_char, true, "\
Delete the previous N characters (following if N is negative).\n\
")
X("delete-horizontal-space", delete_horizontal_space, true, "\
Delete all spaces and tabs around point.\n\
")
X("just-one-space", just_one_space, true, "\
Delete all spaces and tabs around point, leaving one space.\n\
")
X("indent-relative", indent_relative, true, "\
Space out to under next indent point in previous nonblank line.\n\
An indent point is a non-whitespace character following whitespace.\n\
The following line shows the indentation points in this line.\n\
    ^         ^    ^     ^   ^           ^      ^  ^    ^\n\
If the previous nonblank line has no indent points beyond the\n\
column point starts at, `tab-to-tab-stop' is done instead, unless\n\
this command is invoked with a numeric argument, in which case it\n\
does nothing.\n\
")
X("indent-for-tab-command", indent_for_tab_command, true, "\
Indent line or insert a tab.\n\
Depending on `tab-always-indent', either insert a tab or indent.\n\
If initial point was within line's indentation, position after\n\
the indentation.  Else stay at same point in text.\n\
")
X("newline-and-indent", newline_and_indent, true, "\
Insert a newline, then indent.\n\
Indentation is done using the `indent-for-tab-command' function.\n\
")
X("load", load, true, "\
Execute a file of Lisp code named FILE.\n\
")
X("start-kbd-macro", start_kbd_macro, true, "\
Record subsequent keyboard input, defining a keyboard macro.\n\
The commands are recorded even as they are executed.\n\
Use C-x ) to finish recording and make the macro available.\n\
")
X("end-kbd-macro", end_kbd_macro, true, "\
Finish defining a keyboard macro.\n\
The definition was started by C-x (.\n\
The macro is now available for use via C-x e.\n\
")
X("call-last-kbd-macro", call_last_kbd_macro, true, "\
Call the last keyboard macro that you defined with C-x (.\n\
A prefix argument serves as a repeat count.\n\
")
X("execute-kbd-macro", execute_kbd_macro, false, "\
Execute macro as string of editor command characters.\n\
")
X("recenter", recenter, true, "\
Center point in selected window and redisplay frame.\n\
")
X("copy-to-register", copy_to_register, true, "\
Copy region into register REGISTER.\n\
")
X("insert-register", insert_register, true, "\
Insert contents of the user specified register.\n\
Puts point before and mark after the inserted text.\n\
")
X("list-registers", list_registers, true, "\
List defined registers.\n\
")
X("search-forward", search_forward, true, "\
Search forward from point for the user specified text.\n\
")
X("search-backward", search_backward, true, "\
Search backward from point for the user specified text.\n\
")
X("search-forward-regexp", search_forward_regexp, true, "\
Search forward from point for regular expression REGEXP.\n\
")
X("search-backward-regexp", search_backward_regexp, true, "\
Search backward from point for match for regular expression REGEXP.\n\
")
X("isearch-forward", isearch_forward, true, "\
Do incremental search forward.\n\
With a prefix argument, do an incremental regular expression search instead.\n\
As you type characters, they add to the search string and are found.\n\
Type return to exit, leaving point at location found.\n\
Type C-s to search again forward, C-r to search again backward.\n\
C-g when search is successful aborts and moves point to starting point.\n\
")
X("isearch-backward", isearch_backward, true, "\
Do incremental search backward.\n\
With a prefix argument, do a regular expression search instead.\n\
As you type characters, they add to the search string and are found.\n\
Type return to exit, leaving point at location found.\n\
Type C-r to search again backward, C-s to search again forward.\n\
C-g when search is successful aborts and moves point to starting point.\n\
")
X("isearch-forward-regexp", isearch_forward_regexp, true, "\
Do incremental search forward for regular expression.\n\
With a prefix argument, do a regular string search instead.\n\
Like ordinary incremental search except that your input\n\
is treated as a regexp.  See M-x isearch-forward for more info.\n\
")
X("isearch-backward-regexp", isearch_backward_regexp, true, "\
Do incremental search backward for regular expression.\n\
With a prefix argument, do a regular string search instead.\n\
Like ordinary incremental search except that your input\n\
is treated as a regexp.  See M-x isearch-backward for more info.\n\
")
X("query-replace", query_replace, true, "\
Replace occurrences of a string with other text.\n\
As each match is found, the user must type a character saying\n\
what to do with it.\n\
")
X("undo", undo, true, "\
Undo some previous changes.\n\
Repeat this command to undo more changes.\n\
")
X("set-variable", set_variable, true, "\
Set a variable value to the user-specified value.\n\
")
X("split-window", split_window, true, "\
Split current window into two windows, one above the other.\n\
Both windows display the same buffer now current.\n\
")
X("delete-window", delete_window, true, "\
Remove the current window from the screen.\n\
")
X("enlarge-window", enlarge_window, true, "\
Make current window one line bigger.\n\
")
X("shrink-window", shrink_window, true, "\
Make current window one line smaller.\n\
")
X("delete-other-windows", delete_other_windows, true, "\
Make the selected window fill the screen.\n\
")
X("other-window", other_window, true, "\
Select the first different window on the screen.\n\
All windows are arranged in a cyclic order.\n\
This command selects the window one step away in that order.\n\
")
