Time	Sum	Command
7	2984	> __fish_on_interactive
62	62	-> functions -e __fish_on_interactive
84	2915	-> __fish_config_interactive
285	289	--> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/__fish_config_interactive.fish
4	4	---> function __fish_config_interactive -d "Initializations that should be performed when entering interactive mode"...
2	5	--> if not set -q __fish_initialized...
3	3	---> not set -q __fish_initialized
4	4	--> set -g __fish_active_key_bindings
3	3	--> function __init_uvar -d "Sets a universal variable if it's not already set"...
1	7	--> if test $__fish_initialized -lt 3400...
6	6	---> test $__fish_initialized -lt 3400
6	21	--> if not set -q FISH_UNIT_TESTS_RUNNING...
2	2	---> not set -q FISH_UNIT_TESTS_RUNNING
5	5	---> set -l script $__fish_data_dir/tools/create_manpage_completions.py
0	8	---> if not test -d $__fish_user_data_dir/generated_completions...
8	8	----> not test -d $__fish_user_data_dir/generated_completions
3	449	--> if status --is-interactive...
2	2	---> status --is-interactive
64	117	---> functions -q fish_greeting
51	53	----> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_greeting.fish
2	2	-----> function fish_greeting...
5	327	---> fish_greeting
6	303	----> if not set -q fish_greeting...
3	3	-----> not set -q fish_greeting
35	90	-----> set -l line1 (_ 'Welcome to fish, the friendly interactive shell')
55	55	------> _ 'Welcome to fish, the friendly interactive shell'
33	197	-----> set -l line2 \n(printf (_ 'Type %shelp%s for instructions on how to use fish') (set_color green) (set_color normal))
86	164	------> printf (_ 'Type %shelp%s for instructions on how to use fish') (set_color green) (set_color normal)
8	8	-------> _ 'Type %shelp%s for instructions on how to use fish'
65	65	-------> set_color green
5	5	-------> set_color normal
7	7	-----> set -g fish_greeting "$line1$line2"
2	4	----> if set -q fish_private_mode...
2	2	-----> set -q fish_private_mode
4	4	----> test -n "$fish_greeting"
11	11	----> echo $fish_greeting
0	5	--> if test -d /etc/init.d...
5	5	---> test -d /etc/init.d
59	59	--> complete -c [ --wraps test
4	4	--> complete -c ! --wraps not
79	160	--> complete -c(builtin -n | string match -rv '(\.|:|source|cd|contains|count|echo|exec|printf|random|realpath|set|\\[|test|for)') --no-files
81	81	---> builtin -n | string match -rv '(\.|:|source|cd|contains|count|echo|exec|printf|random|realpath|set|\\[|test|for)'
5	5	--> function __fish_reload_key_bindings -d "Reload key bindings when binding variable change" --on-variable fish_key_bindings...
11	1709	--> __fish_reload_key_bindings
9	15	---> __init_uvar fish_key_bindings fish_default_key_bindings
1	6	----> if not set --query $argv[1]...
5	5	-----> not set --query $argv[1]
1	9	---> if test "$fish_key_bindings" = "$__fish_active_key_bindings" -a -n "$fish_key_bindings"...
8	8	----> test "$fish_key_bindings" = "$__fish_active_key_bindings" -a -n "$fish_key_bindings"
1	234	---> if not functions -q "$fish_key_bindings"...
71	233	----> not functions -q "$fish_key_bindings"
157	162	-----> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_default_key_bindings.fish
5	5	------> function fish_default_key_bindings -d "emacs-like key binds"...
6	6	---> set -g __fish_active_key_bindings "$fish_key_bindings"
4	4	---> set -g fish_bind_mode default
2	1362	---> if test "$fish_key_bindings" = fish_default_key_bindings...
4	4	----> test "$fish_key_bindings" = fish_default_key_bindings
37	1356	----> fish_default_key_bindings 2>/dev/null
1	8	-----> if contains -- -h $argv...
4	4	------> contains -- -h $argv
3	3	------> contains -- --help $argv
3	19	-----> if not set -q argv[1]...
2	2	------> not set -q argv[1]
7	7	------> bind --erase --all --preset
0	7	------> if test "$fish_key_bindings" != fish_default_key_bindings...
7	7	-------> test "$fish_key_bindings" != fish_default_key_bindings
3	8	-----> if not contains -- -s $argv...
2	2	------> not contains -- -s $argv
3	3	------> set argv -s $argv
120	1123	-----> __fish_shared_key_bindings $argv
288	297	------> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/__fish_shared_key_bindings.fish
5	5	-------> function __fish_shared_key_bindings -d "Bindings shared between emacs and vi mode"...
3	3	-------> function __fish_commandline_insert_escaped --description 'Insert the first arg escaped if a second arg is given'...
1	1	-------> function __fish_start_bracketed_paste...
0	0	-------> function __fish_stop_bracketed_paste...
0	9	------> if contains -- -h $argv...
5	5	-------> contains -- -h $argv
4	4	-------> contains -- --help $argv
8	8	------> bind --preset $argv \cy yank
4	4	------> bind --preset $argv \ey yank-pop
5	5	------> bind --preset $argv -k right forward-char
4	4	------> bind --preset $argv -k left backward-char
4	4	------> bind --preset $argv \e\[C forward-char
4	4	------> bind --preset $argv \e\[D backward-char
3	3	------> bind --preset $argv \eOC forward-char
3	3	------> bind --preset $argv \eOD backward-char
4	4	------> bind --preset $argv \e\[1\;5C forward-word
4	4	------> bind --preset $argv \e\[1\;5D backward-word
5	5	------> bind --preset $argv -k ppage beginning-of-history
4	4	------> bind --preset $argv -k npage end-of-history
4	4	------> bind --preset $argv \cx fish_clipboard_copy
4	4	------> bind --preset $argv \cv fish_clipboard_paste
4	4	------> bind --preset $argv \e cancel
4	4	------> bind --preset $argv \t complete
4	4	------> bind --preset $argv \cs pager-toggle-search
4	4	------> bind --preset $argv --key btab complete-and-search
5	5	------> bind --preset $argv -k sdc history-pager-delete or backward-delete-char
5	5	------> bind --preset $argv \e\n "commandline -f expand-abbr; commandline -i \n"
5	5	------> bind --preset $argv \e\r "commandline -f expand-abbr; commandline -i \n"
4	4	------> bind --preset $argv -k down down-or-search
4	4	------> bind --preset $argv -k up up-or-search
4	4	------> bind --preset $argv \e\[A up-or-search
4	4	------> bind --preset $argv \e\[B down-or-search
4	4	------> bind --preset $argv \eOA up-or-search
3	3	------> bind --preset $argv \eOB down-or-search
4	4	------> bind --preset $argv -k sright forward-bigword
5	5	------> bind --preset $argv -k sleft backward-bigword
4	4	------> bind --preset $argv \e\eOC nextd-or-forward-word
4	4	------> bind --preset $argv \e\eOD prevd-or-backward-word
4	4	------> bind --preset $argv \e\e\[C nextd-or-forward-word
4	4	------> bind --preset $argv \e\e\[D prevd-or-backward-word
4	4	------> bind --preset $argv \eO3C nextd-or-forward-word
4	4	------> bind --preset $argv \eO3D prevd-or-backward-word
4	4	------> bind --preset $argv \e\[3C nextd-or-forward-word
7	7	------> bind --preset $argv \e\[3D prevd-or-backward-word
4	4	------> bind --preset $argv \e\[1\;3C nextd-or-forward-word
4	4	------> bind --preset $argv \e\[1\;3D prevd-or-backward-word
4	4	------> bind --preset $argv \e\[1\;9C nextd-or-forward-word
4	4	------> bind --preset $argv \e\[1\;9D prevd-or-backward-word
4	4	------> bind --preset $argv \e\eOA history-token-search-backward
4	4	------> bind --preset $argv \e\eOB history-token-search-forward
4	4	------> bind --preset $argv \e\e\[A history-token-search-backward
4	4	------> bind --preset $argv \e\e\[B history-token-search-forward
5	5	------> bind --preset $argv \eO3A history-token-search-backward
4	4	------> bind --preset $argv \eO3B history-token-search-forward
4	4	------> bind --preset $argv \e\[3A history-token-search-backward
3	3	------> bind --preset $argv \e\[3B history-token-search-forward
4	4	------> bind --preset $argv \e\[1\;3A history-token-search-backward
4	4	------> bind --preset $argv \e\[1\;3B history-token-search-forward
4	4	------> bind --preset $argv \e\[1\;9A history-token-search-backward
4	4	------> bind --preset $argv \e\[1\;9B history-token-search-forward
4	4	------> bind --preset $argv \e. history-token-search-backward
4	4	------> bind --preset $argv \el __fish_list_current_token
4	4	------> bind --preset $argv \eo __fish_preview_current_file
3	3	------> bind --preset $argv \ew __fish_whatis_current_token
4	4	------> bind --preset $argv \cl clear-screen
4	4	------> bind --preset $argv \cc cancel-commandline
4	4	------> bind --preset $argv \cu backward-kill-line
3	3	------> bind --preset $argv \cw backward-kill-path-component
4	4	------> bind --preset $argv \e\[F end-of-line
4	4	------> bind --preset $argv \e\[H beginning-of-line
7	7	------> bind --preset $argv \ed 'set -l cmd (commandline); if test -z "$cmd"; echo; dirh; commandline -f repaint; else; commandline -f kill-word; end'
4	4	------> bind --preset $argv \cd delete-or-exit
5	5	------> bind --preset $argv \es 'for cmd in sudo doas please; if command -q $cmd; fish_commandline_prepend $cmd; break; end; end'
4	4	------> bind --preset $argv -k f1 __fish_man_page
4	4	------> bind --preset $argv \eh __fish_man_page
8	8	------> bind --preset $argv \ep __fish_paginate
4	4	------> bind --preset $argv \e\# __fish_toggle_comment_commandline
5	5	------> bind --preset $argv \ee edit_command_buffer
5	5	------> bind --preset $argv \ev edit_command_buffer
46	139	------> for mode in (bind --list-modes | string match -v paste)...
61	61	-------> bind --list-modes | string match -v paste
9	9	-------> bind --preset -M $mode \e\[I 'emit fish_focus_in'
5	5	-------> bind --preset -M $mode \e\[O false
5	5	-------> bind --preset -M $mode \e\[\?1004h false
5	5	-------> bind --preset -M $mode \e\[I 'emit fish_focus_in'
4	4	-------> bind --preset -M $mode \e\[O false
4	4	-------> bind --preset -M $mode \e\[\?1004h false
38	108	------> for mode in (bind --list-modes | string match -v paste)...
56	56	-------> bind --list-modes | string match -v paste
9	9	-------> bind --preset -M $mode -m paste \e\[200~ __fish_start_bracketed_paste
5	5	-------> bind --preset -M $mode -m paste \e\[200~ __fish_start_bracketed_paste
4	4	------> bind --preset -M paste \e\[201~ __fish_stop_bracketed_paste
4	4	------> bind --preset -M paste "" self-insert
4	4	------> bind --preset -M paste \r "commandline -i \n"
4	4	------> bind --preset -M paste "'" "__fish_commandline_insert_escaped \' \$__fish_paste_quoted"
4	4	------> bind --preset -M paste \\ "__fish_commandline_insert_escaped \\\ \$__fish_paste_quoted"
3	3	------> bind --preset -M paste " " self-insert-notfirst
11	121	------> if not set -l index (contains --index -- -M $argv)...
33	38	-------> not set -l index (contains --index -- -M $argv)
5	5	--------> contains --index -- -M $argv
6	6	-------> bind --preset $argv "" self-insert
5	5	-------> bind --preset $argv " " self-insert expand-abbr
5	5	-------> bind --preset $argv ";" self-insert expand-abbr
4	4	-------> bind --preset $argv "|" self-insert expand-abbr
4	4	-------> bind --preset $argv "&" self-insert expand-abbr
4	4	-------> bind --preset $argv ">" self-insert expand-abbr
4	4	-------> bind --preset $argv "<" self-insert expand-abbr
4	4	-------> bind --preset $argv ")" self-insert expand-abbr
6	6	-------> bind --preset $argv -k nul 'test -n "$(commandline)" && commandline -i " "'
6	6	-------> bind --preset $argv \e\[32\;2u 'commandline -i " "; commandline -f expand-abbr'
4	4	-------> bind --preset $argv \n execute
4	4	-------> bind --preset $argv \r execute
4	4	-------> bind --preset $argv \e\[27\;5\;13~ execute
4	4	-------> bind --preset $argv \e\[13\;5u execute
4	4	-------> bind --preset $argv \e\[27\;2\;13~ execute
4	4	-------> bind --preset $argv \e\[13\;2u execute
4	4	-----> bind --preset $argv \ck kill-line
3	3	-----> bind --preset $argv \eOC forward-char
3	3	-----> bind --preset $argv \eOD backward-char
3	3	-----> bind --preset $argv \e\[C forward-char
3	3	-----> bind --preset $argv \e\[D backward-char
4	4	-----> bind --preset $argv -k right forward-char
3	3	-----> bind --preset $argv -k left backward-char
4	4	-----> bind --preset $argv -k dc delete-char
4	4	-----> bind --preset $argv -k backspace backward-delete-char
4	4	-----> bind --preset $argv \x7f backward-delete-char
5	5	-----> bind --preset $argv \e\[1~ beginning-of-line
4	4	-----> bind --preset $argv \e\[3~ delete-char
4	4	-----> bind --preset $argv \e\[4~ end-of-line
4	4	-----> bind --preset $argv -k home beginning-of-line
4	4	-----> bind --preset $argv -k end end-of-line
4	4	-----> bind --preset $argv \ca beginning-of-line
3	3	-----> bind --preset $argv \ce end-of-line
3	3	-----> bind --preset $argv \ch backward-delete-char
4	4	-----> bind --preset $argv \cp up-or-search
4	4	-----> bind --preset $argv \cn down-or-search
4	4	-----> bind --preset $argv \cf forward-char
3	3	-----> bind --preset $argv \cb backward-char
4	4	-----> bind --preset $argv \ct transpose-chars
4	4	-----> bind --preset $argv \cg cancel
4	4	-----> bind --preset $argv \c_ undo
3	3	-----> bind --preset $argv \cz undo
4	4	-----> bind --preset $argv \e/ redo
4	4	-----> bind --preset $argv \et transpose-words
4	4	-----> bind --preset $argv \eu upcase-word
5	5	-----> bind --preset $argv \ec capitalize-word
4	4	-----> bind --preset $argv \e\x7f backward-kill-word
4	4	-----> bind --preset $argv \e\b backward-kill-word
5	17	-----> if not test "$TERM_PROGRAM" = Apple_Terminal...
4	4	------> not test "$TERM_PROGRAM" = Apple_Terminal
4	4	------> bind --preset $argv \eb backward-word
4	4	------> bind --preset $argv \ef forward-word
4	4	-----> bind --preset $argv \e\< beginning-of-buffer
4	4	-----> bind --preset $argv \e\> end-of-buffer
4	4	-----> bind --preset $argv \ed kill-word
4	4	-----> bind --preset $argv \cr history-pager
4	4	-----> switch "$TERM"...
3	3	-----> set -e -g fish_cursor_selection_mode
0	68	---> if functions --query fish_user_key_bindings >/dev/null...
68	68	----> functions --query fish_user_key_bindings >/dev/null
2	25	--> if not set -q FISH_UNIT_TESTS_RUNNING...
3	3	---> not set -q FISH_UNIT_TESTS_RUNNING
2	2	---> function __fish_enable_bracketed_paste --on-event fish_prompt...
3	3	---> function __fish_disable_bracketed_paste --on-event fish_preexec --on-event fish_exit...
2	2	---> status is-interactive
6	13	---> __fish_enable_bracketed_paste
7	7	----> printf "\e[?2004h"
2	9	--> if set -q TMUX...
2	2	---> set -q TMUX
2	2	---> not set -q FISH_UNIT_TESTS_RUNNING
2	2	---> function __fish_enable_focus --on-event fish_postexec...
1	1	---> function __fish_disable_focus --on-event fish_preexec...
3	23	--> if not set -q fish_handle_reflow...
2	2	---> not set -q fish_handle_reflow
2	18	---> if set -q VTE_VERSION...
2	2	----> set -q VTE_VERSION
5	5	----> string match -q -- 'alacritty*' $TERM
4	4	----> string match -q -- '*kitty' $TERM
2	2	----> set -q KONSOLE_VERSION
3	3	----> set -g fish_handle_reflow 1
4	4	--> function __fish_winch_handler --on-signal WINCH -d "Repaint screen when window changes size"...
2	29	--> if not set -q FISH_UNIT_TESTS_RUNNING...
2	2	---> not set -q FISH_UNIT_TESTS_RUNNING
5	25	---> begin...
4	4	----> string match -q -- 'foot*' $TERM
3	3	----> string match -q -- 'xterm-kitty*' $TERM
4	4	----> test 0"$VTE_VERSION" -ge 3405
3	3	----> test "$TERM_PROGRAM" = Apple_Terminal
3	3	----> test "$TERM_PROGRAM" = WezTerm
3	3	----> test "$TERM_PROGRAM" = iTerm.app
5	5	--> set __fish_initialized 3400
16	16	--> functions -e __fish_config_interactive
10	2004	> __direnv_export_eval
1974	1974	-> "/nix/store/bri4plkp5ykp2dvjs1bwzd92z7grp6h7-direnv-2.34.0/bin/direnv" export fish | source
5	20	-> if test "$direnv_fish_mode" != "disable_arrow"...
12	12	--> test "$direnv_fish_mode" != "disable_arrow"
3	3	--> function __direnv_cd_hook --on-variable PWD...
30	33	> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_mode_prompt.fish
3	3	-> function fish_mode_prompt --description "Displays the current mode"...
7	170	> fish_mode_prompt
87	163	-> fish_default_mode_prompt
60	65	--> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_default_mode_prompt.fish
5	5	---> function fish_default_mode_prompt --description "Display vi prompt mode"...
2	11	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
6	6	---> test "$fish_key_bindings" = fish_vi_key_bindings
3	3	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
84	87	> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_prompt.fish
3	3	-> function fish_prompt --description 'Write out the prompt'...
14	13149	> fish_prompt
7	7	-> set -l last_pipestatus $pipestatus
4	4	-> set -lx __fish_last_status $status
33	40	-> set -l normal (set_color normal)
7	7	--> set_color normal
4	4	-> set -q fish_color_status
4	4	-> set -l color_cwd $fish_color_cwd
3	3	-> set -l suffix '>'
2	131	-> if functions -q fish_is_root_user...
67	104	--> functions -q fish_is_root_user
34	37	---> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_is_root_user.fish
3	3	----> function fish_is_root_user --description "Check if the user is root"...
7	25	--> fish_is_root_user
0	11	---> if test "$EUID" = 0 2>/dev/null...
11	11	----> test "$EUID" = 0 2>/dev/null
0	5	---> if contains -- $USER root toor Administrator...
5	5	----> contains -- $USER root toor Administrator
2	2	---> return 1
4	4	-> set -l bold_flag --bold
3	3	-> set -q __fish_prompt_status_generation
5	5	-> set -g __fish_prompt_status_generation $status_generation
3	9	-> if test $__fish_prompt_status_generation = $status_generation...
4	4	--> test $__fish_prompt_status_generation = $status_generation
2	2	--> set bold_flag
4	4	-> set __fish_prompt_status_generation $status_generation
35	42	-> set -l status_color (set_color $fish_color_status)
7	7	--> set_color $fish_color_status
32	39	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
7	7	--> set_color $bold_flag $fish_color_status
35	235	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
81	200	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
75	78	---> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/__fish_print_pipestatus.fish
3	3	----> function __fish_print_pipestatus --description "Print pipestatus for prompt"...
5	5	---> set -l last_status
1	7	---> if set -q __fish_last_status...
2	2	----> set -q __fish_last_status
4	4	----> set last_status $__fish_last_status
4	4	---> set -l left_brace $argv[1]
3	3	---> set -l right_brace $argv[2]
3	3	---> set -l separator $argv[3]
3	3	---> set -l brace_sep_color $argv[4]
3	3	---> set -l status_color $argv[5]
4	4	---> set -e argv[1 2 3 4 5]
3	4	---> if not set -q argv[1]...
1	1	----> not set -q argv[1]
0	5	---> if not contains $last_status 0 141...
5	5	----> not contains $last_status 0 141
130	12601	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
68	556	--> prompt_login
65	69	---> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/prompt_login.fish
4	4	----> function prompt_login --description "display user name for the prompt"...
2	23	---> if not set -q __fish_machine...
5	5	----> not set -q __fish_machine
3	3	----> set -g __fish_machine
4	4	----> set -l debian_chroot $debian_chroot
1	6	----> if test -r /etc/debian_chroot...
5	5	-----> test -r /etc/debian_chroot
1	3	----> if set -q debian_chroot[1]...
2	2	-----> set -q debian_chroot[1]
0	2	---> if set -q __fish_machine[1]...
2	2	----> set -q __fish_machine[1]
4	4	---> set -l color_host $fish_color_host
0	2	---> if set -q SSH_TTY...
2	2	----> set -q SSH_TTY
196	388	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
9	9	----> set_color $fish_color_user
5	5	----> set_color normal
5	5	----> set_color $color_host
93	164	----> prompt_hostname
37	42	-----> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/prompt_hostname.fish
5	5	------> function prompt_hostname --description 'short hostname for the prompt'...
29	29	-----> string replace -r -- "\..*" "" $hostname
9	9	----> set_color normal
10	10	--> set_color $color_cwd
87	626	--> prompt_pwd
124	128	---> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/prompt_pwd.fish
4	4	----> function prompt_pwd --description 'short CWD for the prompt'...
7	7	---> set -l options h/help d/dir-length= D/full-length-dirs=
10	10	---> argparse -n prompt_pwd $options -- $argv
4	7	---> if set -q _flag_help...
3	3	----> set -q _flag_help
3	3	---> set -q argv[1]
5	5	---> set argv $PWD
3	3	---> set -ql _flag_d
3	3	---> set -q fish_prompt_pwd_dir_length
4	4	---> set -l fish_prompt_pwd_dir_length 1
3	3	---> set -l fulldirs 0
2	2	---> set -ql _flag_D
3	3	---> set -q fish_prompt_pwd_full_dirs
4	4	---> set -l fish_prompt_pwd_full_dirs 1
8	357	---> for path in $argv...
43	52	----> set -l realhome (string escape --style=regex -- ~)
9	9	-----> string escape --style=regex -- ~
39	56	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
17	17	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
7	241	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
7	7	-----> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-----> set -l full
6	170	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	------> test $fish_prompt_pwd_full_dirs -gt 0
37	149	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
37	112	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
75	75	--------> math $fish_prompt_pwd_full_dirs - 1
6	6	------> set tmp $all[1]
4	4	------> set full $all[2..]
38	53	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
15	15	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
67	11279	--> fish_vcs_prompt
30	34	---> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_vcs_prompt.fish
4	4	----> function fish_vcs_prompt --description "Print all vcs prompts"...
105	11178	---> fish_git_prompt $argv
947	2264	----> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_git_prompt.fish
3	3	-----> function __fish_git_prompt_show_upstream --description "Helper function for fish_git_prompt"...
1	1259	-----> if string match -q Darwin -- (uname)...
53	1258	------> string match -q Darwin -- (uname)
1205	1205	-------> uname
3	3	-----> function fish_git_prompt --description "Prompt function for Git"...
2	2	-----> function __fish_git_prompt_informative_status...
2	2	-----> function __fish_git_prompt_operation_branch_bare --description "fish_git_prompt helper, returns the current Git operation and branch"...
1	1	-----> function __fish_git_prompt_set_char...
2	2	-----> function __fish_git_prompt_validate_chars --description "fish_git_prompt helper, checks char variables"...
0	0	-----> function __fish_git_prompt_set_color...
2	2	-----> function __fish_git_prompt_validate_colors --description "fish_git_prompt helper, checks color variables"...
8	8	-----> function __fish_git_prompt_reset -a type -a op -a var --description "Event handler, resets prompt when functionality changes" \
    --on-variable=__fish_git_prompt_{show_informative_status,use_informative_chars}...
19	19	-----> function __fish_git_prompt_reset_color -a type -a op -a var --description "Event handler, resets prompt when any color changes" \
    --on-variable=__fish_git_prompt_color{'',_prefix,_suffix,_bare,_merging,_cleanstate,_invalidstate,_upstream,_flags,_branch,_dirtystate,_stagedstate,_branch_detached,_stashstate,_untrackedfiles} --on-variable=__fish_git_prompt_showcolorhints...
16	16	-----> function __fish_git_prompt_reset_char -a type -a op -a var --description "Event handler, resets prompt when any char changes" \
    --on-variable=__fish_git_prompt_char_{cleanstate,dirtystate,invalidstate,stagedstate,stashstate,stateseparator,untrackedfiles,upstream_ahead,upstream_behind,upstream_diverged,upstream_equal,upstream_prefix}...
1	185	----> if not command -sq git...
184	184	-----> not command -sq git
1	64	----> if functions -q __fish_git_prompt_ready...
63	63	-----> functions -q __fish_git_prompt_ready
58	1562	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1504	1504	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
5	5	----> set -l git_dir $repo_info[1]
4	4	----> set -l inside_gitdir $repo_info[2]
3	3	----> set -l bare_repo $repo_info[3]
3	3	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
4	4	----> set -l sha $repo_info[5]
39	1701	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
22	1662	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
4	4	------> set -l inside_gitdir $argv[2]
3	3	------> set -l bare_repo $argv[3]
2	2	------> set -q argv[5]
3	3	------> set -l sha $argv[5]
3	3	------> set -l branch
2	2	------> set -l operation
2	2	------> set -l detached no
2	2	------> set -l bare
2	2	------> set -l step
2	2	------> set -l total
18	45	------> if test -d $git_dir/rebase-merge...
6	6	-------> test -d $git_dir/rebase-merge
1	21	-------> if test -d $git_dir/rebase-apply...
4	4	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
4	4	--------> test -f $git_dir/CHERRY_PICK_HEAD
4	4	--------> test -f $git_dir/REVERT_HEAD
4	4	--------> test -f $git_dir/BISECT_LOG
1	6	------> if test -n "$step" -a -n "$total"...
5	5	-------> test -n "$step" -a -n "$total"
3	1539	------> if test -z "$branch"...
3	3	-------> test -z "$branch"
1	1533	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
43	1532	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1489	1489	---------> command git symbolic-ref HEAD 2>/dev/null
1	6	------> if test true = $inside_gitdir...
5	5	-------> test true = $inside_gitdir
5	5	------> echo $operation
4	4	------> echo $branch
3	3	------> echo $detached
2	2	------> echo $bare
5	5	----> set -l r $rbc[1]
3	3	----> set -l b $rbc[2]
4	4	----> set -l detached $rbc[3]
3	3	----> set -l dirtystate
3	3	----> set -l stagedstate
2	2	----> set -l invalidstate
2	2	----> set -l stashstate
2	2	----> set -l untrackedfiles
4	4	----> set -l c $rbc[4]
2	2	----> set -l p
4	4	----> set -l informative_status
2	2	----> set -q __fish_git_prompt_status_order
4	4	----> set -g __fish_git_prompt_status_order stagedstate invalidstate dirtystate untrackedfiles stashstate
3	2849	----> if not set -q ___fish_git_prompt_init...
2	2	-----> not set -q ___fish_git_prompt_init
16	1191	-----> __fish_git_prompt_validate_chars
5	5	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
32	123	------> __fish_git_prompt_set_char __fish_git_prompt_char_cleanstate ''
6	6	-------> set -l user_variable_name "$argv[1]"
4	4	-------> set -l char $argv[2]
0	3	-------> if set -q argv[3]...
3	3	--------> set -q argv[3]
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
19	70	-------> if not set -q $variable...
4	4	--------> not set -q $variable
39	47	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
4	4	---------> set -q $user_variable_name
4	4	---------> echo $char
10	106	------> __fish_git_prompt_set_char __fish_git_prompt_char_dirtystate '*' '✚'
5	5	-------> set -l user_variable_name "$argv[1]"
4	4	-------> set -l char $argv[2]
1	31	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
19	28	--------> begin...
5	5	---------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
4	4	---------> contains -- "$__fish_git_prompt_use_informative_chars" yes true 1
4	4	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
2	49	-------> if not set -q $variable...
3	3	--------> not set -q $variable
37	44	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
4	4	---------> set -q $user_variable_name
3	3	---------> echo $char
8	100	------> __fish_git_prompt_set_char __fish_git_prompt_char_invalidstate '#' '✖'
5	5	-------> set -l user_variable_name "$argv[1]"
4	4	-------> set -l char $argv[2]
1	30	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
19	27	--------> begin...
5	5	---------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
3	3	---------> contains -- "$__fish_git_prompt_use_informative_chars" yes true 1
4	4	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
2	46	-------> if not set -q $variable...
3	3	--------> not set -q $variable
33	41	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
4	4	---------> set -q $user_variable_name
4	4	---------> echo $char
7	98	------> __fish_git_prompt_set_char __fish_git_prompt_char_stagedstate '+' '●'
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l char $argv[2]
1	30	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
18	27	--------> begin...
5	5	---------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
4	4	---------> contains -- "$__fish_git_prompt_use_informative_chars" yes true 1
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
3	46	-------> if not set -q $variable...
2	2	--------> not set -q $variable
34	41	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
4	4	---------> set -q $user_variable_name
3	3	---------> echo $char
10	98	------> __fish_git_prompt_set_char __fish_git_prompt_char_stashstate '$' '⚑'
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l char $argv[2]
1	30	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
19	27	--------> begin...
4	4	---------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
4	4	---------> contains -- "$__fish_git_prompt_use_informative_chars" yes true 1
3	3	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
3	45	-------> if not set -q $variable...
2	2	--------> not set -q $variable
34	40	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
3	3	---------> set -q $user_variable_name
3	3	---------> echo $char
8	98	------> __fish_git_prompt_set_char __fish_git_prompt_char_stateseparator ' ' '|'
4	4	-------> set -l user_variable_name "$argv[1]"
4	4	-------> set -l char $argv[2]
2	30	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
17	26	--------> begin...
5	5	---------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
4	4	---------> contains -- "$__fish_git_prompt_use_informative_chars" yes true 1
4	4	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
3	45	-------> if not set -q $variable...
2	2	--------> not set -q $variable
34	40	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
3	3	---------> set -q $user_variable_name
3	3	---------> echo $char
8	96	------> __fish_git_prompt_set_char __fish_git_prompt_char_untrackedfiles '%' '…'
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l char $argv[2]
2	30	-------> if set -q argv[3]...
1	1	--------> set -q argv[3]
19	27	--------> begin...
5	5	---------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
3	3	---------> contains -- "$__fish_git_prompt_use_informative_chars" yes true 1
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
2	43	-------> if not set -q $variable...
2	2	--------> not set -q $variable
31	39	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
4	4	---------> set -q $user_variable_name
4	4	---------> echo $char
9	97	------> __fish_git_prompt_set_char __fish_git_prompt_char_upstream_ahead '>' '↑'
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l char $argv[2]
2	30	-------> if set -q argv[3]...
1	1	--------> set -q argv[3]
19	27	--------> begin...
5	5	---------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
3	3	---------> contains -- "$__fish_git_prompt_use_informative_chars" yes true 1
4	4	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
2	44	-------> if not set -q $variable...
2	2	--------> not set -q $variable
33	40	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
4	4	---------> set -q $user_variable_name
3	3	---------> echo $char
9	97	------> __fish_git_prompt_set_char __fish_git_prompt_char_upstream_behind '<' '↓'
4	4	-------> set -l user_variable_name "$argv[1]"
4	4	-------> set -l char $argv[2]
0	29	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
20	27	--------> begin...
4	4	---------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
3	3	---------> contains -- "$__fish_git_prompt_use_informative_chars" yes true 1
3	3	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
2	45	-------> if not set -q $variable...
2	2	--------> not set -q $variable
35	41	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
3	3	---------> set -q $user_variable_name
3	3	---------> echo $char
11	87	------> __fish_git_prompt_set_char __fish_git_prompt_char_upstream_diverged '<>'
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l char $argv[2]
0	2	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
3	3	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
19	61	-------> if not set -q $variable...
2	2	--------> not set -q $variable
33	40	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
3	3	---------> set -q $user_variable_name
4	4	---------> echo $char
10	85	------> __fish_git_prompt_set_char __fish_git_prompt_char_upstream_equal '='
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l char $argv[2]
0	2	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
3	3	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
20	60	-------> if not set -q $variable...
2	2	--------> not set -q $variable
31	38	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
3	3	---------> set -q $user_variable_name
4	4	---------> echo $char
6	85	------> __fish_git_prompt_set_char __fish_git_prompt_char_upstream_prefix ''
4	4	-------> set -l user_variable_name "$argv[1]"
4	4	-------> set -l char $argv[2]
1	3	-------> if set -q argv[3]...
2	2	--------> set -q argv[3]
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
18	60	-------> if not set -q $variable...
3	3	--------> not set -q $variable
32	39	--------> set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
3	3	---------> set -q $user_variable_name
4	4	---------> echo $char
17	1650	-----> __fish_git_prompt_validate_colors
27	131	------> __fish_git_prompt_set_color __fish_git_prompt_color '' ''
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l default default_done
47	69	-------> switch (count $argv)...
12	12	--------> count $argv
6	6	--------> set default "$argv[2]"
4	4	--------> set default_done "$argv[3]"
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
2	20	-------> if not set -q $variable...
3	3	--------> not set -q $variable
3	15	--------> if test -n "$$user_variable_name"...
4	4	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
4	4	---------> set -g $variable_done $default_done
5	104	------> __fish_git_prompt_set_color __fish_git_prompt_color_prefix
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l default default_done
46	61	-------> switch (count $argv)...
6	6	--------> count $argv
5	5	--------> set default $___fish_git_prompt_color
4	4	--------> set default_done $___fish_git_prompt_color_done
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
3	23	-------> if not set -q $variable...
3	3	--------> not set -q $variable
1	17	--------> if test -n "$$user_variable_name"...
4	4	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
8	8	---------> set -g $variable_done $default_done
8	103	------> __fish_git_prompt_set_color __fish_git_prompt_color_suffix
4	4	-------> set -l user_variable_name "$argv[1]"
2	2	-------> set -l default default_done
47	60	-------> switch (count $argv)...
5	5	--------> count $argv
5	5	--------> set default $___fish_git_prompt_color
3	3	--------> set default_done $___fish_git_prompt_color_done
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
3	21	-------> if not set -q $variable...
2	2	--------> not set -q $variable
0	16	--------> if test -n "$$user_variable_name"...
4	4	---------> test -n "$$user_variable_name"
5	5	---------> set -g $variable $default
7	7	---------> set -g $variable_done $default_done
4	97	------> __fish_git_prompt_set_color __fish_git_prompt_color_bare
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l default default_done
47	59	-------> switch (count $argv)...
5	5	--------> count $argv
4	4	--------> set default $___fish_git_prompt_color
3	3	--------> set default_done $___fish_git_prompt_color_done
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
4	19	-------> if not set -q $variable...
2	2	--------> not set -q $variable
2	13	--------> if test -n "$$user_variable_name"...
3	3	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
4	4	---------> set -g $variable_done $default_done
7	104	------> __fish_git_prompt_set_color __fish_git_prompt_color_merging
6	6	-------> set -l user_variable_name "$argv[1]"
2	2	-------> set -l default default_done
46	60	-------> switch (count $argv)...
5	5	--------> count $argv
5	5	--------> set default $___fish_git_prompt_color
4	4	--------> set default_done $___fish_git_prompt_color_done
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
1	21	-------> if not set -q $variable...
3	3	--------> not set -q $variable
3	17	--------> if test -n "$$user_variable_name"...
4	4	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
6	6	---------> set -g $variable_done $default_done
5	100	------> __fish_git_prompt_set_color __fish_git_prompt_color_cleanstate
4	4	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l default default_done
46	59	-------> switch (count $argv)...
5	5	--------> count $argv
5	5	--------> set default $___fish_git_prompt_color
3	3	--------> set default_done $___fish_git_prompt_color_done
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
4	21	-------> if not set -q $variable...
3	3	--------> not set -q $variable
1	14	--------> if test -n "$$user_variable_name"...
4	4	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
5	5	---------> set -g $variable_done $default_done
7	98	------> __fish_git_prompt_set_color __fish_git_prompt_color_invalidstate
4	4	-------> set -l user_variable_name "$argv[1]"
2	2	-------> set -l default default_done
46	58	-------> switch (count $argv)...
5	5	--------> count $argv
4	4	--------> set default $___fish_git_prompt_color
3	3	--------> set default_done $___fish_git_prompt_color_done
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
2	19	-------> if not set -q $variable...
3	3	--------> not set -q $variable
3	14	--------> if test -n "$$user_variable_name"...
3	3	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
4	4	---------> set -g $variable_done $default_done
6	100	------> __fish_git_prompt_set_color __fish_git_prompt_color_upstream
7	7	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l default default_done
47	59	-------> switch (count $argv)...
5	5	--------> count $argv
4	4	--------> set default $___fish_git_prompt_color
3	3	--------> set default_done $___fish_git_prompt_color_done
3	3	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
4	19	-------> if not set -q $variable...
2	2	--------> not set -q $variable
2	13	--------> if test -n "$$user_variable_name"...
3	3	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
4	4	---------> set -g $variable_done $default_done
5	418	------> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
7	7	-------> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
9	98	-------> __fish_git_prompt_set_color __fish_git_prompt_color_flags
4	4	--------> set -l user_variable_name "$argv[1]"
3	3	--------> set -l default default_done
45	58	--------> switch (count $argv)...
4	4	---------> count $argv
5	5	---------> set default $___fish_git_prompt_color
4	4	---------> set default_done $___fish_git_prompt_color_done
3	3	--------> set -l variable _$user_variable_name
3	3	--------> set -l variable_done "$variable"_done
2	18	--------> if not set -q $variable...
2	2	---------> not set -q $variable
2	14	---------> if test -n "$$user_variable_name"...
4	4	----------> test -n "$$user_variable_name"
4	4	----------> set -g $variable $default
4	4	----------> set -g $variable_done $default_done
8	99	-------> __fish_git_prompt_set_color __fish_git_prompt_color_branch
3	3	--------> set -l user_variable_name "$argv[1]"
2	2	--------> set -l default default_done
46	61	--------> switch (count $argv)...
7	7	---------> count $argv
4	4	---------> set default $___fish_git_prompt_color
4	4	---------> set default_done $___fish_git_prompt_color_done
3	3	--------> set -l variable _$user_variable_name
3	3	--------> set -l variable_done "$variable"_done
4	19	--------> if not set -q $variable...
2	2	---------> not set -q $variable
2	13	---------> if test -n "$$user_variable_name"...
3	3	----------> test -n "$$user_variable_name"
4	4	----------> set -g $variable $default
4	4	----------> set -g $variable_done $default_done
12	105	-------> __fish_git_prompt_set_color __fish_git_prompt_color_dirtystate $___fish_git_prompt_color_flags $___fish_git_prompt_color_flags_done
5	5	--------> set -l user_variable_name "$argv[1]"
3	3	--------> set -l default default_done
45	60	--------> switch (count $argv)...
6	6	---------> count $argv
5	5	---------> set default "$argv[2]"
4	4	---------> set default_done "$argv[3]"
3	3	--------> set -l variable _$user_variable_name
3	3	--------> set -l variable_done "$variable"_done
1	19	--------> if not set -q $variable...
3	3	---------> not set -q $variable
3	15	---------> if test -n "$$user_variable_name"...
4	4	----------> test -n "$$user_variable_name"
4	4	----------> set -g $variable $default
4	4	----------> set -g $variable_done $default_done
8	104	-------> __fish_git_prompt_set_color __fish_git_prompt_color_stagedstate $___fish_git_prompt_color_flags $___fish_git_prompt_color_flags_done
4	4	--------> set -l user_variable_name "$argv[1]"
2	2	--------> set -l default default_done
45	63	--------> switch (count $argv)...
6	6	---------> count $argv
8	8	---------> set default "$argv[2]"
4	4	---------> set default_done "$argv[3]"
4	4	--------> set -l variable _$user_variable_name
4	4	--------> set -l variable_done "$variable"_done
2	19	--------> if not set -q $variable...
3	3	---------> not set -q $variable
2	14	---------> if test -n "$$user_variable_name"...
4	4	----------> test -n "$$user_variable_name"
4	4	----------> set -g $variable $default
4	4	----------> set -g $variable_done $default_done
34	172	------> __fish_git_prompt_set_color __fish_git_prompt_color_branch_detached (set_color red)
7	7	-------> set_color red
6	6	-------> set -l user_variable_name "$argv[1]"
2	2	-------> set -l default default_done
47	94	-------> switch (count $argv)...
8	8	--------> count $argv
6	6	--------> set default "$argv[2]"
28	33	--------> set default_done (set_color normal)
5	5	---------> set_color normal
5	5	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
2	20	-------> if not set -q $variable...
3	3	--------> not set -q $variable
3	15	--------> if test -n "$$user_variable_name"...
4	4	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
4	4	---------> set -g $variable_done $default_done
8	106	------> __fish_git_prompt_set_color __fish_git_prompt_color_stashstate $___fish_git_prompt_color_flags $___fish_git_prompt_color_flags_done
7	7	-------> set -l user_variable_name "$argv[1]"
3	3	-------> set -l default default_done
47	61	-------> switch (count $argv)...
6	6	--------> count $argv
5	5	--------> set default "$argv[2]"
3	3	--------> set default_done "$argv[3]"
4	4	-------> set -l variable _$user_variable_name
4	4	-------> set -l variable_done "$variable"_done
3	19	-------> if not set -q $variable...
2	2	--------> not set -q $variable
1	14	--------> if test -n "$$user_variable_name"...
4	4	---------> test -n "$$user_variable_name"
5	5	---------> set -g $variable $default
4	4	---------> set -g $variable_done $default_done
11	100	------> __fish_git_prompt_set_color __fish_git_prompt_color_untrackedfiles $___fish_git_prompt_color_flags $___fish_git_prompt_color_flags_done
4	4	-------> set -l user_variable_name "$argv[1]"
2	2	-------> set -l default default_done
45	59	-------> switch (count $argv)...
5	5	--------> count $argv
5	5	--------> set default "$argv[2]"
4	4	--------> set default_done "$argv[3]"
3	3	-------> set -l variable _$user_variable_name
3	3	-------> set -l variable_done "$variable"_done
2	18	-------> if not set -q $variable...
2	2	--------> not set -q $variable
2	14	--------> if test -n "$$user_variable_name"...
4	4	---------> test -n "$$user_variable_name"
4	4	---------> set -g $variable $default
4	4	---------> set -g $variable_done $default_done
3	3	-----> set -g ___fish_git_prompt_init
6	6	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
2	2	----> set -l informative
2	2	----> set -l dirty
2	2	----> set -l untracked
520	2074	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1554	1554	-----> read -lz key value
4	18	----> if not set -q dirty[1]...
5	5	-----> not set -q dirty[1]
9	9	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
7	7	----> contains dirtystate $__fish_git_prompt_status_order
2	10	----> if not set -q untracked[1]...
3	3	-----> not set -q untracked[1]
5	5	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
6	6	----> contains untrackedfiles $__fish_git_prompt_status_order
5	53	----> if test true = $inside_worktree...
4	4	-----> test true = $inside_worktree
6	35	-----> if test "$informative" = true...
4	4	------> test "$informative" = true
2	7	------> begin...
5	5	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
3	13	------> if not test "$dirty" = true...
4	4	-------> not test "$dirty" = true
3	3	-------> test "$untracked" = true
3	3	-------> test "$dirty" = true
1	5	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
4	4	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
1	9	-----> if set -q __fish_git_prompt_showupstream...
3	3	------> set -q __fish_git_prompt_showupstream
5	5	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
6	6	----> set -l branch_color $___fish_git_prompt_color_branch
5	5	----> set -l branch_done $___fish_git_prompt_color_branch_done
1	6	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
5	5	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
4	4	----> set -l f ""
14	38	----> for i in $__fish_git_prompt_status_order...
1	6	-----> if test -n "$$i"...
5	5	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
44	53	----> set b (string replace refs/heads/ '' -- $b)
9	9	-----> string replace refs/heads/ '' -- $b
1	17	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
16	16	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
3	24	----> if test -n "$b"...
5	5	-----> test -n "$b"
6	6	-----> set b "$branch_color$b$branch_done"
1	10	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
5	5	------> test -z "$dirtystate$untrackedfiles$stagedstate"
4	4	------> test -n "$___fish_git_prompt_char_cleanstate"
1	4	----> if test -n "$c"...
3	3	-----> test -n "$c"
1	4	----> if test -n "$r"...
3	3	-----> test -n "$r"
1	4	----> if test -n "$p"...
3	3	-----> test -n "$p"
1	4	----> if test -n "$f"...
3	3	-----> test -n "$f"
6	6	----> set -l format $argv[1]
1	10	----> if test -z "$format"...
4	4	-----> test -z "$format"
5	5	-----> set format " (%s)"
16	16	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
12	7589	> fish_right_prompt
7568	7568	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
9	9	-> printf " "
94	96	> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/fish_title.fish
2	2	-> function fish_title...
7	466	> fish_title
6	459	-> if not set -q INSIDE_EMACS...
5	5	--> not set -q INSIDE_EMACS
5	5	--> set -l ssh
3	3	--> set -q SSH_TTY
5	440	--> if set -q argv[1]...
3	3	---> set -q argv[1]
31	35	---> set -l command (status current-command)
4	4	----> status current-command
2	12	---> if test "$command" = fish...
7	7	----> test "$command" = fish
3	3	----> set command
56	385	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
7	7	----> string sub -l 20 -- $command
19	322	----> prompt_pwd -d 1 -D 1
7	7	-----> set -l options h/help d/dir-length= D/full-length-dirs=
15	15	-----> argparse -n prompt_pwd $options -- $argv
1	4	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
3	3	-----> set -q argv[1]
5	5	-----> set argv $PWD
2	2	-----> set -ql _flag_d
5	5	-----> set -l fish_prompt_pwd_dir_length $_flag_d
2	2	-----> set -q fish_prompt_pwd_dir_length
3	3	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
4	4	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
9	249	-----> for path in $argv...
31	39	------> set -l realhome (string escape --style=regex -- ~)
8	8	-------> string escape --style=regex -- ~
31	49	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
18	18	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
6	152	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
6	6	-------> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-------> set -l full
4	90	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	--------> test $fish_prompt_pwd_full_dirs -gt 0
33	72	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
29	39	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
10	10	----------> math $fish_prompt_pwd_full_dirs - 1
5	5	--------> set tmp $all[1]
4	4	--------> set full $all[2..]
31	46	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
15	15	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
20	42	> __direnv_export_eval_2 ls
2	13	-> if set -q __direnv_export_again...
11	11	--> set -q __direnv_export_again
9	9	-> functions --erase __direnv_cd_hook
7	17	> __fish_disable_bracketed_paste ls
10	10	-> printf "\e[?2004l"
5	13	> __fish_disable_focus ls
8	8	-> echo -n \e\[\?1004l
7	558	> fish_title ls
10	551	-> if not set -q INSIDE_EMACS...
6	6	--> not set -q INSIDE_EMACS
5	5	--> set -l ssh
3	3	--> set -q SSH_TTY
3	527	--> if set -q argv[1]...
3	3	---> set -q argv[1]
83	521	---> echo -- $ssh (string sub -l 20 -- $argv[1]) (prompt_pwd -d 1 -D 1)
9	9	----> string sub -l 20 -- $argv[1]
18	429	----> prompt_pwd -d 1 -D 1
11	11	-----> set -l options h/help d/dir-length= D/full-length-dirs=
16	16	-----> argparse -n prompt_pwd $options -- $argv
4	7	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
2	2	-----> set -q argv[1]
5	5	-----> set argv $PWD
3	3	-----> set -ql _flag_d
5	5	-----> set -l fish_prompt_pwd_dir_length $_flag_d
3	3	-----> set -q fish_prompt_pwd_dir_length
3	3	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
5	5	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
8	347	-----> for path in $argv...
48	59	------> set -l realhome (string escape --style=regex -- ~)
11	11	-------> string escape --style=regex -- ~
42	72	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
30	30	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
7	208	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
9	9	-------> test "$fish_prompt_pwd_dir_length" -eq 0
5	5	-------> set -l full
3	123	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	--------> test $fish_prompt_pwd_full_dirs -gt 0
48	104	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
41	56	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
15	15	----------> math $fish_prompt_pwd_full_dirs - 1
6	6	--------> set tmp $all[1]
5	5	--------> set full $all[2..]
41	64	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
23	23	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
43	4278	> ls
102	107	-> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/ls.fish
5	5	--> function ls --description "List contents of directory"...
9	2779	-> if not set -q __fish_ls_command...
5	5	--> not set -q __fish_ls_command
5	5	--> set -g __fish_ls_command ls
3	3	--> set -g __fish_ls_color_opt
3	3	--> set -g __fish_ls_indicators_opt
8	2754	--> if command -sq colorls...
346	346	---> command -sq colorls
7	1351	---> for opt in --color=auto -G --color...
12	1344	----> if command ls $opt / >/dev/null 2>/dev/null...
1319	1319	-----> command ls $opt / >/dev/null 2>/dev/null
11	11	-----> set -g __fish_ls_color_opt $opt
2	2	-----> break
10	1049	---> if command ls -F / >/dev/null 2>/dev/null...
1031	1031	----> command ls -F / >/dev/null 2>/dev/null
8	8	----> set -g __fish_ls_indicators_opt -F
4	4	-> set -l indicators_opt
92	251	-> isatty stdout
108	115	--> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/functions/isatty.fish
7	7	---> function isatty -d "Tests if a file descriptor is a tty"...
7	7	--> set -l options h/help
9	9	--> argparse -n isatty $options -- $argv
2	5	--> if set -q _flag_help...
3	3	---> set -q _flag_help
2	4	--> if set -q argv[2]...
2	2	---> set -q argv[2]
3	3	--> set -l fd
6	9	--> switch "$argv"...
3	3	---> set fd 1
7	7	--> test -t "$fd"
6	6	-> set -a indicators_opt $__fish_ls_indicators_opt
6	6	-> test "$TERM_PROGRAM" = Apple_Terminal
1082	1082	-> command $__fish_ls_command $__fish_ls_color_opt $indicators_opt $argv
11	25	> __fish_enable_focus ls
14	14	-> echo -n \e\[\?1004h
12	2244	> __direnv_export_eval
2210	2210	-> "/nix/store/bri4plkp5ykp2dvjs1bwzd92z7grp6h7-direnv-2.34.0/bin/direnv" export fish | source
6	22	-> if test "$direnv_fish_mode" != "disable_arrow"...
10	10	--> test "$direnv_fish_mode" != "disable_arrow"
6	6	--> function __direnv_cd_hook --on-variable PWD...
8	21	> __fish_enable_bracketed_paste
13	13	-> printf "\e[?2004h"
7	38	> fish_mode_prompt
14	31	-> fish_default_mode_prompt
3	17	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
10	10	---> test "$fish_key_bindings" = fish_vi_key_bindings
4	4	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
24	7500	> fish_prompt
9	9	-> set -l last_pipestatus $pipestatus
6	6	-> set -lx __fish_last_status $status
37	47	-> set -l normal (set_color normal)
10	10	--> set_color normal
4	4	-> set -q fish_color_status
7	7	-> set -l color_cwd $fish_color_cwd
5	5	-> set -l suffix '>'
2	45	-> if functions -q fish_is_root_user...
8	8	--> functions -q fish_is_root_user
8	35	--> fish_is_root_user
1	15	---> if test "$EUID" = 0 2>/dev/null...
14	14	----> test "$EUID" = 0 2>/dev/null
1	10	---> if contains -- $USER root toor Administrator...
9	9	----> contains -- $USER root toor Administrator
2	2	---> return 1
5	5	-> set -l bold_flag --bold
3	3	-> set -q __fish_prompt_status_generation
2	8	-> if test $__fish_prompt_status_generation = $status_generation...
6	6	--> test $__fish_prompt_status_generation = $status_generation
6	6	-> set __fish_prompt_status_generation $status_generation
40	48	-> set -l status_color (set_color $fish_color_status)
8	8	--> set_color $fish_color_status
37	47	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
10	10	--> set_color $bold_flag $fish_color_status
38	112	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
22	74	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
7	7	---> set -l last_status
3	11	---> if set -q __fish_last_status...
3	3	----> set -q __fish_last_status
5	5	----> set last_status $__fish_last_status
5	5	---> set -l left_brace $argv[1]
5	5	---> set -l right_brace $argv[2]
4	4	---> set -l separator $argv[3]
4	4	---> set -l brace_sep_color $argv[4]
4	4	---> set -l status_color $argv[5]
4	4	---> set -e argv[1 2 3 4 5]
1	3	---> if not set -q argv[1]...
2	2	----> not set -q argv[1]
0	5	---> if not contains $last_status 0 141...
5	5	----> not contains $last_status 0 141
138	7124	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
17	265	--> prompt_login
1	4	---> if not set -q __fish_machine...
3	3	----> not set -q __fish_machine
1	3	---> if set -q __fish_machine[1]...
2	2	----> set -q __fish_machine[1]
5	5	---> set -l color_host $fish_color_host
1	3	---> if set -q SSH_TTY...
2	2	----> set -q SSH_TTY
156	233	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
8	8	----> set_color $fish_color_user
6	6	----> set_color normal
6	6	----> set_color $color_host
15	50	----> prompt_hostname
35	35	-----> string replace -r -- "\..*" "" $hostname
7	7	----> set_color normal
8	8	--> set_color $color_cwd
20	307	--> prompt_pwd
6	6	---> set -l options h/help d/dir-length= D/full-length-dirs=
9	9	---> argparse -n prompt_pwd $options -- $argv
1	4	---> if set -q _flag_help...
3	3	----> set -q _flag_help
2	2	---> set -q argv[1]
4	4	---> set argv $PWD
2	2	---> set -ql _flag_d
3	3	---> set -q fish_prompt_pwd_dir_length
4	4	---> set -l fish_prompt_pwd_dir_length 1
3	3	---> set -l fulldirs 0
2	2	---> set -ql _flag_D
2	2	---> set -q fish_prompt_pwd_full_dirs
3	3	---> set -l fish_prompt_pwd_full_dirs 1
8	243	---> for path in $argv...
35	45	----> set -l realhome (string escape --style=regex -- ~)
10	10	-----> string escape --style=regex -- ~
31	46	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
15	15	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
4	144	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
6	6	-----> test "$fish_prompt_pwd_dir_length" -eq 0
3	3	-----> set -l full
3	83	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
3	3	------> test $fish_prompt_pwd_full_dirs -gt 0
31	69	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
30	38	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
8	8	--------> math $fish_prompt_pwd_full_dirs - 1
4	4	------> set tmp $all[1]
4	4	------> set full $all[2..]
32	48	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
16	16	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
9	6406	--> fish_vcs_prompt
55	6397	---> fish_git_prompt $argv
2	113	----> if not command -sq git...
111	111	-----> not command -sq git
1	5	----> if functions -q __fish_git_prompt_ready...
4	4	-----> functions -q __fish_git_prompt_ready
80	1500	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1420	1420	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
4	4	----> set -l git_dir $repo_info[1]
3	3	----> set -l inside_gitdir $repo_info[2]
3	3	----> set -l bare_repo $repo_info[3]
3	3	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
4	4	----> set -l sha $repo_info[5]
40	2038	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
21	1998	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
4	4	------> set -l inside_gitdir $argv[2]
3	3	------> set -l bare_repo $argv[3]
2	2	------> set -q argv[5]
3	3	------> set -l sha $argv[5]
2	2	------> set -l branch
3	3	------> set -l operation
3	3	------> set -l detached no
2	2	------> set -l bare
3	3	------> set -l step
2	2	------> set -l total
19	48	------> if test -d $git_dir/rebase-merge...
7	7	-------> test -d $git_dir/rebase-merge
3	22	-------> if test -d $git_dir/rebase-apply...
4	4	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
3	3	--------> test -f $git_dir/CHERRY_PICK_HEAD
4	4	--------> test -f $git_dir/REVERT_HEAD
4	4	--------> test -f $git_dir/BISECT_LOG
1	7	------> if test -n "$step" -a -n "$total"...
6	6	-------> test -n "$step" -a -n "$total"
3	1869	------> if test -z "$branch"...
3	3	-------> test -z "$branch"
1	1863	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
69	1862	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1793	1793	---------> command git symbolic-ref HEAD 2>/dev/null
0	6	------> if test true = $inside_gitdir...
6	6	-------> test true = $inside_gitdir
5	5	------> echo $operation
4	4	------> echo $branch
3	3	------> echo $detached
3	3	------> echo $bare
5	5	----> set -l r $rbc[1]
3	3	----> set -l b $rbc[2]
4	4	----> set -l detached $rbc[3]
3	3	----> set -l dirtystate
2	2	----> set -l stagedstate
2	2	----> set -l invalidstate
3	3	----> set -l stashstate
3	3	----> set -l untrackedfiles
3	3	----> set -l c $rbc[4]
2	2	----> set -l p
3	3	----> set -l informative_status
2	2	----> set -q __fish_git_prompt_status_order
0	3	----> if not set -q ___fish_git_prompt_init...
3	3	-----> not set -q ___fish_git_prompt_init
9	9	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
3	3	----> set -l informative
3	3	----> set -l dirty
2	2	----> set -l untracked
651	2301	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1650	1650	-----> read -lz key value
3	18	----> if not set -q dirty[1]...
6	6	-----> not set -q dirty[1]
9	9	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
7	7	----> contains dirtystate $__fish_git_prompt_status_order
3	11	----> if not set -q untracked[1]...
3	3	-----> not set -q untracked[1]
5	5	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
6	6	----> contains untrackedfiles $__fish_git_prompt_status_order
7	56	----> if test true = $inside_worktree...
5	5	-----> test true = $inside_worktree
5	34	-----> if test "$informative" = true...
4	4	------> test "$informative" = true
2	7	------> begin...
5	5	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
4	13	------> if not test "$dirty" = true...
3	3	-------> not test "$dirty" = true
3	3	-------> test "$untracked" = true
3	3	-------> test "$dirty" = true
1	5	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
4	4	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
2	10	-----> if set -q __fish_git_prompt_showupstream...
3	3	------> set -q __fish_git_prompt_showupstream
5	5	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
7	7	----> set -l branch_color $___fish_git_prompt_color_branch
5	5	----> set -l branch_done $___fish_git_prompt_color_branch_done
1	6	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
5	5	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
4	4	----> set -l f ""
15	39	----> for i in $__fish_git_prompt_status_order...
1	6	-----> if test -n "$$i"...
5	5	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
46	55	----> set b (string replace refs/heads/ '' -- $b)
9	9	-----> string replace refs/heads/ '' -- $b
2	18	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
16	16	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
4	24	----> if test -n "$b"...
4	4	-----> test -n "$b"
6	6	-----> set b "$branch_color$b$branch_done"
2	10	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
5	5	------> test -z "$dirtystate$untrackedfiles$stagedstate"
3	3	------> test -n "$___fish_git_prompt_char_cleanstate"
1	4	----> if test -n "$c"...
3	3	-----> test -n "$c"
0	3	----> if test -n "$r"...
3	3	-----> test -n "$r"
0	3	----> if test -n "$p"...
3	3	-----> test -n "$p"
0	3	----> if test -n "$f"...
3	3	-----> test -n "$f"
5	5	----> set -l format $argv[1]
3	10	----> if test -z "$format"...
3	3	-----> test -z "$format"
4	4	-----> set format " (%s)"
21	21	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
14	6721	> fish_right_prompt
6697	6697	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
10	10	-> printf " "
7	402	> fish_title
6	395	-> if not set -q INSIDE_EMACS...
4	4	--> not set -q INSIDE_EMACS
4	4	--> set -l ssh
2	2	--> set -q SSH_TTY
3	379	--> if set -q argv[1]...
3	3	---> set -q argv[1]
29	32	---> set -l command (status current-command)
3	3	----> status current-command
2	10	---> if test "$command" = fish...
5	5	----> test "$command" = fish
3	3	----> set command
47	331	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
5	5	----> string sub -l 20 -- $command
16	279	----> prompt_pwd -d 1 -D 1
5	5	-----> set -l options h/help d/dir-length= D/full-length-dirs=
12	12	-----> argparse -n prompt_pwd $options -- $argv
0	3	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
2	2	-----> set -q argv[1]
4	4	-----> set argv $PWD
2	2	-----> set -ql _flag_d
4	4	-----> set -l fish_prompt_pwd_dir_length $_flag_d
2	2	-----> set -q fish_prompt_pwd_dir_length
3	3	-----> set -l fulldirs 0
1	1	-----> set -ql _flag_D
3	3	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
8	220	-----> for path in $argv...
28	35	------> set -l realhome (string escape --style=regex -- ~)
7	7	-------> string escape --style=regex -- ~
28	47	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
19	19	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
4	130	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
6	6	-------> test "$fish_prompt_pwd_dir_length" -eq 0
3	3	-------> set -l full
3	77	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
3	3	--------> test $fish_prompt_pwd_full_dirs -gt 0
27	63	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
28	36	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
8	8	----------> math $fish_prompt_pwd_full_dirs - 1
4	4	--------> set tmp $all[1]
4	4	--------> set full $all[2..]
28	40	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
12	12	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
20	148	> __fish_winch_handler SIGWINCH
13	128	-> if test "$fish_handle_reflow" = 1 2>/dev/null...
27	27	--> test "$fish_handle_reflow" = 1 2>/dev/null
88	88	--> commandline -f repaint >/dev/null 2>/dev/null
8	35	> fish_mode_prompt
9	27	-> fish_default_mode_prompt
3	18	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
10	10	---> test "$fish_key_bindings" = fish_vi_key_bindings
5	5	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
15	7045	> fish_prompt
9	9	-> set -l last_pipestatus $pipestatus
6	6	-> set -lx __fish_last_status $status
31	42	-> set -l normal (set_color normal)
11	11	--> set_color normal
4	4	-> set -q fish_color_status
6	6	-> set -l color_cwd $fish_color_cwd
5	5	-> set -l suffix '>'
1	40	-> if functions -q fish_is_root_user...
5	5	--> functions -q fish_is_root_user
10	34	--> fish_is_root_user
1	13	---> if test "$EUID" = 0 2>/dev/null...
12	12	----> test "$EUID" = 0 2>/dev/null
1	7	---> if contains -- $USER root toor Administrator...
6	6	----> contains -- $USER root toor Administrator
4	4	---> return 1
4	4	-> set -l bold_flag --bold
3	3	-> set -q __fish_prompt_status_generation
4	11	-> if test $__fish_prompt_status_generation = $status_generation...
4	4	--> test $__fish_prompt_status_generation = $status_generation
3	3	--> set bold_flag
4	4	-> set __fish_prompt_status_generation $status_generation
63	72	-> set -l status_color (set_color $fish_color_status)
9	9	--> set_color $fish_color_status
57	74	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
17	17	--> set_color $bold_flag $fish_color_status
46	153	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
35	107	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
7	7	---> set -l last_status
3	12	---> if set -q __fish_last_status...
4	4	----> set -q __fish_last_status
5	5	----> set last_status $__fish_last_status
8	8	---> set -l left_brace $argv[1]
8	8	---> set -l right_brace $argv[2]
8	8	---> set -l separator $argv[3]
5	5	---> set -l brace_sep_color $argv[4]
4	4	---> set -l status_color $argv[5]
5	5	---> set -e argv[1 2 3 4 5]
1	6	---> if not set -q argv[1]...
5	5	----> not set -q argv[1]
1	9	---> if not contains $last_status 0 141...
8	8	----> not contains $last_status 0 141
106	6597	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
11	224	--> prompt_login
1	5	---> if not set -q __fish_machine...
4	4	----> not set -q __fish_machine
0	3	---> if set -q __fish_machine[1]...
3	3	----> set -q __fish_machine[1]
6	6	---> set -l color_host $fish_color_host
1	4	---> if set -q SSH_TTY...
3	3	----> set -q SSH_TTY
131	195	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
10	10	----> set_color $fish_color_user
6	6	----> set_color normal
7	7	----> set_color $color_host
8	36	----> prompt_hostname
28	28	-----> string replace -r -- "\..*" "" $hostname
5	5	----> set_color normal
7	7	--> set_color $color_cwd
13	289	--> prompt_pwd
12	12	---> set -l options h/help d/dir-length= D/full-length-dirs=
9	9	---> argparse -n prompt_pwd $options -- $argv
1	3	---> if set -q _flag_help...
2	2	----> set -q _flag_help
2	2	---> set -q argv[1]
4	4	---> set argv $PWD
2	2	---> set -ql _flag_d
2	2	---> set -q fish_prompt_pwd_dir_length
2	2	---> set -l fish_prompt_pwd_dir_length 1
3	3	---> set -l fulldirs 0
2	2	---> set -ql _flag_D
2	2	---> set -q fish_prompt_pwd_full_dirs
3	3	---> set -l fish_prompt_pwd_full_dirs 1
8	230	---> for path in $argv...
25	34	----> set -l realhome (string escape --style=regex -- ~)
9	9	-----> string escape --style=regex -- ~
25	37	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
12	12	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
3	151	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
7	7	-----> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-----> set -l full
4	85	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
4	4	------> test $fish_prompt_pwd_full_dirs -gt 0
26	63	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
26	37	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
11	11	--------> math $fish_prompt_pwd_full_dirs - 1
4	4	------> set tmp $all[1]
10	10	------> set full $all[2..]
37	52	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
15	15	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
7	5971	--> fish_vcs_prompt
43	5964	---> fish_git_prompt $argv
1	131	----> if not command -sq git...
130	130	-----> not command -sq git
0	5	----> if functions -q __fish_git_prompt_ready...
5	5	-----> functions -q __fish_git_prompt_ready
54	1657	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1603	1603	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
5	5	----> set -l git_dir $repo_info[1]
3	3	----> set -l inside_gitdir $repo_info[2]
3	3	----> set -l bare_repo $repo_info[3]
4	4	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
3	3	----> set -l sha $repo_info[5]
30	1513	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
20	1483	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
3	3	------> set -l inside_gitdir $argv[2]
3	3	------> set -l bare_repo $argv[3]
2	2	------> set -q argv[5]
4	4	------> set -l sha $argv[5]
3	3	------> set -l branch
3	3	------> set -l operation
3	3	------> set -l detached no
2	2	------> set -l bare
3	3	------> set -l step
2	2	------> set -l total
18	46	------> if test -d $git_dir/rebase-merge...
6	6	-------> test -d $git_dir/rebase-merge
4	22	-------> if test -d $git_dir/rebase-apply...
4	4	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
4	4	--------> test -f $git_dir/CHERRY_PICK_HEAD
3	3	--------> test -f $git_dir/REVERT_HEAD
3	3	--------> test -f $git_dir/BISECT_LOG
1	6	------> if test -n "$step" -a -n "$total"...
5	5	-------> test -n "$step" -a -n "$total"
4	1359	------> if test -z "$branch"...
3	3	-------> test -z "$branch"
1	1352	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
36	1351	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1315	1315	---------> command git symbolic-ref HEAD 2>/dev/null
0	6	------> if test true = $inside_gitdir...
6	6	-------> test true = $inside_gitdir
4	4	------> echo $operation
3	3	------> echo $branch
3	3	------> echo $detached
3	3	------> echo $bare
5	5	----> set -l r $rbc[1]
3	3	----> set -l b $rbc[2]
4	4	----> set -l detached $rbc[3]
3	3	----> set -l dirtystate
3	3	----> set -l stagedstate
2	2	----> set -l invalidstate
2	2	----> set -l stashstate
3	3	----> set -l untrackedfiles
4	4	----> set -l c $rbc[4]
2	2	----> set -l p
2	2	----> set -l informative_status
3	3	----> set -q __fish_git_prompt_status_order
0	3	----> if not set -q ___fish_git_prompt_init...
3	3	-----> not set -q ___fish_git_prompt_init
6	6	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
3	3	----> set -l informative
3	3	----> set -l dirty
2	2	----> set -l untracked
577	2255	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1678	1678	-----> read -lz key value
3	16	----> if not set -q dirty[1]...
5	5	-----> not set -q dirty[1]
8	8	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
7	7	----> contains dirtystate $__fish_git_prompt_status_order
2	10	----> if not set -q untracked[1]...
3	3	-----> not set -q untracked[1]
5	5	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
6	6	----> contains untrackedfiles $__fish_git_prompt_status_order
7	53	----> if test true = $inside_worktree...
4	4	-----> test true = $inside_worktree
4	33	-----> if test "$informative" = true...
4	4	------> test "$informative" = true
2	8	------> begin...
6	6	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
3	12	------> if not test "$dirty" = true...
3	3	-------> not test "$dirty" = true
3	3	-------> test "$untracked" = true
3	3	-------> test "$dirty" = true
0	5	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
5	5	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
2	9	-----> if set -q __fish_git_prompt_showupstream...
3	3	------> set -q __fish_git_prompt_showupstream
4	4	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
6	6	----> set -l branch_color $___fish_git_prompt_color_branch
4	4	----> set -l branch_done $___fish_git_prompt_color_branch_done
1	6	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
5	5	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
4	4	----> set -l f ""
16	38	----> for i in $__fish_git_prompt_status_order...
0	6	-----> if test -n "$$i"...
6	6	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
1	4	-----> if test -n "$$i"...
3	3	------> test -n "$$i"
1	4	-----> if test -n "$$i"...
3	3	------> test -n "$$i"
38	46	----> set b (string replace refs/heads/ '' -- $b)
8	8	-----> string replace refs/heads/ '' -- $b
1	16	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
15	15	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
4	23	----> if test -n "$b"...
4	4	-----> test -n "$b"
6	6	-----> set b "$branch_color$b$branch_done"
2	9	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
4	4	------> test -z "$dirtystate$untrackedfiles$stagedstate"
3	3	------> test -n "$___fish_git_prompt_char_cleanstate"
1	4	----> if test -n "$c"...
3	3	-----> test -n "$c"
0	3	----> if test -n "$r"...
3	3	-----> test -n "$r"
0	3	----> if test -n "$p"...
3	3	-----> test -n "$p"
2	4	----> if test -n "$f"...
2	2	-----> test -n "$f"
5	5	----> set -l format $argv[1]
3	10	----> if test -z "$format"...
3	3	-----> test -z "$format"
4	4	-----> set format " (%s)"
17	17	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
12	6943	> fish_right_prompt
6919	6919	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
12	12	-> printf " "
8	526	> fish_title
8	518	-> if not set -q INSIDE_EMACS...
5	5	--> not set -q INSIDE_EMACS
4	4	--> set -l ssh
3	3	--> set -q SSH_TTY
4	498	--> if set -q argv[1]...
3	3	---> set -q argv[1]
38	43	---> set -l command (status current-command)
5	5	----> status current-command
2	12	---> if test "$command" = fish...
6	6	----> test "$command" = fish
4	4	----> set command
63	436	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
7	7	----> string sub -l 20 -- $command
18	366	----> prompt_pwd -d 1 -D 1
6	6	-----> set -l options h/help d/dir-length= D/full-length-dirs=
17	17	-----> argparse -n prompt_pwd $options -- $argv
0	4	-----> if set -q _flag_help...
4	4	------> set -q _flag_help
2	2	-----> set -q argv[1]
4	4	-----> set argv $PWD
2	2	-----> set -ql _flag_d
5	5	-----> set -l fish_prompt_pwd_dir_length $_flag_d
3	3	-----> set -q fish_prompt_pwd_dir_length
4	4	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
4	4	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
9	293	-----> for path in $argv...
40	57	------> set -l realhome (string escape --style=regex -- ~)
17	17	-------> string escape --style=regex -- ~
38	58	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
20	20	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
5	169	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
7	7	-------> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-------> set -l full
4	101	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	--------> test $fish_prompt_pwd_full_dirs -gt 0
36	83	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
37	47	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
10	10	----------> math $fish_prompt_pwd_full_dirs - 1
5	5	--------> set tmp $all[1]
4	4	--------> set full $all[2..]
37	52	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
15	15	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
20	111	> __direnv_export_eval_2 vim
3	14	-> if set -q __direnv_export_again...
11	11	--> set -q __direnv_export_again
77	77	-> functions --erase __direnv_cd_hook
8	85	> __fish_disable_bracketed_paste vim
77	77	-> printf "\e[?2004l"
8	82	> __fish_disable_focus vim
74	74	-> echo -n \e\[\?1004l
9	679	> fish_title vim
9	670	-> if not set -q INSIDE_EMACS...
7	7	--> not set -q INSIDE_EMACS
5	5	--> set -l ssh
4	4	--> set -q SSH_TTY
3	645	--> if set -q argv[1]...
3	3	---> set -q argv[1]
80	639	---> echo -- $ssh (string sub -l 20 -- $argv[1]) (prompt_pwd -d 1 -D 1)
9	9	----> string sub -l 20 -- $argv[1]
87	550	----> prompt_pwd -d 1 -D 1
10	10	-----> set -l options h/help d/dir-length= D/full-length-dirs=
19	19	-----> argparse -n prompt_pwd $options -- $argv
1	5	-----> if set -q _flag_help...
4	4	------> set -q _flag_help
4	4	-----> set -q argv[1]
7	7	-----> set argv $PWD
3	3	-----> set -ql _flag_d
6	6	-----> set -l fish_prompt_pwd_dir_length $_flag_d
2	2	-----> set -q fish_prompt_pwd_dir_length
3	3	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
5	5	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
11	395	-----> for path in $argv...
43	54	------> set -l realhome (string escape --style=regex -- ~)
11	11	-------> string escape --style=regex -- ~
42	66	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
24	24	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
4	264	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
9	9	-------> test "$fish_prompt_pwd_dir_length" -eq 0
5	5	-------> set -l full
5	187	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	--------> test $fish_prompt_pwd_full_dirs -gt 0
42	164	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
43	122	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
79	79	----------> math $fish_prompt_pwd_full_dirs - 1
7	7	--------> set tmp $all[1]
6	6	--------> set full $all[2..]
40	59	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
19	19	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
33	66814462340	> vim
66814462307	66814462307	-> nvim $argv
17	119	> __fish_enable_focus vim
102	102	-> echo -n \e\[\?1004h
17	2218	> __direnv_export_eval
2173	2173	-> "/nix/store/bri4plkp5ykp2dvjs1bwzd92z7grp6h7-direnv-2.34.0/bin/direnv" export fish | source
6	28	-> if test "$direnv_fish_mode" != "disable_arrow"...
17	17	--> test "$direnv_fish_mode" != "disable_arrow"
5	5	--> function __direnv_cd_hook --on-variable PWD...
8	84	> __fish_enable_bracketed_paste
76	76	-> printf "\e[?2004h"
8	120	> fish_mode_prompt
82	112	-> fish_default_mode_prompt
4	30	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
10	10	---> test "$fish_key_bindings" = fish_vi_key_bindings
16	16	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
16	7729	> fish_prompt
11	11	-> set -l last_pipestatus $pipestatus
7	7	-> set -lx __fish_last_status $status
34	107	-> set -l normal (set_color normal)
73	73	--> set_color normal
4	4	-> set -q fish_color_status
6	6	-> set -l color_cwd $fish_color_cwd
6	6	-> set -l suffix '>'
2	207	-> if functions -q fish_is_root_user...
124	124	--> functions -q fish_is_root_user
5	81	--> fish_is_root_user
1	15	---> if test "$EUID" = 0 2>/dev/null...
14	14	----> test "$EUID" = 0 2>/dev/null
1	58	---> if contains -- $USER root toor Administrator...
57	57	----> contains -- $USER root toor Administrator
3	3	---> return 1
4	4	-> set -l bold_flag --bold
3	3	-> set -q __fish_prompt_status_generation
1	6	-> if test $__fish_prompt_status_generation = $status_generation...
5	5	--> test $__fish_prompt_status_generation = $status_generation
5	5	-> set __fish_prompt_status_generation $status_generation
30	38	-> set -l status_color (set_color $fish_color_status)
8	8	--> set_color $fish_color_status
28	36	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
8	8	--> set_color $bold_flag $fish_color_status
26	150	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
80	124	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
5	5	---> set -l last_status
2	8	---> if set -q __fish_last_status...
3	3	----> set -q __fish_last_status
3	3	----> set last_status $__fish_last_status
4	4	---> set -l left_brace $argv[1]
4	4	---> set -l right_brace $argv[2]
4	4	---> set -l separator $argv[3]
4	4	---> set -l brace_sep_color $argv[4]
4	4	---> set -l status_color $argv[5]
4	4	---> set -e argv[1 2 3 4 5]
1	3	---> if not set -q argv[1]...
2	2	----> not set -q argv[1]
0	4	---> if not contains $last_status 0 141...
4	4	----> not contains $last_status 0 141
100	7123	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
64	304	--> prompt_login
1	5	---> if not set -q __fish_machine...
4	4	----> not set -q __fish_machine
0	3	---> if set -q __fish_machine[1]...
3	3	----> set -q __fish_machine[1]
5	5	---> set -l color_host $fish_color_host
1	3	---> if set -q SSH_TTY...
2	2	----> set -q SSH_TTY
113	224	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
7	7	----> set_color $fish_color_user
5	5	----> set_color normal
6	6	----> set_color $color_host
62	88	----> prompt_hostname
26	26	-----> string replace -r -- "\..*" "" $hostname
5	5	----> set_color normal
6	6	--> set_color $color_cwd
69	387	--> prompt_pwd
6	6	---> set -l options h/help d/dir-length= D/full-length-dirs=
8	8	---> argparse -n prompt_pwd $options -- $argv
0	3	---> if set -q _flag_help...
3	3	----> set -q _flag_help
2	2	---> set -q argv[1]
3	3	---> set argv $PWD
2	2	---> set -ql _flag_d
3	3	---> set -q fish_prompt_pwd_dir_length
3	3	---> set -l fish_prompt_pwd_dir_length 1
3	3	---> set -l fulldirs 0
2	2	---> set -ql _flag_D
2	2	---> set -q fish_prompt_pwd_full_dirs
3	3	---> set -l fish_prompt_pwd_full_dirs 1
8	278	---> for path in $argv...
26	35	----> set -l realhome (string escape --style=regex -- ~)
9	9	-----> string escape --style=regex -- ~
28	44	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
16	16	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
3	191	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
7	7	-----> test "$fish_prompt_pwd_dir_length" -eq 0
3	3	-----> set -l full
4	133	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
4	4	------> test $fish_prompt_pwd_full_dirs -gt 0
26	118	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
30	92	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
62	62	--------> math $fish_prompt_pwd_full_dirs - 1
4	4	------> set tmp $all[1]
3	3	------> set full $all[2..]
28	45	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
17	17	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
63	6326	--> fish_vcs_prompt
98	6263	---> fish_git_prompt $argv
1	132	----> if not command -sq git...
131	131	-----> not command -sq git
1	61	----> if functions -q __fish_git_prompt_ready...
60	60	-----> functions -q __fish_git_prompt_ready
50	1645	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1595	1595	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
4	4	----> set -l git_dir $repo_info[1]
4	4	----> set -l inside_gitdir $repo_info[2]
3	3	----> set -l bare_repo $repo_info[3]
3	3	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
4	4	----> set -l sha $repo_info[5]
31	1672	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
17	1641	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
4	4	------> set -l inside_gitdir $argv[2]
4	4	------> set -l bare_repo $argv[3]
2	2	------> set -q argv[5]
3	3	------> set -l sha $argv[5]
2	2	------> set -l branch
3	3	------> set -l operation
2	2	------> set -l detached no
2	2	------> set -l bare
2	2	------> set -l step
3	3	------> set -l total
18	49	------> if test -d $git_dir/rebase-merge...
8	8	-------> test -d $git_dir/rebase-merge
4	23	-------> if test -d $git_dir/rebase-apply...
4	4	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
4	4	--------> test -f $git_dir/CHERRY_PICK_HEAD
3	3	--------> test -f $git_dir/REVERT_HEAD
4	4	--------> test -f $git_dir/BISECT_LOG
0	6	------> if test -n "$step" -a -n "$total"...
6	6	-------> test -n "$step" -a -n "$total"
5	1517	------> if test -z "$branch"...
2	2	-------> test -z "$branch"
0	1510	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
34	1510	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1476	1476	---------> command git symbolic-ref HEAD 2>/dev/null
0	6	------> if test true = $inside_gitdir...
6	6	-------> test true = $inside_gitdir
4	4	------> echo $operation
4	4	------> echo $branch
3	3	------> echo $detached
3	3	------> echo $bare
4	4	----> set -l r $rbc[1]
3	3	----> set -l b $rbc[2]
3	3	----> set -l detached $rbc[3]
2	2	----> set -l dirtystate
2	2	----> set -l stagedstate
2	2	----> set -l invalidstate
3	3	----> set -l stashstate
3	3	----> set -l untrackedfiles
3	3	----> set -l c $rbc[4]
2	2	----> set -l p
3	3	----> set -l informative_status
2	2	----> set -q __fish_git_prompt_status_order
1	3	----> if not set -q ___fish_git_prompt_init...
2	2	-----> not set -q ___fish_git_prompt_init
7	7	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
3	3	----> set -l informative
2	2	----> set -l dirty
2	2	----> set -l untracked
578	2304	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1726	1726	-----> read -lz key value
4	17	----> if not set -q dirty[1]...
5	5	-----> not set -q dirty[1]
8	8	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
6	6	----> contains dirtystate $__fish_git_prompt_status_order
2	10	----> if not set -q untracked[1]...
3	3	-----> not set -q untracked[1]
5	5	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
5	5	----> contains untrackedfiles $__fish_git_prompt_status_order
8	52	----> if test true = $inside_worktree...
4	4	-----> test true = $inside_worktree
2	32	-----> if test "$informative" = true...
4	4	------> test "$informative" = true
2	7	------> begin...
5	5	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
4	13	------> if not test "$dirty" = true...
3	3	-------> not test "$dirty" = true
3	3	-------> test "$untracked" = true
3	3	-------> test "$dirty" = true
1	6	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
5	5	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
2	8	-----> if set -q __fish_git_prompt_showupstream...
2	2	------> set -q __fish_git_prompt_showupstream
4	4	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
6	6	----> set -l branch_color $___fish_git_prompt_color_branch
5	5	----> set -l branch_done $___fish_git_prompt_color_branch_done
1	5	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
4	4	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
4	4	----> set -l f ""
13	36	----> for i in $__fish_git_prompt_status_order...
2	6	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
1	4	-----> if test -n "$$i"...
3	3	------> test -n "$$i"
0	3	-----> if test -n "$$i"...
3	3	------> test -n "$$i"
2	5	-----> if test -n "$$i"...
3	3	------> test -n "$$i"
38	46	----> set b (string replace refs/heads/ '' -- $b)
8	8	-----> string replace refs/heads/ '' -- $b
2	17	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
15	15	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
3	22	----> if test -n "$b"...
4	4	-----> test -n "$b"
5	5	-----> set b "$branch_color$b$branch_done"
1	10	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
5	5	------> test -z "$dirtystate$untrackedfiles$stagedstate"
4	4	------> test -n "$___fish_git_prompt_char_cleanstate"
0	3	----> if test -n "$c"...
3	3	-----> test -n "$c"
1	4	----> if test -n "$r"...
3	3	-----> test -n "$r"
1	4	----> if test -n "$p"...
3	3	-----> test -n "$p"
0	3	----> if test -n "$f"...
3	3	-----> test -n "$f"
5	5	----> set -l format $argv[1]
2	10	----> if test -z "$format"...
4	4	-----> test -z "$format"
4	4	-----> set format " (%s)"
16	16	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
10	6717	> fish_right_prompt
6697	6697	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
10	10	-> printf " "
7	402	> fish_title
7	395	-> if not set -q INSIDE_EMACS...
5	5	--> not set -q INSIDE_EMACS
4	4	--> set -l ssh
2	2	--> set -q SSH_TTY
4	377	--> if set -q argv[1]...
2	2	---> set -q argv[1]
29	32	---> set -l command (status current-command)
3	3	----> status current-command
2	10	---> if test "$command" = fish...
5	5	----> test "$command" = fish
3	3	----> set command
50	329	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
5	5	----> string sub -l 20 -- $command
14	274	----> prompt_pwd -d 1 -D 1
5	5	-----> set -l options h/help d/dir-length= D/full-length-dirs=
12	12	-----> argparse -n prompt_pwd $options -- $argv
1	4	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
2	2	-----> set -q argv[1]
3	3	-----> set argv $PWD
2	2	-----> set -ql _flag_d
4	4	-----> set -l fish_prompt_pwd_dir_length $_flag_d
2	2	-----> set -q fish_prompt_pwd_dir_length
2	2	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
4	4	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
8	216	-----> for path in $argv...
27	34	------> set -l realhome (string escape --style=regex -- ~)
7	7	-------> string escape --style=regex -- ~
28	41	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
13	13	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
5	133	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
5	5	-------> test "$fish_prompt_pwd_dir_length" -eq 0
3	3	-------> set -l full
2	78	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
4	4	--------> test $fish_prompt_pwd_full_dirs -gt 0
27	63	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
28	36	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
8	8	----------> math $fish_prompt_pwd_full_dirs - 1
5	5	--------> set tmp $all[1]
4	4	--------> set full $all[2..]
29	42	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
13	13	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
25	52	> __direnv_export_eval_2 'g d'
3	16	-> if set -q __direnv_export_again...
13	13	--> set -q __direnv_export_again
11	11	-> functions --erase __direnv_cd_hook
8	18	> __fish_disable_bracketed_paste 'g d'
10	10	-> printf "\e[?2004l"
6	14	> __fish_disable_focus 'g d'
8	8	-> echo -n \e\[\?1004l
10	536	> fish_title g\ d
10	526	-> if not set -q INSIDE_EMACS...
7	7	--> not set -q INSIDE_EMACS
6	6	--> set -l ssh
3	3	--> set -q SSH_TTY
2	500	--> if set -q argv[1]...
4	4	---> set -q argv[1]
85	494	---> echo -- $ssh (string sub -l 20 -- $argv[1]) (prompt_pwd -d 1 -D 1)
10	10	----> string sub -l 20 -- $argv[1]
21	399	----> prompt_pwd -d 1 -D 1
8	8	-----> set -l options h/help d/dir-length= D/full-length-dirs=
20	20	-----> argparse -n prompt_pwd $options -- $argv
1	6	-----> if set -q _flag_help...
5	5	------> set -q _flag_help
3	3	-----> set -q argv[1]
6	6	-----> set argv $PWD
3	3	-----> set -ql _flag_d
7	7	-----> set -l fish_prompt_pwd_dir_length $_flag_d
3	3	-----> set -q fish_prompt_pwd_dir_length
4	4	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
4	4	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
11	310	-----> for path in $argv...
41	51	------> set -l realhome (string escape --style=regex -- ~)
10	10	-------> string escape --style=regex -- ~
39	63	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
24	24	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
5	185	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
10	10	-------> test "$fish_prompt_pwd_dir_length" -eq 0
5	5	-------> set -l full
4	108	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	--------> test $fish_prompt_pwd_full_dirs -gt 0
36	87	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
37	51	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
14	14	----------> math $fish_prompt_pwd_full_dirs - 1
7	7	--------> set tmp $all[1]
5	5	--------> set full $all[2..]
37	57	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
20	20	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
48	4741	> g d
4693	4693	-> git $argv
20	54	> __fish_enable_focus 'g d'
34	34	-> echo -n \e\[\?1004h
29	2078	> __direnv_export_eval
2015	2015	-> "/nix/store/bri4plkp5ykp2dvjs1bwzd92z7grp6h7-direnv-2.34.0/bin/direnv" export fish | source
6	34	-> if test "$direnv_fish_mode" != "disable_arrow"...
14	14	--> test "$direnv_fish_mode" != "disable_arrow"
14	14	--> function __direnv_cd_hook --on-variable PWD...
11	23	> __fish_enable_bracketed_paste
12	12	-> printf "\e[?2004h"
16	64	> fish_mode_prompt
18	48	-> fish_default_mode_prompt
3	30	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
20	20	---> test "$fish_key_bindings" = fish_vi_key_bindings
7	7	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
23	7757	> fish_prompt
10	10	-> set -l last_pipestatus $pipestatus
10	10	-> set -lx __fish_last_status $status
40	50	-> set -l normal (set_color normal)
10	10	--> set_color normal
4	4	-> set -q fish_color_status
13	13	-> set -l color_cwd $fish_color_cwd
8	8	-> set -l suffix '>'
2	94	-> if functions -q fish_is_root_user...
8	8	--> functions -q fish_is_root_user
10	84	--> fish_is_root_user
4	51	---> if test "$EUID" = 0 2>/dev/null...
47	47	----> test "$EUID" = 0 2>/dev/null
1	20	---> if contains -- $USER root toor Administrator...
19	19	----> contains -- $USER root toor Administrator
3	3	---> return 1
6	6	-> set -l bold_flag --bold
3	3	-> set -q __fish_prompt_status_generation
1	8	-> if test $__fish_prompt_status_generation = $status_generation...
7	7	--> test $__fish_prompt_status_generation = $status_generation
9	9	-> set __fish_prompt_status_generation $status_generation
50	65	-> set -l status_color (set_color $fish_color_status)
15	15	--> set_color $fish_color_status
45	65	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
20	20	--> set_color $bold_flag $fish_color_status
47	151	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
36	104	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
10	10	---> set -l last_status
4	12	---> if set -q __fish_last_status...
3	3	----> set -q __fish_last_status
5	5	----> set last_status $__fish_last_status
6	6	---> set -l left_brace $argv[1]
11	11	---> set -l right_brace $argv[2]
5	5	---> set -l separator $argv[3]
5	5	---> set -l brace_sep_color $argv[4]
5	5	---> set -l status_color $argv[5]
5	5	---> set -e argv[1 2 3 4 5]
0	3	---> if not set -q argv[1]...
3	3	----> not set -q argv[1]
0	6	---> if not contains $last_status 0 141...
6	6	----> not contains $last_status 0 141
129	7238	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
21	292	--> prompt_login
1	6	---> if not set -q __fish_machine...
5	5	----> not set -q __fish_machine
0	3	---> if set -q __fish_machine[1]...
3	3	----> set -q __fish_machine[1]
6	6	---> set -l color_host $fish_color_host
0	3	---> if set -q SSH_TTY...
3	3	----> set -q SSH_TTY
173	253	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
10	10	----> set_color $fish_color_user
8	8	----> set_color normal
9	9	----> set_color $color_host
21	44	----> prompt_hostname
23	23	-----> string replace -r -- "\..*" "" $hostname
9	9	----> set_color normal
8	8	--> set_color $color_cwd
18	319	--> prompt_pwd
5	5	---> set -l options h/help d/dir-length= D/full-length-dirs=
8	8	---> argparse -n prompt_pwd $options -- $argv
0	3	---> if set -q _flag_help...
3	3	----> set -q _flag_help
2	2	---> set -q argv[1]
4	4	---> set argv $PWD
2	2	---> set -ql _flag_d
2	2	---> set -q fish_prompt_pwd_dir_length
3	3	---> set -l fish_prompt_pwd_dir_length 1
2	2	---> set -l fulldirs 0
1	1	---> set -ql _flag_D
2	2	---> set -q fish_prompt_pwd_full_dirs
3	3	---> set -l fish_prompt_pwd_full_dirs 1
9	264	---> for path in $argv...
34	45	----> set -l realhome (string escape --style=regex -- ~)
11	11	-----> string escape --style=regex -- ~
32	47	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
15	15	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
4	163	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
6	6	-----> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-----> set -l full
4	100	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
4	4	------> test $fish_prompt_pwd_full_dirs -gt 0
35	84	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
39	49	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
10	10	--------> math $fish_prompt_pwd_full_dirs - 1
5	5	------> set tmp $all[1]
3	3	------> set full $all[2..]
32	49	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
17	17	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
7	6490	--> fish_vcs_prompt
61	6483	---> fish_git_prompt $argv
2	109	----> if not command -sq git...
107	107	-----> not command -sq git
1	5	----> if functions -q __fish_git_prompt_ready...
4	4	-----> functions -q __fish_git_prompt_ready
58	1632	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1574	1574	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
7	7	----> set -l git_dir $repo_info[1]
4	4	----> set -l inside_gitdir $repo_info[2]
3	3	----> set -l bare_repo $repo_info[3]
4	4	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
4	4	----> set -l sha $repo_info[5]
40	1855	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
23	1815	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
4	4	------> set -l inside_gitdir $argv[2]
4	4	------> set -l bare_repo $argv[3]
1	1	------> set -q argv[5]
4	4	------> set -l sha $argv[5]
2	2	------> set -l branch
2	2	------> set -l operation
2	2	------> set -l detached no
2	2	------> set -l bare
2	2	------> set -l step
3	3	------> set -l total
19	53	------> if test -d $git_dir/rebase-merge...
12	12	-------> test -d $git_dir/rebase-merge
4	22	-------> if test -d $git_dir/rebase-apply...
4	4	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
4	4	--------> test -f $git_dir/CHERRY_PICK_HEAD
3	3	--------> test -f $git_dir/REVERT_HEAD
3	3	--------> test -f $git_dir/BISECT_LOG
1	6	------> if test -n "$step" -a -n "$total"...
5	5	-------> test -n "$step" -a -n "$total"
5	1676	------> if test -z "$branch"...
3	3	-------> test -z "$branch"
1	1668	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
47	1667	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1620	1620	---------> command git symbolic-ref HEAD 2>/dev/null
1	7	------> if test true = $inside_gitdir...
6	6	-------> test true = $inside_gitdir
6	6	------> echo $operation
5	5	------> echo $branch
4	4	------> echo $detached
4	4	------> echo $bare
6	6	----> set -l r $rbc[1]
5	5	----> set -l b $rbc[2]
5	5	----> set -l detached $rbc[3]
3	3	----> set -l dirtystate
3	3	----> set -l stagedstate
2	2	----> set -l invalidstate
3	3	----> set -l stashstate
4	4	----> set -l untrackedfiles
4	4	----> set -l c $rbc[4]
2	2	----> set -l p
3	3	----> set -l informative_status
3	3	----> set -q __fish_git_prompt_status_order
1	4	----> if not set -q ___fish_git_prompt_init...
3	3	-----> not set -q ___fish_git_prompt_init
15	15	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
3	3	----> set -l informative
3	3	----> set -l dirty
3	3	----> set -l untracked
642	2414	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1772	1772	-----> read -lz key value
3	18	----> if not set -q dirty[1]...
6	6	-----> not set -q dirty[1]
9	9	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
7	7	----> contains dirtystate $__fish_git_prompt_status_order
2	11	----> if not set -q untracked[1]...
3	3	-----> not set -q untracked[1]
6	6	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
6	6	----> contains untrackedfiles $__fish_git_prompt_status_order
6	54	----> if test true = $inside_worktree...
5	5	-----> test true = $inside_worktree
4	34	-----> if test "$informative" = true...
5	5	------> test "$informative" = true
2	7	------> begin...
5	5	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
0	12	------> if not test "$dirty" = true...
4	4	-------> not test "$dirty" = true
4	4	-------> test "$untracked" = true
4	4	-------> test "$dirty" = true
1	6	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
5	5	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
1	9	-----> if set -q __fish_git_prompt_showupstream...
3	3	------> set -q __fish_git_prompt_showupstream
5	5	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
6	6	----> set -l branch_color $___fish_git_prompt_color_branch
5	5	----> set -l branch_done $___fish_git_prompt_color_branch_done
1	6	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
5	5	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
4	4	----> set -l f ""
15	38	----> for i in $__fish_git_prompt_status_order...
0	6	-----> if test -n "$$i"...
6	6	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
45	54	----> set b (string replace refs/heads/ '' -- $b)
9	9	-----> string replace refs/heads/ '' -- $b
1	17	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
16	16	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
5	25	----> if test -n "$b"...
4	4	-----> test -n "$b"
6	6	-----> set b "$branch_color$b$branch_done"
2	10	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
5	5	------> test -z "$dirtystate$untrackedfiles$stagedstate"
3	3	------> test -n "$___fish_git_prompt_char_cleanstate"
1	4	----> if test -n "$c"...
3	3	-----> test -n "$c"
1	4	----> if test -n "$r"...
3	3	-----> test -n "$r"
1	4	----> if test -n "$p"...
3	3	-----> test -n "$p"
0	3	----> if test -n "$f"...
3	3	-----> test -n "$f"
5	5	----> set -l format $argv[1]
2	10	----> if test -z "$format"...
4	4	-----> test -z "$format"
4	4	-----> set format " (%s)"
25	25	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
17	6930	> fish_right_prompt
6902	6902	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
11	11	-> printf " "
12	476	> fish_title
6	464	-> if not set -q INSIDE_EMACS...
6	6	--> not set -q INSIDE_EMACS
5	5	--> set -l ssh
3	3	--> set -q SSH_TTY
5	444	--> if set -q argv[1]...
3	3	---> set -q argv[1]
38	44	---> set -l command (status current-command)
6	6	----> status current-command
2	10	---> if test "$command" = fish...
5	5	----> test "$command" = fish
3	3	----> set command
56	382	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
6	6	----> string sub -l 20 -- $command
17	320	----> prompt_pwd -d 1 -D 1
6	6	-----> set -l options h/help d/dir-length= D/full-length-dirs=
13	13	-----> argparse -n prompt_pwd $options -- $argv
1	3	-----> if set -q _flag_help...
2	2	------> set -q _flag_help
2	2	-----> set -q argv[1]
4	4	-----> set argv $PWD
2	2	-----> set -ql _flag_d
4	4	-----> set -l fish_prompt_pwd_dir_length $_flag_d
2	2	-----> set -q fish_prompt_pwd_dir_length
3	3	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
3	3	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
8	257	-----> for path in $argv...
32	41	------> set -l realhome (string escape --style=regex -- ~)
9	9	-------> string escape --style=regex -- ~
34	55	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
21	21	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
4	153	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
6	6	-------> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-------> set -l full
5	91	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
4	4	--------> test $fish_prompt_pwd_full_dirs -gt 0
31	74	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
33	43	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
10	10	----------> math $fish_prompt_pwd_full_dirs - 1
5	5	--------> set tmp $all[1]
3	3	--------> set full $all[2..]
34	48	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
14	14	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
31	2162	> __direnv_export_eval
2095	2095	-> "/nix/store/bri4plkp5ykp2dvjs1bwzd92z7grp6h7-direnv-2.34.0/bin/direnv" export fish | source
11	36	-> if test "$direnv_fish_mode" != "disable_arrow"...
18	18	--> test "$direnv_fish_mode" != "disable_arrow"
7	7	--> function __direnv_cd_hook --on-variable PWD...
9	21	> __fish_enable_bracketed_paste
12	12	-> printf "\e[?2004h"
9	44	> fish_mode_prompt
16	35	-> fish_default_mode_prompt
2	19	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
11	11	---> test "$fish_key_bindings" = fish_vi_key_bindings
6	6	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
20	6907	> fish_prompt
11	11	-> set -l last_pipestatus $pipestatus
7	7	-> set -lx __fish_last_status $status
38	47	-> set -l normal (set_color normal)
9	9	--> set_color normal
4	4	-> set -q fish_color_status
6	6	-> set -l color_cwd $fish_color_cwd
6	6	-> set -l suffix '>'
2	49	-> if functions -q fish_is_root_user...
7	7	--> functions -q fish_is_root_user
8	40	--> fish_is_root_user
1	21	---> if test "$EUID" = 0 2>/dev/null...
20	20	----> test "$EUID" = 0 2>/dev/null
1	8	---> if contains -- $USER root toor Administrator...
7	7	----> contains -- $USER root toor Administrator
3	3	---> return 1
5	5	-> set -l bold_flag --bold
4	4	-> set -q __fish_prompt_status_generation
2	13	-> if test $__fish_prompt_status_generation = $status_generation...
6	6	--> test $__fish_prompt_status_generation = $status_generation
5	5	--> set bold_flag
5	5	-> set __fish_prompt_status_generation $status_generation
39	48	-> set -l status_color (set_color $fish_color_status)
9	9	--> set_color $fish_color_status
39	48	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
9	9	--> set_color $bold_flag $fish_color_status
34	113	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
21	79	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
6	6	---> set -l last_status
3	10	---> if set -q __fish_last_status...
3	3	----> set -q __fish_last_status
4	4	----> set last_status $__fish_last_status
6	6	---> set -l left_brace $argv[1]
6	6	---> set -l right_brace $argv[2]
4	4	---> set -l separator $argv[3]
5	5	---> set -l brace_sep_color $argv[4]
5	5	---> set -l status_color $argv[5]
6	6	---> set -e argv[1 2 3 4 5]
1	4	---> if not set -q argv[1]...
3	3	----> not set -q argv[1]
1	6	---> if not contains $last_status 0 141...
5	5	----> not contains $last_status 0 141
125	6521	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
13	271	--> prompt_login
1	5	---> if not set -q __fish_machine...
4	4	----> not set -q __fish_machine
0	3	---> if set -q __fish_machine[1]...
3	3	----> set -q __fish_machine[1]
6	6	---> set -l color_host $fish_color_host
1	4	---> if set -q SSH_TTY...
3	3	----> set -q SSH_TTY
177	240	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
8	8	----> set_color $fish_color_user
7	7	----> set_color normal
6	6	----> set_color $color_host
9	35	----> prompt_hostname
26	26	-----> string replace -r -- "\..*" "" $hostname
7	7	----> set_color normal
7	7	--> set_color $color_cwd
13	294	--> prompt_pwd
6	6	---> set -l options h/help d/dir-length= D/full-length-dirs=
10	10	---> argparse -n prompt_pwd $options -- $argv
1	4	---> if set -q _flag_help...
3	3	----> set -q _flag_help
2	2	---> set -q argv[1]
4	4	---> set argv $PWD
2	2	---> set -ql _flag_d
2	2	---> set -q fish_prompt_pwd_dir_length
3	3	---> set -l fish_prompt_pwd_dir_length 1
3	3	---> set -l fulldirs 0
2	2	---> set -ql _flag_D
2	2	---> set -q fish_prompt_pwd_full_dirs
3	3	---> set -l fish_prompt_pwd_full_dirs 1
7	238	---> for path in $argv...
30	38	----> set -l realhome (string escape --style=regex -- ~)
8	8	-----> string escape --style=regex -- ~
32	45	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
13	13	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
5	148	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
6	6	-----> test "$fish_prompt_pwd_dir_length" -eq 0
3	3	-----> set -l full
2	87	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
4	4	------> test $fish_prompt_pwd_full_dirs -gt 0
32	73	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
31	41	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
10	10	--------> math $fish_prompt_pwd_full_dirs - 1
4	4	------> set tmp $all[1]
4	4	------> set full $all[2..]
33	47	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
14	14	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
8	5824	--> fish_vcs_prompt
44	5816	---> fish_git_prompt $argv
1	135	----> if not command -sq git...
134	134	-----> not command -sq git
0	5	----> if functions -q __fish_git_prompt_ready...
5	5	-----> functions -q __fish_git_prompt_ready
59	1495	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1436	1436	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
4	4	----> set -l git_dir $repo_info[1]
4	4	----> set -l inside_gitdir $repo_info[2]
4	4	----> set -l bare_repo $repo_info[3]
4	4	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
3	3	----> set -l sha $repo_info[5]
37	1488	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
22	1451	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
3	3	------> set -l inside_gitdir $argv[2]
4	4	------> set -l bare_repo $argv[3]
2	2	------> set -q argv[5]
3	3	------> set -l sha $argv[5]
2	2	------> set -l branch
2	2	------> set -l operation
2	2	------> set -l detached no
2	2	------> set -l bare
2	2	------> set -l step
3	3	------> set -l total
19	47	------> if test -d $git_dir/rebase-merge...
6	6	-------> test -d $git_dir/rebase-merge
4	22	-------> if test -d $git_dir/rebase-apply...
4	4	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
4	4	--------> test -f $git_dir/CHERRY_PICK_HEAD
3	3	--------> test -f $git_dir/REVERT_HEAD
3	3	--------> test -f $git_dir/BISECT_LOG
1	6	------> if test -n "$step" -a -n "$total"...
5	5	-------> test -n "$step" -a -n "$total"
4	1325	------> if test -z "$branch"...
3	3	-------> test -z "$branch"
1	1318	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
45	1317	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1272	1272	---------> command git symbolic-ref HEAD 2>/dev/null
1	6	------> if test true = $inside_gitdir...
5	5	-------> test true = $inside_gitdir
5	5	------> echo $operation
4	4	------> echo $branch
3	3	------> echo $detached
3	3	------> echo $bare
5	5	----> set -l r $rbc[1]
4	4	----> set -l b $rbc[2]
4	4	----> set -l detached $rbc[3]
3	3	----> set -l dirtystate
3	3	----> set -l stagedstate
2	2	----> set -l invalidstate
2	2	----> set -l stashstate
3	3	----> set -l untrackedfiles
4	4	----> set -l c $rbc[4]
2	2	----> set -l p
3	3	----> set -l informative_status
2	2	----> set -q __fish_git_prompt_status_order
0	3	----> if not set -q ___fish_git_prompt_init...
3	3	-----> not set -q ___fish_git_prompt_init
6	6	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
2	2	----> set -l informative
2	2	----> set -l dirty
2	2	----> set -l untracked
541	2302	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1761	1761	-----> read -lz key value
2	13	----> if not set -q dirty[1]...
4	4	-----> not set -q dirty[1]
7	7	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
6	6	----> contains dirtystate $__fish_git_prompt_status_order
3	9	----> if not set -q untracked[1]...
2	2	-----> not set -q untracked[1]
4	4	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
4	4	----> contains untrackedfiles $__fish_git_prompt_status_order
6	43	----> if test true = $inside_worktree...
4	4	-----> test true = $inside_worktree
3	26	-----> if test "$informative" = true...
3	3	------> test "$informative" = true
0	5	------> begin...
5	5	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
3	10	------> if not test "$dirty" = true...
3	3	-------> not test "$dirty" = true
2	2	-------> test "$untracked" = true
2	2	-------> test "$dirty" = true
2	5	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
3	3	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
0	7	-----> if set -q __fish_git_prompt_showupstream...
3	3	------> set -q __fish_git_prompt_showupstream
4	4	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
5	5	----> set -l branch_color $___fish_git_prompt_color_branch
4	4	----> set -l branch_done $___fish_git_prompt_color_branch_done
1	4	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
3	3	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
3	3	----> set -l f ""
15	38	----> for i in $__fish_git_prompt_status_order...
1	6	-----> if test -n "$$i"...
5	5	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
43	52	----> set b (string replace refs/heads/ '' -- $b)
9	9	-----> string replace refs/heads/ '' -- $b
1	13	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
12	12	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
4	24	----> if test -n "$b"...
4	4	-----> test -n "$b"
6	6	-----> set b "$branch_color$b$branch_done"
1	10	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
5	5	------> test -z "$dirtystate$untrackedfiles$stagedstate"
4	4	------> test -n "$___fish_git_prompt_char_cleanstate"
1	4	----> if test -n "$c"...
3	3	-----> test -n "$c"
1	4	----> if test -n "$r"...
3	3	-----> test -n "$r"
1	4	----> if test -n "$p"...
3	3	-----> test -n "$p"
1	4	----> if test -n "$f"...
3	3	-----> test -n "$f"
6	6	----> set -l format $argv[1]
3	11	----> if test -z "$format"...
4	4	-----> test -z "$format"
4	4	-----> set format " (%s)"
17	17	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
11	7551	> fish_right_prompt
7530	7530	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
10	10	-> printf " "
7	525	> fish_title
8	518	-> if not set -q INSIDE_EMACS...
6	6	--> not set -q INSIDE_EMACS
4	4	--> set -l ssh
2	2	--> set -q SSH_TTY
3	498	--> if set -q argv[1]...
3	3	---> set -q argv[1]
37	41	---> set -l command (status current-command)
4	4	----> status current-command
2	11	---> if test "$command" = fish...
6	6	----> test "$command" = fish
3	3	----> set command
68	440	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
6	6	----> string sub -l 20 -- $command
18	366	----> prompt_pwd -d 1 -D 1
6	6	-----> set -l options h/help d/dir-length= D/full-length-dirs=
17	17	-----> argparse -n prompt_pwd $options -- $argv
1	4	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
3	3	-----> set -q argv[1]
5	5	-----> set argv $PWD
3	3	-----> set -ql _flag_d
5	5	-----> set -l fish_prompt_pwd_dir_length $_flag_d
2	2	-----> set -q fish_prompt_pwd_dir_length
4	4	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
5	5	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
8	290	-----> for path in $argv...
38	46	------> set -l realhome (string escape --style=regex -- ~)
8	8	-------> string escape --style=regex -- ~
39	62	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
23	23	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
6	174	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
7	7	-------> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-------> set -l full
6	103	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
4	4	--------> test $fish_prompt_pwd_full_dirs -gt 0
35	83	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
38	48	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
10	10	----------> math $fish_prompt_pwd_full_dirs - 1
6	6	--------> set tmp $all[1]
4	4	--------> set full $all[2..]
38	54	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
16	16	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
31	2296	> __direnv_export_eval
2232	2232	-> "/nix/store/bri4plkp5ykp2dvjs1bwzd92z7grp6h7-direnv-2.34.0/bin/direnv" export fish | source
9	33	-> if test "$direnv_fish_mode" != "disable_arrow"...
17	17	--> test "$direnv_fish_mode" != "disable_arrow"
7	7	--> function __direnv_cd_hook --on-variable PWD...
9	20	> __fish_enable_bracketed_paste
11	11	-> printf "\e[?2004h"
6	35	> fish_mode_prompt
11	29	-> fish_default_mode_prompt
2	18	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
10	10	---> test "$fish_key_bindings" = fish_vi_key_bindings
6	6	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
18	6989	> fish_prompt
16	16	-> set -l last_pipestatus $pipestatus
7	7	-> set -lx __fish_last_status $status
37	45	-> set -l normal (set_color normal)
8	8	--> set_color normal
4	4	-> set -q fish_color_status
6	6	-> set -l color_cwd $fish_color_cwd
5	5	-> set -l suffix '>'
2	44	-> if functions -q fish_is_root_user...
6	6	--> functions -q fish_is_root_user
7	36	--> fish_is_root_user
2	18	---> if test "$EUID" = 0 2>/dev/null...
16	16	----> test "$EUID" = 0 2>/dev/null
2	9	---> if contains -- $USER root toor Administrator...
7	7	----> contains -- $USER root toor Administrator
2	2	---> return 1
5	5	-> set -l bold_flag --bold
4	4	-> set -q __fish_prompt_status_generation
4	13	-> if test $__fish_prompt_status_generation = $status_generation...
5	5	--> test $__fish_prompt_status_generation = $status_generation
4	4	--> set bold_flag
5	5	-> set __fish_prompt_status_generation $status_generation
36	45	-> set -l status_color (set_color $fish_color_status)
9	9	--> set_color $fish_color_status
39	48	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
9	9	--> set_color $bold_flag $fish_color_status
35	110	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
21	75	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
5	5	---> set -l last_status
2	10	---> if set -q __fish_last_status...
3	3	----> set -q __fish_last_status
5	5	----> set last_status $__fish_last_status
5	5	---> set -l left_brace $argv[1]
5	5	---> set -l right_brace $argv[2]
5	5	---> set -l separator $argv[3]
5	5	---> set -l brace_sep_color $argv[4]
4	4	---> set -l status_color $argv[5]
5	5	---> set -e argv[1 2 3 4 5]
1	4	---> if not set -q argv[1]...
3	3	----> not set -q argv[1]
2	6	---> if not contains $last_status 0 141...
4	4	----> not contains $last_status 0 141
120	6614	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
13	278	--> prompt_login
1	5	---> if not set -q __fish_machine...
4	4	----> not set -q __fish_machine
2	4	---> if set -q __fish_machine[1]...
2	2	----> set -q __fish_machine[1]
5	5	---> set -l color_host $fish_color_host
2	4	---> if set -q SSH_TTY...
2	2	----> set -q SSH_TTY
152	247	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
8	8	----> set_color $fish_color_user
6	6	----> set_color normal
6	6	----> set_color $color_host
11	68	----> prompt_hostname
57	57	-----> string replace -r -- "\..*" "" $hostname
7	7	----> set_color normal
8	8	--> set_color $color_cwd
15	299	--> prompt_pwd
6	6	---> set -l options h/help d/dir-length= D/full-length-dirs=
9	9	---> argparse -n prompt_pwd $options -- $argv
1	4	---> if set -q _flag_help...
3	3	----> set -q _flag_help
2	2	---> set -q argv[1]
3	3	---> set argv $PWD
2	2	---> set -ql _flag_d
3	3	---> set -q fish_prompt_pwd_dir_length
3	3	---> set -l fish_prompt_pwd_dir_length 1
3	3	---> set -l fulldirs 0
2	2	---> set -ql _flag_D
3	3	---> set -q fish_prompt_pwd_full_dirs
3	3	---> set -l fish_prompt_pwd_full_dirs 1
9	241	---> for path in $argv...
31	38	----> set -l realhome (string escape --style=regex -- ~)
7	7	-----> string escape --style=regex -- ~
31	44	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
13	13	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
3	150	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
7	7	-----> test "$fish_prompt_pwd_dir_length" -eq 0
3	3	-----> set -l full
5	90	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
3	3	------> test $fish_prompt_pwd_full_dirs -gt 0
30	73	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
32	43	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
11	11	--------> math $fish_prompt_pwd_full_dirs - 1
5	5	------> set tmp $all[1]
4	4	------> set full $all[2..]
32	47	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
15	15	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
7	5909	--> fish_vcs_prompt
44	5902	---> fish_git_prompt $argv
2	137	----> if not command -sq git...
135	135	-----> not command -sq git
1	6	----> if functions -q __fish_git_prompt_ready...
5	5	-----> functions -q __fish_git_prompt_ready
54	1586	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1532	1532	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
5	5	----> set -l git_dir $repo_info[1]
4	4	----> set -l inside_gitdir $repo_info[2]
4	4	----> set -l bare_repo $repo_info[3]
3	3	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
3	3	----> set -l sha $repo_info[5]
36	1483	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
24	1447	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
3	3	------> set -l inside_gitdir $argv[2]
4	4	------> set -l bare_repo $argv[3]
2	2	------> set -q argv[5]
3	3	------> set -l sha $argv[5]
2	2	------> set -l branch
2	2	------> set -l operation
2	2	------> set -l detached no
2	2	------> set -l bare
3	3	------> set -l step
2	2	------> set -l total
19	47	------> if test -d $git_dir/rebase-merge...
6	6	-------> test -d $git_dir/rebase-merge
1	22	-------> if test -d $git_dir/rebase-apply...
5	5	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
4	4	--------> test -f $git_dir/CHERRY_PICK_HEAD
4	4	--------> test -f $git_dir/REVERT_HEAD
4	4	--------> test -f $git_dir/BISECT_LOG
0	6	------> if test -n "$step" -a -n "$total"...
6	6	-------> test -n "$step" -a -n "$total"
4	1319	------> if test -z "$branch"...
2	2	-------> test -z "$branch"
3	1313	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
44	1310	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1266	1266	---------> command git symbolic-ref HEAD 2>/dev/null
1	7	------> if test true = $inside_gitdir...
6	6	-------> test true = $inside_gitdir
5	5	------> echo $operation
4	4	------> echo $branch
3	3	------> echo $detached
2	2	------> echo $bare
5	5	----> set -l r $rbc[1]
3	3	----> set -l b $rbc[2]
4	4	----> set -l detached $rbc[3]
3	3	----> set -l dirtystate
3	3	----> set -l stagedstate
3	3	----> set -l invalidstate
2	2	----> set -l stashstate
2	2	----> set -l untrackedfiles
3	3	----> set -l c $rbc[4]
2	2	----> set -l p
3	3	----> set -l informative_status
2	2	----> set -q __fish_git_prompt_status_order
0	3	----> if not set -q ___fish_git_prompt_init...
3	3	-----> not set -q ___fish_git_prompt_init
6	6	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
2	2	----> set -l informative
2	2	----> set -l dirty
2	2	----> set -l untracked
541	2263	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1722	1722	-----> read -lz key value
4	19	----> if not set -q dirty[1]...
6	6	-----> not set -q dirty[1]
9	9	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
8	8	----> contains dirtystate $__fish_git_prompt_status_order
3	11	----> if not set -q untracked[1]...
3	3	-----> not set -q untracked[1]
5	5	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
6	6	----> contains untrackedfiles $__fish_git_prompt_status_order
6	55	----> if test true = $inside_worktree...
5	5	-----> test true = $inside_worktree
5	35	-----> if test "$informative" = true...
5	5	------> test "$informative" = true
2	7	------> begin...
5	5	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
3	13	------> if not test "$dirty" = true...
4	4	-------> not test "$dirty" = true
3	3	-------> test "$untracked" = true
3	3	-------> test "$dirty" = true
0	5	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
5	5	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
1	9	-----> if set -q __fish_git_prompt_showupstream...
3	3	------> set -q __fish_git_prompt_showupstream
5	5	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
6	6	----> set -l branch_color $___fish_git_prompt_color_branch
5	5	----> set -l branch_done $___fish_git_prompt_color_branch_done
2	6	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
4	4	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
4	4	----> set -l f ""
15	40	----> for i in $__fish_git_prompt_status_order...
1	6	-----> if test -n "$$i"...
5	5	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
2	5	-----> if test -n "$$i"...
3	3	------> test -n "$$i"
47	55	----> set b (string replace refs/heads/ '' -- $b)
8	8	-----> string replace refs/heads/ '' -- $b
2	18	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
16	16	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
4	25	----> if test -n "$b"...
5	5	-----> test -n "$b"
6	6	-----> set b "$branch_color$b$branch_done"
1	10	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
5	5	------> test -z "$dirtystate$untrackedfiles$stagedstate"
4	4	------> test -n "$___fish_git_prompt_char_cleanstate"
1	4	----> if test -n "$c"...
3	3	-----> test -n "$c"
1	4	----> if test -n "$r"...
3	3	-----> test -n "$r"
0	3	----> if test -n "$p"...
3	3	-----> test -n "$p"
0	3	----> if test -n "$f"...
3	3	-----> test -n "$f"
6	6	----> set -l format $argv[1]
3	10	----> if test -z "$format"...
3	3	-----> test -z "$format"
4	4	-----> set format " (%s)"
18	18	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
13	6655	> fish_right_prompt
6631	6631	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
11	11	-> printf " "
8	533	> fish_title
9	525	-> if not set -q INSIDE_EMACS...
5	5	--> not set -q INSIDE_EMACS
4	4	--> set -l ssh
3	3	--> set -q SSH_TTY
5	504	--> if set -q argv[1]...
3	3	---> set -q argv[1]
36	41	---> set -l command (status current-command)
5	5	----> status current-command
3	12	---> if test "$command" = fish...
6	6	----> test "$command" = fish
3	3	----> set command
65	443	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
6	6	----> string sub -l 20 -- $command
20	372	----> prompt_pwd -d 1 -D 1
6	6	-----> set -l options h/help d/dir-length= D/full-length-dirs=
20	20	-----> argparse -n prompt_pwd $options -- $argv
1	4	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
3	3	-----> set -q argv[1]
5	5	-----> set argv $PWD
3	3	-----> set -ql _flag_d
5	5	-----> set -l fish_prompt_pwd_dir_length $_flag_d
2	2	-----> set -q fish_prompt_pwd_dir_length
3	3	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
4	4	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
10	293	-----> for path in $argv...
37	46	------> set -l realhome (string escape --style=regex -- ~)
9	9	-------> string escape --style=regex -- ~
37	62	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
25	25	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
6	175	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
7	7	-------> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-------> set -l full
6	100	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
4	4	--------> test $fish_prompt_pwd_full_dirs -gt 0
34	81	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
37	47	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
10	10	----------> math $fish_prompt_pwd_full_dirs - 1
5	5	--------> set tmp $all[1]
4	4	--------> set full $all[2..]
41	58	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
17	17	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
6313	28625	> source /nix/store/dv7spdwndp37znhlq3i6s08azymssyqr-fish-3.7.1/share/fish/completions/git.fish
6	6	-> function __fish_git...
3	3	-> function __fish_git_global_optspecs...
1	1	-> function __fish_git_commits...
1	1	-> function __fish_git_recent_commits...
0	0	-> function __fish_git_branches...
5	5	-> function __fish_git_submodules...
1	1	-> function __fish_git_local_branches...
1	1	-> function __fish_git_unique_remote_branches...
0	0	-> function __fish_git_tags...
1	1	-> function __fish_git_heads...
3	3	-> function __fish_git_refs...
0	0	-> function __fish_git_remotes...
1	1	-> function __fish_git_files...
1	1	-> function __fish_git_rev_files...
1	1	-> function __fish_git_complete_rev_files...
1	1	-> function __fish_git_needs_rev_files...
1	1	-> function __fish_git_ranges...
3	3	-> function __fish_git_needs_command...
0	0	-> function __fish_git_config_keys...
1	1	-> function __fish_git_aliased_command...
399	5412	-> git config -z --get-regexp 'alias\..*' | while read -lz alias cmdline
    set -l command (__fish_git_aliased_command $cmdline)
    string match -q --regex '\w+' -- $command; or continue
    # Git aliases can contain chars that variable names can't - escape them.
    set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
    set -g __fish_git_alias_$alias $command $cmdline
    set --append -g __fish_git_aliases $alias
end
1765	1765	--> read -lz alias cmdline
99	235	--> set -l command (__fish_git_aliased_command $cmdline)
24	136	---> __fish_git_aliased_command $cmdline
76	112	----> for word in (string split ' ' -- $argv)...
13	13	-----> string split ' ' -- $argv
11	23	-----> switch $word...
11	11	------> echo $word
1	1	------> return
26	26	--> string match -q --regex '\w+' -- $command
75	135	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
60	60	---> string replace 'alias.' '' -- $alias | string escape --style=var
16	16	--> set -g __fish_git_alias_$alias $command $cmdline
8	8	--> set --append -g __fish_git_aliases $alias
16	16	--> read -lz alias cmdline
36	145	--> set -l command (__fish_git_aliased_command $cmdline)
10	109	---> __fish_git_aliased_command $cmdline
63	99	----> for word in (string split ' ' -- $argv)...
11	11	-----> string split ' ' -- $argv
16	25	-----> switch $word...
7	7	------> echo $word
2	2	------> return
23	23	--> string match -q --regex '\w+' -- $command
39	87	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
48	48	---> string replace 'alias.' '' -- $alias | string escape --style=var
9	9	--> set -g __fish_git_alias_$alias $command $cmdline
7	7	--> set --append -g __fish_git_aliases $alias
14	14	--> read -lz alias cmdline
36	129	--> set -l command (__fish_git_aliased_command $cmdline)
8	93	---> __fish_git_aliased_command $cmdline
59	85	----> for word in (string split ' ' -- $argv)...
8	8	-----> string split ' ' -- $argv
10	18	-----> switch $word...
6	6	------> echo $word
2	2	------> return
14	14	--> string match -q --regex '\w+' -- $command
61	152	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
91	91	---> string replace 'alias.' '' -- $alias | string escape --style=var
12	12	--> set -g __fish_git_alias_$alias $command $cmdline
6	6	--> set --append -g __fish_git_aliases $alias
15	15	--> read -lz alias cmdline
38	134	--> set -l command (__fish_git_aliased_command $cmdline)
8	96	---> __fish_git_aliased_command $cmdline
61	88	----> for word in (string split ' ' -- $argv)...
8	8	-----> string split ' ' -- $argv
10	19	-----> switch $word...
7	7	------> echo $word
2	2	------> return
16	16	--> string match -q --regex '\w+' -- $command
36	84	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
48	48	---> string replace 'alias.' '' -- $alias | string escape --style=var
9	9	--> set -g __fish_git_alias_$alias $command $cmdline
6	6	--> set --append -g __fish_git_aliases $alias
12	12	--> read -lz alias cmdline
27	113	--> set -l command (__fish_git_aliased_command $cmdline)
7	86	---> __fish_git_aliased_command $cmdline
54	79	----> for word in (string split ' ' -- $argv)...
8	8	-----> string split ' ' -- $argv
10	17	-----> switch $word...
5	5	------> echo $word
2	2	------> return
14	14	--> string match -q --regex '\w+' -- $command
34	77	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
43	43	---> string replace 'alias.' '' -- $alias | string escape --style=var
8	8	--> set -g __fish_git_alias_$alias $command $cmdline
5	5	--> set --append -g __fish_git_aliases $alias
17	17	--> read -lz alias cmdline
32	119	--> set -l command (__fish_git_aliased_command $cmdline)
8	87	---> __fish_git_aliased_command $cmdline
55	79	----> for word in (string split ' ' -- $argv)...
8	8	-----> string split ' ' -- $argv
9	16	-----> switch $word...
5	5	------> echo $word
2	2	------> return
14	14	--> string match -q --regex '\w+' -- $command
35	78	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
43	43	---> string replace 'alias.' '' -- $alias | string escape --style=var
7	7	--> set -g __fish_git_alias_$alias $command $cmdline
5	5	--> set --append -g __fish_git_aliases $alias
15	15	--> read -lz alias cmdline
26	112	--> set -l command (__fish_git_aliased_command $cmdline)
7	86	---> __fish_git_aliased_command $cmdline
54	79	----> for word in (string split ' ' -- $argv)...
8	8	-----> string split ' ' -- $argv
10	17	-----> switch $word...
6	6	------> echo $word
1	1	------> return
12	12	--> string match -q --regex '\w+' -- $command
27	65	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
38	38	---> string replace 'alias.' '' -- $alias | string escape --style=var
6	6	--> set -g __fish_git_alias_$alias $command $cmdline
4	4	--> set --append -g __fish_git_aliases $alias
14	14	--> read -lz alias cmdline
23	97	--> set -l command (__fish_git_aliased_command $cmdline)
5	74	---> __fish_git_aliased_command $cmdline
45	69	----> for word in (string split ' ' -- $argv)...
11	11	-----> string split ' ' -- $argv
7	13	-----> switch $word...
5	5	------> echo $word
1	1	------> return
10	10	--> string match -q --regex '\w+' -- $command
28	66	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
38	38	---> string replace 'alias.' '' -- $alias | string escape --style=var
6	6	--> set -g __fish_git_alias_$alias $command $cmdline
4	4	--> set --append -g __fish_git_aliases $alias
10	10	--> read -lz alias cmdline
21	95	--> set -l command (__fish_git_aliased_command $cmdline)
7	74	---> __fish_git_aliased_command $cmdline
43	67	----> for word in (string split ' ' -- $argv)...
10	10	-----> string split ' ' -- $argv
9	14	-----> switch $word...
4	4	------> echo $word
1	1	------> return
10	10	--> string match -q --regex '\w+' -- $command
25	64	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
39	39	---> string replace 'alias.' '' -- $alias | string escape --style=var
6	6	--> set -g __fish_git_alias_$alias $command $cmdline
4	4	--> set --append -g __fish_git_aliases $alias
15	15	--> read -lz alias cmdline
23	92	--> set -l command (__fish_git_aliased_command $cmdline)
5	69	---> __fish_git_aliased_command $cmdline
44	64	----> for word in (string split ' ' -- $argv)...
7	7	-----> string split ' ' -- $argv
6	13	-----> switch $word...
5	5	------> echo $word
2	2	------> return
11	11	--> string match -q --regex '\w+' -- $command
26	52	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
26	26	---> string replace 'alias.' '' -- $alias | string escape --style=var
7	7	--> set -g __fish_git_alias_$alias $command $cmdline
4	4	--> set --append -g __fish_git_aliases $alias
10	10	--> read -lz alias cmdline
23	97	--> set -l command (__fish_git_aliased_command $cmdline)
5	74	---> __fish_git_aliased_command $cmdline
48	69	----> for word in (string split ' ' -- $argv)...
8	8	-----> string split ' ' -- $argv
8	13	-----> switch $word...
4	4	------> echo $word
1	1	------> return
11	11	--> string match -q --regex '\w+' -- $command
27	51	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
24	24	---> string replace 'alias.' '' -- $alias | string escape --style=var
6	6	--> set -g __fish_git_alias_$alias $command $cmdline
4	4	--> set --append -g __fish_git_aliases $alias
12	12	--> read -lz alias cmdline
22	89	--> set -l command (__fish_git_aliased_command $cmdline)
5	67	---> __fish_git_aliased_command $cmdline
43	62	----> for word in (string split ' ' -- $argv)...
6	6	-----> string split ' ' -- $argv
7	13	-----> switch $word...
4	4	------> echo $word
2	2	------> return
8	8	--> string match -q --regex '\w+' -- $command
25	48	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
23	23	---> string replace 'alias.' '' -- $alias | string escape --style=var
6	6	--> set -g __fish_git_alias_$alias $command $cmdline
4	4	--> set --append -g __fish_git_aliases $alias
14	14	--> read -lz alias cmdline
22	90	--> set -l command (__fish_git_aliased_command $cmdline)
7	68	---> __fish_git_aliased_command $cmdline
42	61	----> for word in (string split ' ' -- $argv)...
6	6	-----> string split ' ' -- $argv
8	13	-----> switch $word...
4	4	------> echo $word
1	1	------> return
11	11	--> string match -q --regex '\w+' -- $command
27	50	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
23	23	---> string replace 'alias.' '' -- $alias | string escape --style=var
6	6	--> set -g __fish_git_alias_$alias $command $cmdline
8	8	--> set --append -g __fish_git_aliases $alias
10	10	--> read -lz alias cmdline
22	90	--> set -l command (__fish_git_aliased_command $cmdline)
6	68	---> __fish_git_aliased_command $cmdline
43	62	----> for word in (string split ' ' -- $argv)...
6	6	-----> string split ' ' -- $argv
8	13	-----> switch $word...
4	4	------> echo $word
1	1	------> return
11	11	--> string match -q --regex '\w+' -- $command
27	50	--> set -l alias (string replace 'alias.' '' -- $alias | string escape --style=var)
23	23	---> string replace 'alias.' '' -- $alias | string escape --style=var
6	6	--> set -g __fish_git_alias_$alias $command $cmdline
4	4	--> set --append -g __fish_git_aliases $alias
4	4	--> read -lz alias cmdline
28	832	-> for alias in $__fish_git_aliases...
4	4	--> set -l handled $alias
9	123	--> while true
...
64	64	---> true
5	5	---> set -l alias_varname __fish_git_alias_$alias
5	5	---> set -l aliased_command $$alias_varname[1][1]
27	32	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
5	5	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
5	53	--> while true
...
3	3	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
24	29	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
5	5	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
2	2	---> break
3	3	--> set -l handled $alias
6	51	--> while true
...
3	3	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
23	28	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
5	5	----> string escape --style=var -- $aliased_command
3	3	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
3	49	--> while true
...
2	2	---> true
4	4	---> set -l alias_varname __fish_git_alias_$alias
5	5	---> set -l aliased_command $$alias_varname[1][1]
22	27	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
5	5	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
5	49	--> while true
...
2	2	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
23	27	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
4	4	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
4	47	--> while true
...
2	2	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
22	26	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
4	4	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
5	47	--> while true
...
2	2	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
22	26	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
4	4	----> string escape --style=var -- $aliased_command
3	3	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
4	48	--> while true
...
2	2	---> true
4	4	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
23	27	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
4	4	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
2	2	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
4	48	--> while true
...
2	2	---> true
4	4	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
22	26	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
4	4	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
6	48	--> while true
...
3	3	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
22	26	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
4	4	----> string escape --style=var -- $aliased_command
3	3	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
2	2	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
6	51	--> while true
...
2	2	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
22	29	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
7	7	----> string escape --style=var -- $aliased_command
3	3	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
4	49	--> while true
...
3	3	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
22	26	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
4	4	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
3	3	---> set -q $aliased_varname
2	2	---> break
2	2	--> set -l handled $alias
4	50	--> while true
...
2	2	---> true
4	4	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
25	29	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
4	4	----> string escape --style=var -- $aliased_command
4	4	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
2	2	---> set -q $aliased_varname
1	1	---> break
3	3	--> set -l handled $alias
6	49	--> while true
...
3	3	---> true
3	3	---> set -l alias_varname __fish_git_alias_$alias
4	4	---> set -l aliased_command $$alias_varname[1][1]
22	27	---> set -l aliased_escaped (string escape --style=var -- $aliased_command)
5	5	----> string escape --style=var -- $aliased_command
3	3	---> set -l aliased_varname __fish_git_alias_$aliased_escaped
2	2	---> set -q $aliased_varname
1	1	---> break
1	1	-> function __fish_git_using_command...
1	1	-> function __fish_git_contains_opt...
1	1	-> function __fish_git_stash_using_command...
1	1	-> function __fish_git_stash_not_using_subcommand...
4	4	-> function __fish_git_complete_worktrees...
1	1	-> function __fish_git_complete_stashes...
1	1	-> function __fish_git_aliases...
1	1	-> function __fish_git_custom_commands...
1	1	-> function __fish_git_branch_for_remote...
1	1	-> function __fish_git_possible_commithash...
1	1	-> function __fish_git_reflog...
1	1	-> function __fish_git_help_all_concepts...
2	2	-> function __fish_git_diff_opt -a option...
1	1	-> function __fish_git_show_opt -a option...
2	2	-> function __fish_git_is_rebasing...
75	75	-> complete git -f -l help -s h -d 'Display manual of a Git command'
10	10	-> complete git -f -n __fish_git_needs_command -l version -s v -d 'display git version'
11	11	-> complete git -x -n __fish_git_needs_command -s C -a '(__fish_complete_directories)' -d 'run as if started in dir'
17	17	-> complete git -x -n __fish_git_needs_command -s c -a '(__fish_git config -l 2>/dev/null | string replace = \t)' -d 'set config option (conf-key=val)'
17	17	-> complete git -x -n __fish_git_needs_command -l config-env -a '(__fish_git config -l 2>/dev/null | string replace = \t)' -d 'like -c but environment var (conf-key=ENVVAR)'
9	9	-> complete git -x -n __fish_git_needs_command -l exec-path -a '(__fish_complete_directories)' -d 'get or set the path to git'
6	6	-> complete git -f -n __fish_git_needs_command -l html-path -d 'print html docs path'
5	5	-> complete git -f -n __fish_git_needs_command -l man-path -d 'print man pages path'
5	5	-> complete git -f -n __fish_git_needs_command -l info-path -d 'print info docs path'
6	6	-> complete git -f -n __fish_git_needs_command -s p -l paginate -d 'pipe output into pager'
6	6	-> complete git -f -n __fish_git_needs_command -s P -l no-pager -d 'don\'t pipe output into pager'
5	5	-> complete git -r -n __fish_git_needs_command -l git-dir -d 'set path to the repo'
7	7	-> complete git -r -n __fish_git_needs_command -l work-tree -d 'set path to the working tree'
5	5	-> complete git -f -n __fish_git_needs_command -l namespace -d 'set Git namespace'
5	5	-> complete git -f -n __fish_git_needs_command -l bare -d 'treat the repo as bare'
5	5	-> complete git -f -n __fish_git_needs_command -l no-replace-objects -d 'disable replacement references'
5	5	-> complete git -f -n __fish_git_needs_command -l literal-pathspecs -d 'treat pathspecs literally'
5	5	-> complete git -f -n __fish_git_needs_command -l glob-pathspecs -d 'treat pathspecs as globs'
5	5	-> complete git -f -n __fish_git_needs_command -l noglob-pathspecs -d 'don\'t treat pathspecs as globs'
5	5	-> complete git -f -n __fish_git_needs_command -l icase-pathspecs -d 'match pathspecs case-insensitively'
5	5	-> complete git -f -n __fish_git_needs_command -l no-optional-locks -d 'skip optional operations requiring locks'
15	15	-> complete git -x -n __fish_git_needs_command -l list-cmds -d 'list commands by group' -k -a "builtins\t
parseopt\t'builtins using parse-options'
others\t'git- commands in \$PATH'
nohelpers\t'exclude helper commands'
config\t'list completion.commands'"
12	12	-> complete -f -c git -n '__fish_git_using_command log show diff-tree rev-list' -l pretty -a '(__fish_git_show_opt pretty)'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l abbrev -d 'Show only a partial prefix instead of the full 40-byte hexadecimal object name'
8	8	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l binary -d 'Output a binary diff that can be applied with "git-apply"'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l check -d 'Warn if changes introduce conflict markers or whitespace errors'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l color -d 'Show colored diff'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l color-moved -d 'Moved lines of code are colored differently'
10	10	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l color-words -d 'Equivalent to --word-diff=color plus --word-diff-regex=<regex>'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l compact-summary -d 'Output a condensed summary of extended header information'
8	8	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l dst-prefix -d 'Show the given destination prefix instead of "b/"'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l ext-diff -d 'Allow an external diff helper to be executed'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l find-copies-harder -d 'Inspect unmodified files as candidates for the source of copy'
8	8	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l find-object -d 'Look for differences that change the number of occurrences of the object'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l full-index -d 'Show the full pre- and post-image blob object names on the "index" line'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l histogram -d 'Generate a diff using the "histogram diff" algorithm'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l ignore-blank-lines -d 'Ignore changes whose lines are all blank'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l ignore-cr-at-eol -d 'Ignore carrige-return at the end of line when doing a comparison'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l ignore-space-at-eol -d 'Ignore changes in whitespace at EOL'
8	8	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l indent-heuristic -d 'Enable the heuristic that shift diff hunk boundaries'
7	7	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l inter-hunk-context -d 'Show the context between diff hunks, up to the specified number of lines'
7	7	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l ita-invisible-in-index -d 'Make the entry appear as a new file in "git diff" and non-existent in "git diff -l cached"'
7	7	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l line-prefix -d 'Prepend an additional prefix to every line of output'
8	8	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l minimal -d 'Spend extra time to make sure the smallest possible diff is produced'
9	9	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l name-only -d 'Show only names of changed files'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l name-status -d 'Show only names and status of changed files'
8	8	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l no-color -d 'Turn off colored diff'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l no-ext-diff -d 'Disallow external diff drivers'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l no-indent-heuristic -d 'Disable the indent heuristic'
8	8	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l no-prefix -d 'Do not show any source or destination prefix'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l no-renames -d 'Turn off rename detection'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l no-textconv -d 'Disallow external text conversion filters to be run when comparing binary files'
8	8	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l numstat -d 'Shows number of added/deleted lines in decimal notation'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l patch-with-raw -d 'Synonym for -p --raw'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l patch-with-stat -d 'Synonym for -p --stat'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l patience -d 'Generate a diff using the "patience diff" algorithm'
7	7	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l pickaxe-all -d 'When -S or -G finds a change, show all the changes in that changeset'
9	9	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l pickaxe-regex -d 'Treat the <string> given to -S as an extended POSIX regular expression to match'
7	7	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l relative -d 'Exclude changes outside the directory and show relative pathnames'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l shortstat -d 'Output only the last line of the --stat format containing total number of modified files'
12	12	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -l src-prefix -d 'Show the given source prefix instead of "a/"'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l stat -d 'Generate a diffstat'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff apply' -l stat -d 'Generate a diffstat'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l summary -d 'Output a condensed summary of extended header information'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l textconv -d 'Allow external text conversion filters to be run when comparing binary files'
6	6	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l word-diff -d 'Show a word diff'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -l word-diff-regex -d 'Use <regex> to decide what a word is'
8	8	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -s a -l text -d 'Treat all files as text'
8	8	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -s B -l break-rewrites -d 'Break complete rewrite changes into pairs of delete and create'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -s b -l ignore-space-change -d 'Ignore changes in amount of whitespace'
7	7	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -s C -l find-copies -d 'Detect copies as well as renames'
8	8	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -s D -l irreversible-delete -d 'Omit the preimage for deletes'
7	7	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -s G -d "Look for differences where <regex> matches the patch's added/removed lines"
8	8	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -s M -l find-renames -d 'Detect and report renames'
6	6	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -s R -d 'Swap inputs to create a reverse diff'
7	7	-> complete -c git -n '__fish_git_using_command diff log show range-diff' -s S -d 'Look for differences that change the number of occurrences of the string'
9	9	-> complete -c git -n '__fish_git_using_command diff show range-diff' -s W -l function-context -d 'Show whole surrounding functions of changes'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -s w -l ignore-all-space -d 'Ignore whitespace when comparing lines'
7	7	-> complete -c git -n '__fish_git_using_command diff show range-diff' -s z -d 'Use NULs as output field/commit terminators'
10	10	-> complete -r -c git -n '__fish_git_using_command diff log show range-diff' -s O -d 'Control the order in which files appear in the output'
7	7	-> complete -f -c git -n '__fish_git_using_command diff show range-diff' -l anchored -d 'Generate a diff using the "anchored diff" algorithm'
9	9	-> complete -x -c git -n '__fish_git_using_command diff log show range-diff' -s l -d 'Prevents rename/copy detection when rename/copy targets exceed the given number'
10	10	-> complete -x -c git -n '__fish_git_using_command diff show range-diff' -l diff-filter -a '(__fish_git_diff_opt diff-filter)' -d 'Choose diff filters'
10	10	-> complete -x -c git -n '__fish_git_using_command diff log show range-diff' -l diff-algorithm -a '(__fish_git_diff_opt diff-algorithm)' -d 'Choose a diff algorithm'
14	14	-> complete -x -c git -n '__fish_git_using_command diff log show range-diff' -l dirstat -a '(__fish_git_diff_opt dirstat)' -d 'Output the distribution of relative amount of changes for each sub-directory'
10	10	-> complete -x -c git -n '__fish_git_using_command diff log show range-diff' -l ignore-submodules -a '(__fish_git_diff_opt ignore-submodules)' -d 'Ignore changes to submodules in the diff generation'
10	10	-> complete -x -c git -n '__fish_git_using_command diff log show range-diff' -l submodule -a '(__fish_git_diff_opt submodule)' -d 'Specify how differences in submodules are shown'
10	10	-> complete -x -c git -n '__fish_git_using_command diff log show range-diff' -l ws-error-highlight -a '(__fish_git_diff_opt ws-error-highlight)' -d 'Highlight whitespace errors in lines of the diff'
7	7	-> complete -f -c git -n '__fish_git_using_command fetch pull' -l unshallow -d 'Convert a shallow repository to a complete one'
7	7	-> complete -f -c git -n '__fish_git_using_command fetch pull' -l set-upstream -d 'Add upstream (tracking) reference'
6	6	-> complete -f -c git -n __fish_git_needs_command -a fetch -d 'Download objects from another repo'
10	10	-> complete -f -c git -n '__fish_git_using_command fetch' -n 'not __fish_git_branch_for_remote' -a '(__fish_git_remotes)' -d Remote
15	15	-> complete -f -c git -n '__fish_git_using_command fetch' -n __fish_git_branch_for_remote -ka '(__fish_git_branch_for_remote)'
7	7	-> complete -f -c git -n '__fish_git_using_command fetch' -s q -l quiet -d 'Be more quiet'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -s v -l verbose -d 'Be more verbose'
7	7	-> complete -f -c git -n '__fish_git_using_command fetch' -s a -l append -d 'Append to .git/FETCH_HEAD instead of overwriting'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l upload-pack -d 'Path to upload pack on remote end'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -s f -l force -d 'Force update of local branches'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -s p -l prune -d 'Prune remote-tracking branches no longer on remote'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l all -d 'Fetch all remotes'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l atomic -d 'Use atomic transfer to update references'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -s m -l multiple -d 'Fetch from multiple remotes'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -s t -l tags -d 'Fetch all tags and associated objects'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -s P -l prune-tags -d 'Prune local tags no longer on remote and clobber changed tags'
7	7	-> complete -f -c git -n '__fish_git_using_command fetch' -l prefetch -d 'Modify the refspec to replace all refs within refs/prefetch/'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -s j -l jobs -d 'Numbers of submodules fetched in parallel'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -s n -d 'Do not fetch all tags (--no-tags)'
14	14	-> complete -f -c git -n '__fish_git_using_command fetch' -l dry-run -d 'Dry run'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l depth -d 'Limit number of commits'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l with-fetch-head -d 'Write fetched references to the FETCH_HEAD file'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l update-shallow -d 'Accept refs that update .git/shallow'
7	7	-> complete -f -c git -n '__fish_git_using_command fetch' -s k -l keep -d 'Keep downloaded pack'
8	8	-> complete -f -c git -n '__fish_git_using_command fetch' -s u -l update-head-ok -d 'Allow updating of HEAD ref'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l progress -d 'Force progress reporting'
7	7	-> complete -f -c git -n '__fish_git_using_command fetch' -l deepen -d 'Deepen history of shallow clones'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l shallow-since -d 'Deepen history of shallow repository based on time'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l shallow-exclude -d 'Deepen history of shallow clone, excluding rev'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l unshallow -d 'Convert to a complete repository'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l refetch -d 'Re-fetch without negotiating common commits'
10	10	-> complete -f -c git -n '__fish_git_using_command fetch' -l negotiation-tip -d 'Only report commits reachable from these tips' -kxa '(__fish_git_commits; __fish_git_branches)'
6	6	-> complete -f -c git -n '__fish_git_using_command fetch' -l negotiate-only -d "Don't fetch, only show commits in common with the server"
6	6	-> complete -f -c git -n __fish_git_needs_command -a filter-branch -d 'Rewrite branches'
7	7	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l env-filter -d 'Filter for rewriting env vars like author name/email'
6	6	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l tree-filter -d 'Filter for rewriting the tree and its contents'
6	6	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l index-filter -d 'Filter for rewriting the index'
7	7	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l parent-filter -d 'Filter for rewriting the commit'
6	6	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l msg-filter -d 'Filter for rewriting the commit messages'
6	6	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l commit-filter -d 'Filter for performing the commit'
8	8	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l tag-name-filter -d 'Filter for rewriting tag names'
6	6	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l subdirectory-filter -d 'Only look at the history which touches the given subdirectory'
6	6	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l prune-empty -d 'Ignore empty commits generated by filters'
7	7	-> complete -f -c git -n '__fish_git_using_command filter-branch' -l original -d 'Use this option to set the namespace where the original commits will be stored'
6	6	-> complete -r -c git -n '__fish_git_using_command filter-branch' -s d -d 'Use this option to set the path to the temporary directory used for rewriting'
7	7	-> complete -c git -n '__fish_git_using_command filter-branch' -s f -l force -d 'Filter even with refs in refs/original or existing temp directory'
7	7	-> set -l remotecommands add rm remove show prune update rename set-head set-url set-branches get-url
6	6	-> complete -f -c git -n __fish_git_needs_command -a remote -d 'Manage tracked repositories'
15	15	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from $remotecommands" -a '(__fish_git_remotes)'
14	14	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -s v -l verbose -d 'Be verbose'
13	13	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a add -d 'Adds a new remote'
12	12	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a rm -d 'Removes a remote'
12	12	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a remove -d 'Removes a remote'
12	12	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a show -d 'Shows a remote'
14	14	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a prune -d 'Deletes all stale tracking branches'
12	12	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a update -d 'Fetches updates'
12	12	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a rename -d 'Renames a remote'
13	13	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a set-head -d 'Sets the default branch for a remote'
12	12	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a set-url -d 'Changes URLs for a remote'
12	12	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a get-url -d 'Retrieves URLs for a remote'
15	15	-> complete -f -c git -n "__fish_git_using_command remote" -n "not __fish_seen_subcommand_from $remotecommands" -a set-branches -d 'Changes the list of branches tracked by a remote'
10	10	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from add " -s f -d 'Once the remote information is set up git fetch <name> is run'
9	9	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from add " -l tags -d 'Import every tag from a remote with git fetch <name>'
8	8	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from add " -l no-tags -d "Don't import tags from a remote with git fetch <name>"
11	11	-> complete -f -c git -n '__fish_git_using_command remote' -n '__fish_seen_subcommand_from remove' -xa '(__fish_git_remotes)'
8	8	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from set-branches" -l add -d 'Add to the list of currently tracked branches instead of replacing it'
9	9	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from set-url" -l push -d 'Manipulate push URLs instead of fetch URLs'
9	9	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from set-url" -l add -d 'Add new URL instead of changing the existing URLs'
10	10	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from set-url" -l delete -d 'Remove URLs that match specified URL'
8	8	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from get-url" -l push -d 'Query push URLs rather than fetch URLs'
8	8	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from get-url" -l all -d 'All URLs for the remote will be listed'
10	10	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from show" -s n -d 'Remote heads are not queried, cached information is used instead'
9	9	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from prune" -l dry-run -d 'Report what will be pruned but do not actually prune it'
8	8	-> complete -f -c git -n "__fish_git_using_command remote" -n "__fish_seen_subcommand_from update" -l prune -d 'Prune all remotes that are updated'
6	6	-> complete -f -c git -n __fish_git_needs_command -a show -d 'Show the last commit of a branch'
14	14	-> complete -f -c git -n '__fish_git_using_command show' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_branches)'
12	12	-> complete -f -c git -n '__fish_git_using_command show' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_tags)' -d Tag
11	11	-> complete -f -c git -n '__fish_git_using_command show' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_commits)'
13	13	-> complete -f -c git -n '__fish_git_using_command show' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_complete_stashes)'
11	11	-> complete -f -c git -n __fish_git_needs_rev_files -n 'not contains -- -- (commandline -opc)' -xa '(__fish_git_complete_rev_files)'
9	9	-> complete -F -c git -n '__fish_git_using_command show' -n 'contains -- -- (commandline -opc)'
9	9	-> complete -f -c git -n '__fish_git_using_command show' -l format -d 'Pretty-print the contents of the commit logs in a given format' -a '(__fish_git_show_opt format)'
6	6	-> complete -f -c git -n '__fish_git_using_command show' -l abbrev-commit -d 'Show only a partial hexadecimal commit object name'
7	7	-> complete -f -c git -n '__fish_git_using_command show' -l no-abbrev-commit -d 'Show the full 40-byte hexadecimal commit object name'
6	6	-> complete -f -c git -n '__fish_git_using_command show' -l oneline -d 'Shorthand for "--pretty=oneline --abbrev-commit"'
6	6	-> complete -f -c git -n '__fish_git_using_command show' -l encoding -d 'Re-code the commit log message in the encoding'
9	9	-> complete -f -c git -n '__fish_git_using_command show' -l expand-tabs -d 'Perform a tab expansion in the log message'
6	6	-> complete -f -c git -n '__fish_git_using_command show' -l no-expand-tabs -d 'Do not perform a tab expansion in the log message'
8	8	-> complete -f -c git -n '__fish_git_using_command show' -l notes -ka '(__fish_git_refs)' -d 'Show the notes that annotate the commit'
6	6	-> complete -f -c git -n '__fish_git_using_command show' -l no-notes -d 'Do not show notes'
8	8	-> complete -f -c git -n '__fish_git_using_command show' -s s -l no-patch -d 'Suppress diff output'
6	6	-> complete -f -c git -n '__fish_git_using_command show' -l show-signature -d 'Check the validity of a signed commit object'
5	5	-> complete -f -c git -n __fish_git_needs_command -a show-branch -d 'Show the commits on branches'
8	8	-> complete -f -c git -n '__fish_git_using_command show-branch' -ka '(__fish_git_refs)' -d Rev
7	7	-> complete -f -c git -n '__fish_git_using_command show-branch' -s r -l remotes -d "Shows the remote tracking branches"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -s a -l all -d "Show both remote-tracking branches and local branches"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -l current -d "Includes the current branch to the list of revs to be shown"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -l topo-order -d "Makes commits appear in topological order"
7	7	-> complete -f -c git -n '__fish_git_using_command show-branch' -l date-order -d "Makes commits appear in date order"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -l sparse -d "Shows merges only reachable from one tip"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -l no-name -d "Do not show naming strings for each commit"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -l sha1-name -d "Name commits with unique prefix"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -l no-color -d "Turn off colored output"
7	7	-> complete -f -c git -n '__fish_git_using_command show-branch' -l merge-base -d "Determine merge bases for the given commits"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -l independent -d "Show which refs can't be reached from any other"
6	6	-> complete -f -c git -n '__fish_git_using_command show-branch' -l topics -d "Show only commits that are not on the first given branch"
6	6	-> complete -c git -n __fish_git_needs_command -a add -d 'Add file contents to the staging area'
10	10	-> complete -c git -n '__fish_git_using_command add' -s n -l dry-run -d "Don't actually add the file(s)"
6	6	-> complete -c git -n '__fish_git_using_command add' -s v -l verbose -d 'Be verbose'
6	6	-> complete -c git -n '__fish_git_using_command add' -s f -l force -d 'Allow adding otherwise ignored files'
6	6	-> complete -c git -n '__fish_git_using_command add' -s i -l interactive -d 'Interactive mode'
7	7	-> complete -c git -n '__fish_git_using_command add' -s p -l patch -d 'Interactively choose hunks to stage'
6	6	-> complete -c git -n '__fish_git_using_command add' -s e -l edit -d 'Manually create a patch'
6	6	-> complete -c git -n '__fish_git_using_command add' -s u -l update -d 'Only match tracked files'
6	6	-> complete -c git -n '__fish_git_using_command add' -s A -l all -d 'Match files both in working tree and index'
7	7	-> complete -c git -n '__fish_git_using_command add' -s N -l intent-to-add -d 'Record only the fact that the path will be added later'
6	6	-> complete -c git -n '__fish_git_using_command add' -l refresh -d "Don't add the file(s), but only refresh their stat"
7	7	-> complete -c git -n '__fish_git_using_command add' -l chmod -xa "-x\t'Track file as non-executable' +x\t'Track file as executable'"
7	7	-> complete -c git -n '__fish_git_using_command add' -l ignore-errors -d 'Ignore errors'
6	6	-> complete -c git -n '__fish_git_using_command add' -l ignore-missing -d 'Check if any of the given files would be ignored'
11	11	-> complete -f -c git -n '__fish_git_using_command add' -a '(__fish_git_files modified untracked deleted unmerged modified-staged-deleted)'
5	5	-> complete -c git -n __fish_git_needs_command -a am -d 'Apply patches from a mailbox'
8	8	-> complete -f -c git -n '__fish_git_using_command am' -s s -l signoff -d 'Add a Signed-off-By trailer to commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l keep-non-patch -d 'Only strip bracket pairs containing \'PATCH\''
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l no-keep-cr -d 'Override am.keepcr to false'
7	7	-> complete -f -c git -n '__fish_git_using_command am' -s c -l scissors -d 'Remove everything in body before scissors'
8	8	-> complete -f -c git -n '__fish_git_using_command am' -l no-scissors -d 'Ignore scissor lines'
9	9	-> complete -x -c git -n '__fish_git_using_command am' -l quoted-cr -a 'nowarn warn strip' -d 'What to do when an email ends with CRLF'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l no-messageid -d 'Do not add message id to commit message'
20	20	-> complete -f -c git -n '__fish_git_using_command am' -s q -l quiet -d 'Supress logs'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l no-utf8 -d 'Disable all charset re-encoding of metadata'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -s 3 -l 3way -d 'Fall back to three way merge on patch failure'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l no-3way -d 'Do not fall back to three way merge on patch failure'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l rerere-autoupdate -d 'Allow rerere to update index if possible'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l ignore-space-change -d 'Pass --ignore-space-change to git apply'
6	6	-> complete -F -c git -n '__fish_git_using_command am' -l directory -d 'Pass --directory to git apply'
7	7	-> complete -F -c git -n '__fish_git_using_command am' -l exclude -d 'Pass --exclude to git apply'
6	6	-> complete -F -c git -n '__fish_git_using_command am' -l include -d 'Pass --include to git apply'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l reject -d 'Pass --reject to git apply'
9	9	-> complete -x -f git -n '__fish_git_using_command am' -l patch-format -a 'mbox mboxrd stgit stgit-series hg' -d 'Specify the patch format'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -s i -l interactive -d 'Run interactively'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l committer-date-is-author-date -d 'Treat committer date as author date'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l ignore-date -d 'Treat author date as committer date'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l skip -d 'Skip current patch'
12	12	-> complete -x -c git -n '__fish_git_using_command am' -s S -l gpg-sign -a '(type -q gpg && __fish_complete_gpg_key_id gpg)' -d 'Sign commits with gpg'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l no-gpg-sign -d 'Do not sign commits'
7	7	-> complete -f -c git -n '__fish_git_using_command am' -s r -l resolved -l continue -d 'Mark patch failures as resolved'
6	6	-> complete -x -c git -n '__fish_git_using_command am' -l resolvemsg -d 'Message to print after patch failure'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l abort -d 'Abort patch operation and restore branch'
6	6	-> complete -f -c git -n '__fish_git_using_command am' -l quit -d 'Abort without restoring branch'
7	7	-> complete -c git -n '__fish_git_using_command am' -l show-current-patch -a 'diff raw' -d 'Show message at which patch failures occured'
9	9	-> complete -F -c git -n '__fish_git_using_command checkout' -n 'contains -- -- (commandline -opc)'
6	6	-> complete -f -c git -n __fish_git_needs_command -a checkout -d 'Checkout and switch to a branch'
12	12	-> complete -f -c git -n '__fish_git_using_command checkout' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_recent_commits --all)'
11	11	-> complete -f -c git -n '__fish_git_using_command checkout' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_tags)' -d Tag
11	11	-> complete -f -c git -n '__fish_git_using_command checkout' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_heads)' -d Head
12	12	-> complete -f -c git -n '__fish_git_using_command checkout' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_unique_remote_branches)' -d 'Unique Remote Branch'
11	11	-> complete -f -c git -n '__fish_git_using_command checkout' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_branches)'
9	9	-> complete -f -c git -n '__fish_git_using_command checkout' -ka '(__fish_git_files modified deleted modified-staged-deleted)'
6	6	-> complete -f -c git -n '__fish_git_using_command checkout' -s b -d 'Create a new branch'
6	6	-> complete -f -c git -n '__fish_git_using_command checkout' -s B -d 'Create a new branch or reset existing to start point'
9	9	-> complete -f -c git -n '__fish_git_using_command checkout' -s t -l track -d 'Track a new branch'
6	6	-> complete -f -c git -n '__fish_git_using_command checkout' -l theirs -d 'Keep staged changes'
7	7	-> complete -f -c git -n '__fish_git_using_command checkout' -l ours -d 'Keep unmerged changes'
6	6	-> complete -f -c git -n '__fish_git_using_command checkout' -l recurse-submodules -d 'Update the work trees of submodules'
6	6	-> complete -f -c git -n '__fish_git_using_command checkout' -l no-recurse-submodules -d 'Do not update the work trees of submodules'
8	8	-> complete -f -c git -n '__fish_git_using_command checkout' -l progress -d 'Report progress even if not connected to a terminal'
6	6	-> complete -f -c git -n '__fish_git_using_command checkout' -l no-progress -d "Don't report progress"
6	6	-> complete -f -c git -n '__fish_git_using_command checkout' -s f -l force -d 'Switch even if working tree differs or unmerged files exist'
6	6	-> complete -c git -n __fish_git_needs_command -a apply -d 'Apply a patch'
7	7	-> complete -f -c git -n '__fish_git_using_command apply' -l numstat -d 'Show number of additions and deletions'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l summary -d 'Output a condensed summary'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l check -d 'Just check if the patches can be applied'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l index -d 'Apply patch to index and working tree'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l cached -d 'Apply patch to index'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l intent-to-add -d 'Add entry for file in index with no content'
8	8	-> complete -f -c git -n '__fish_git_using_command apply' -s 3 -l 3way -d 'Attempt a 3 way merge on conflicts'
6	6	-> complete -F -c git -n '__fish_git_using_command apply' -l build-fake-ancestor -d 'Build a temporary index containing these blobs'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -s R -l reverse -d 'Apply the patch in reverse'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l reject -d 'Leave rejected hunks in *.rej files'
10	10	-> complete -f -c git -n '__fish_git_using_command apply' -n '__fish_git_contains_opt numstat' -s z -d 'Do not munge pathnames'
6	6	-> complete -x -c git -n '__fish_git_using_command apply am' -s p -d 'Remove n leading path components'
7	7	-> complete -x -c git -n '__fish_git_using_command apply am' -s C -d 'Ensure n that lines of surrounding context match'
8	8	-> complete -f -c git -n '__fish_git_using_command apply' -l unidiff-zero -d 'Do not break on diffs generated using --unified=0'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l apply -d 'Always apply patches'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l no-add -d 'Ignore additions made by patches'
7	7	-> complete -f -c git -n '__fish_git_using_command apply' -l allow-binary-replacement -l binary -d 'Also patch binaries'
6	6	-> complete -F -c git -n '__fish_git_using_command apply' -l exclude -d 'Dont apply changes to files matching given pattern'
6	6	-> complete -F -c git -n '__fish_git_using_command apply' -l include -d 'Apply changes to files matching given pattern'
7	7	-> complete -f -c git -n '__fish_git_using_command apply am' -l ignore-space-change -l ignore-whitespace -d 'Ignore whitespace change in context lines'
11	11	-> complete -x -c git -n '__fish_git_using_command apply am' -l whitespace -a 'nowarn warn fix error error-all' -d 'Action to take when there are whitespace errors'
7	7	-> complete -f -c git -n '__fish_git_using_command apply' -l inaccurate-eof -d 'Work around some diff versions not detecting newlines at end of file'
7	7	-> complete -f -c git -n '__fish_git_using_command apply' -s v -l verbose -d 'Report progress to stderr'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l recount -d 'Do not trust the line counts in the hunk headers'
6	6	-> complete -F -c git -n '__fish_git_using_command apply' -l directory -d 'Prepend given path to all filenames'
6	6	-> complete -f -c git -n '__fish_git_using_command apply' -l unsafe-paths -d 'Allow patches that work outside working area'
6	6	-> complete -f -c git -n __fish_git_needs_command -a archive -d 'Create an archive of files from a tree'
7	7	-> complete -f -c git -n '__fish_git_using_command archive' -s l -l list -d "Show all available formats"
7	7	-> complete -f -c git -n '__fish_git_using_command archive' -s v -l verbose -d "Be verbose"
6	6	-> complete -f -c git -n '__fish_git_using_command archive' -l worktree-attributes -d "Look for attributes in .gitattributes files in the working tree as well"
6	6	-> complete -f -c git -n __fish_git_needs_command -a bisect -d 'Use binary search to find what introduced a bug'
26	26	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_prev_arg_in bisect' -xa "
start\t'Start a new bisect session'
bad\t'Mark a commit as bad'
new\t'Mark a commit as new'
good\t'Mark a commit as good'
old\t'Mark a commit as old'
terms\t'Show terms used for new/old states'
skip\t'Skip some commits'
reset\t'Exit a bisect session and reset HEAD'
visualize\t'See remaining commits in gitk'
replay\t'Replay a bisect log file'
log\t'Record a bisect log file'
run\t'Bisect automaically with the given command as discriminator'
help\t'Print a synopsis of all commands'
"
8	8	-> complete -c git -n '__fish_git_using_command bisect' -n '__fish_seen_argument --' -F
10	10	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from start' -l term-new -l term-bad -x -d 'Use another term instead of new/bad'
9	9	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from start' -l term-old -l term-good -x -d 'Use another term instead of old/good'
9	9	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from start' -l no-checkout -d 'Do not checkout tree, only update BISECT_HEAD'
8	8	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from start' -l first-parent -d 'On merge commits, follow only the first parent commit'
14	14	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from start' -n 'not contains -- -- (commandline -opc)' -a '(__fish_git_refs)'
10	10	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from bad new good old' -a '(__fish_git_refs)'
9	9	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from terms' -l --term-good -d 'Print the term for the old state'
8	8	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from terms' -l --term-bad -d 'Print the term for the new state'
9	9	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from skip' -a '(__fish_git_ranges)'
10	10	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from reset' -a '(__fish_git_refs)'
7	7	-> complete -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from replay' -F
10	10	-> complete -f -c git -n '__fish_git_using_command bisect' -n '__fish_seen_subcommand_from run' -xa '(__fish_complete_subcommand --fcs-skip=3)'
6	6	-> complete -f -c git -n __fish_git_needs_command -a branch -d 'List, create, or delete branches'
8	8	-> complete -f -c git -n '__fish_git_using_command branch' -ka '(__fish_git_branches)'
13	13	-> complete -f -c git -n '__fish_git_using_command branch' -s d -l delete -d 'Delete branch' -xa '(__fish_git_local_branches)'
9	9	-> complete -f -c git -n '__fish_git_using_command branch' -s D -d 'Force deletion of branch' -xa '(__fish_git_local_branches)'
6	6	-> complete -f -c git -n '__fish_git_using_command branch' -s f -l force -d 'Reset branch even if it already exists'
8	8	-> complete -f -c git -n '__fish_git_using_command branch' -s m -l move -d 'Rename branch'
6	6	-> complete -f -c git -n '__fish_git_using_command branch' -s M -d 'Force rename branch'
6	6	-> complete -f -c git -n '__fish_git_using_command branch' -s c -l copy -d 'Copy branch'
6	6	-> complete -f -c git -n '__fish_git_using_command branch' -s C -d 'Force copy branch'
7	7	-> complete -f -c git -n '__fish_git_using_command branch' -s a -l all -d 'Lists both local and remote branches'
9	9	-> complete -f -c git -n '__fish_git_using_command branch' -s r -l remotes -d 'List or delete (if used with -d) the remote-tracking branches.'
7	7	-> complete -f -c git -n '__fish_git_using_command branch' -s t -l track -l track -d 'Track remote branch'
6	6	-> complete -f -c git -n '__fish_git_using_command branch' -l no-track -d 'Do not track remote branch'
8	8	-> complete -f -c git -n '__fish_git_using_command branch' -l set-upstream-to -d 'Set remote branch to track' -ka '(__fish_git_branches)'
7	7	-> complete -f -c git -n '__fish_git_using_command branch' -l merged -d 'List branches that have been merged'
6	6	-> complete -f -c git -n '__fish_git_using_command branch' -l no-merged -d 'List branches that have not been merged'
6	6	-> complete -f -c git -n '__fish_git_using_command branch' -l unset-upstream -d 'Remove branch upstream information'
9	9	-> complete -f -c git -n '__fish_git_using_command branch' -l contains -d 'List branches that contain the specified commit' -xa '(__fish_git_commits)'
9	9	-> complete -f -c git -n '__fish_git_using_command branch' -l no-contains -d 'List branches that don\'t contain the specified commit' -xa '(__fish_git_commits)'
5	5	-> set -l bundlecommands create verify list-heads unbundle
6	6	-> complete -f -c git -n __fish_git_needs_command -a bundle -d 'Create, unpack, and manipulate "bundle" files'
21	21	-> complete -f -c git -n "__fish_git_using_command bundle" -n "not __fish_seen_subcommand_from $bundlecommands" -a "create\t'Create a bundle'
verify\t'Check that the bundle is valid and will apply cleanly'
list-heads\t'List the references defined in the bundle'
unbundle\t'Build a pack index file and print all defined references'"
10	10	-> complete -f -c git -n "__fish_git_using_command bundle" -n "__fish_seen_subcommand_from create verify" -s q -l quiet -d 'Do not show progress meter'
9	9	-> complete -f -c git -n "__fish_git_using_command bundle" -n "__fish_seen_subcommand_from create unbundle" -l progress -d 'Show progress meter'
8	8	-> complete -f -c git -n "__fish_git_using_command bundle" -n "__fish_seen_subcommand_from create" -l all-progress -d 'Show progress meter during object writing phase'
8	8	-> complete -f -c git -n "__fish_git_using_command bundle" -n "__fish_seen_subcommand_from create" -l all-progress-implied -d 'Similar to --all-progress when progress meter is shown'
10	10	-> complete -x -c git -n "__fish_git_using_command bundle" -n "__fish_seen_subcommand_from create" -l version -d 'Specify bundle format version'
9	9	-> complete -c git -n '__fish_git_using_command bundle' -n "__fish_seen_subcommand_from create" -ka '--all\t"All refs"'
12	12	-> complete -c git -n '__fish_git_using_command bundle' -n "__fish_seen_subcommand_from create" -ka '(__fish_git_ranges)'
6	6	-> complete -f -c git -n __fish_git_needs_command -a cherry -d 'Find commits yet to be applied to upstream'
6	6	-> complete -f -c git -n '__fish_git_using_command cherry' -s v -d 'Show the commit subjects next to the SHA1s'
7	7	-> complete -f -c git -n '__fish_git_using_command cherry' -ka '(__fish_git_refs)' -d Upstream
6	6	-> complete -f -c git -n __fish_git_needs_command -a cherry-pick -d 'Reapply a commit on another branch'
9	9	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -ka '(__fish_git_ranges)'
9	9	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -n __fish_git_possible_commithash -ka '(__fish_git_commits)'
8	8	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -s e -l edit -d 'Edit the commit message prior to committing'
6	6	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -s x -d 'Append info in generated commit on the origin of the cherry-picked change'
7	7	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -s n -l no-commit -d 'Apply changes without making any commit'
8	8	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -s s -l signoff -d 'Add Signed-off-by line to the commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -l ff -d 'Fast-forward if possible'
6	6	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -l continue -d 'Continue the operation in progress'
6	6	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -l abort -d 'Cancel the operation'
6	6	-> complete -f -c git -n '__fish_git_using_command cherry-pick' -l skip -d 'Skip the current commit and continue with the rest of the sequence'
6	6	-> complete -f -c git -n __fish_git_needs_command -a clone -d 'Clone a repository into a new directory'
6	6	-> complete -f -c git -n '__fish_git_using_command clone' -l no-hardlinks -d 'Copy files instead of using hardlinks'
6	6	-> complete -f -c git -n '__fish_git_using_command clone' -s q -l quiet -d 'Operate quietly and do not report progress'
6	6	-> complete -f -c git -n '__fish_git_using_command clone' -s v -l verbose -d 'Provide more information on what is going on'
7	7	-> complete -f -c git -n '__fish_git_using_command clone' -s n -l no-checkout -d 'No checkout of HEAD is performed after the clone is complete'
6	6	-> complete -f -c git -n '__fish_git_using_command clone' -l bare -d 'Make a bare Git repository'
7	7	-> complete -f -c git -n '__fish_git_using_command clone' -l mirror -d 'Set up a mirror of the source repository'
8	8	-> complete -f -c git -n '__fish_git_using_command clone' -s o -l origin -d 'Use a specific name of the remote instead of the default'
8	8	-> complete -f -c git -n '__fish_git_using_command clone' -s b -l branch -d 'Use a specific branch instead of the one used by the cloned repository'
6	6	-> complete -f -c git -n '__fish_git_using_command clone' -l depth -d 'Truncate the history to a specified number of revisions'
6	6	-> complete -f -c git -n '__fish_git_using_command clone' -l recursive -d 'Initialize all submodules within the cloned repository'
6	6	-> complete -c git -n __fish_git_needs_command -a commit -d 'Record changes to the repository'
6	6	-> complete -c git -n '__fish_git_using_command commit' -l amend -d 'Amend the log message of the last commit'
10	10	-> complete -f -c git -n '__fish_git_using_command commit' -a '(__fish_git_files modified deleted modified-staged-deleted untracked)'
7	7	-> complete -c git -n '__fish_git_using_command commit' -s a -l all -d 'Automatically stage modified and deleted files'
6	6	-> complete -c git -n '__fish_git_using_command commit' -s p -l patch -d 'Use interactive patch selection interface'
6	6	-> complete -f -c git -n '__fish_git_using_command commit' -l fixup -d 'Fixup commit to be used with rebase --autosquash'
8	8	-> complete -f -c git -n '__fish_git_using_command commit' -l squash -d 'Squash commit to be used with rebase --autosquash'
6	6	-> complete -c git -n '__fish_git_using_command commit' -l reset-author -d 'When amending, reset author of commit to the committer'
6	6	-> complete -x -c git -n '__fish_git_using_command commit' -l author -d 'Override the commit author'
16	16	-> complete -x -c git -n '__fish_git_using_command commit' -l cleanup -a "strip\t'Leading/trailing whitespace/empty lines, #commentary'
 whitespace\t'Like strip but keep #commentary'
 verbatim\t'Do not change the message'
 scissors\t'Like whitespace but also remove after scissor lines'
 default\t'Like strip if the message is to be edited, whitespace otherwise'" -d 'How to clean up the commit message'
6	6	-> complete -x -c git -n '__fish_git_using_command commit' -l date -d 'Override the author date'
7	7	-> complete -x -c git -n '__fish_git_using_command commit' -s m -l message -d 'Use the given message as the commit message'
7	7	-> complete -f -c git -n '__fish_git_using_command commit' -l no-edit -d 'Use the selected commit message without launching an editor'
6	6	-> complete -f -c git -n '__fish_git_using_command commit' -l no-gpg-sign -d 'Do not sign commit'
6	6	-> complete -f -c git -n '__fish_git_using_command commit' -s n -l no-verify -d 'Do not run pre-commit and commit-msg hooks'
10	10	-> complete -f -c git -n '__fish_git_using_command commit' -n '__fish_git_contains_opt fixup squash' -ka '(__fish_git_recent_commits)'
6	6	-> complete -f -c git -n '__fish_git_using_command commit' -l allow-empty -d 'Create a commit with no changes'
6	6	-> complete -f -c git -n '__fish_git_using_command commit' -l allow-empty-message -d 'Create a commit with no commit message'
7	7	-> complete -f -c git -n '__fish_git_using_command commit' -s s -l signoff -d 'Append Signed-off-by trailer to commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command commit' -l no-signoff -d 'Do not append Signed-off-by trailer to commit message'
6	6	-> complete -f -c git -n __fish_git_needs_command -a count-objects -d 'Count number of objects and disk consumption'
7	7	-> complete -f -c git -n '__fish_git_using_command count-objects' -s v -l verbose -d 'Be verbose'
10	10	-> complete -f -c git -n '__fish_git_using_command count-objects' -s H -l human-readable -d 'Print in human readable format'
6	6	-> complete -c git -n __fish_git_needs_command -a daemon -d 'A simple server for git repositories'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l strict-paths -d 'Match paths exactly'
6	6	-> complete -F -c git -n '__fish_git_using_command daemon' -l base-path -d 'Git Root'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l base-path-relaxed -d 'When looking up with base path fails, try without it'
6	6	-> complete -F -c git -n '__fish_git_using_command daemon' -l interpolated-path -d 'Construct a path from the given template'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l export-all -d 'Allow pulling from all directories'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l inetd -d 'Run as inetd service'
6	6	-> complete -x -c git -n '__fish_git_using_command daemon' -l listen -d 'Listen on this IP'
5	5	-> complete -x -c git -n '__fish_git_using_command daemon' -l port -d 'Listen on this port'
6	6	-> complete -x -c git -n '__fish_git_using_command daemon' -l init-timeout -d 'Connection timeout'
6	6	-> complete -x -c git -n '__fish_git_using_command daemon' -l timeout -d 'Timeout for each request'
6	6	-> complete -x -c git -n '__fish_git_using_command daemon' -l max-connections -d 'Maximum parallel clients'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l syslog -d '--log-destination=syslog'
13	13	-> complete -x -c git -n '__fish_git_using_command daemon' -l log-destination -a 'stderr syslog none' -d 'Log destination'
9	9	-> complete -x -c git -n '__fish_git_using_command daemon' -l user-path -d 'Allow ~user notation to be used'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l verbose -d 'Log all details'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l reuseaddr -d 'Reuse address when binding to listening server'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l detach -d 'Detach from shell'
6	6	-> complete -x -c git -n '__fish_git_using_command daemon' -l reuseaddr -d 'Save the process id in file'
6	6	-> complete -x -c git -n '__fish_git_using_command daemon' -l user -d 'Change daemon\'s uid'
6	6	-> complete -x -c git -n '__fish_git_using_command daemon' -l group -d 'Change daemon\'s gid'
9	9	-> complete -x -c git -n '__fish_git_using_command daemon' -l enable -a 'upload-pack upload-archive receive-pack' -d 'Enable service'
8	8	-> complete -x -c git -n '__fish_git_using_command daemon' -l disable -a 'upload-pack upload-archive receive-pack' -d 'Disable service'
8	8	-> complete -x -c git -n '__fish_git_using_command daemon' -l allow-override -a 'upload-pack upload-archive receive-pack' -d 'Allow overriding site-wide default per repository configuration'
9	9	-> complete -x -c git -n '__fish_git_using_command daemon' -l forbid-override -a 'upload-pack upload-archive receive-pack' -d 'Forbid overriding site-wide default per repository configuration'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l informative-errors -d 'Report more verbose errors to clients'
6	6	-> complete -f -c git -n '__fish_git_using_command daemon' -l no-informative-errors -d 'Report less verbose errors to clients'
6	6	-> complete -x -c git -n '__fish_git_using_command daemon' -l access-hook -d 'Hook to run whenever a client connects'
6	6	-> complete -c git -n __fish_git_needs_command -a describe -d 'Give an object a human readable name'
8	8	-> complete -f -c git -n '__fish_git_using_command describe' -ka '(__fish_git_tags)' -d Tag
9	9	-> complete -f -c git -n '__fish_git_using_command describe' -ka '(__fish_git_branches)'
7	7	-> complete -f -c git -n '__fish_git_using_command describe' -ka '(__fish_git_heads)' -d Head
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l dirty -d 'Describe the state of the working tree, append dirty if there are local changes'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l broken -d 'Describe the state of the working tree, append -broken instead of erroring'
10	10	-> complete -f -c git -n '__fish_git_using_command describe' -l all -d 'Use all tags, not just annotated'
7	7	-> complete -f -c git -n '__fish_git_using_command describe' -l tags -d 'Use all commits/tags, not just annotated tags'
7	7	-> complete -f -c git -n '__fish_git_using_command describe' -l contains -d 'Find the tag that comes after the commit'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l abbrev -d 'Use <n> digits, or as many digits as needed to form a unique object name'
8	8	-> complete -f -c git -n '__fish_git_using_command describe' -l candidates -d 'Consider up to <n> candidates'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l exact-match -d 'Only output exact matches'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l debug -d 'Display debug info'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l long -d 'Always output the long format'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l match -d 'Only consider tags matching the given glob pattern'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l exclude -d 'Do not consider tags matching the given glob pattern'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l always -d 'Show uniquely abbreviated commit object as fallback'
6	6	-> complete -f -c git -n '__fish_git_using_command describe' -l first-parent -d 'Follow only the first parent of a merge commit'
6	6	-> complete -c git -n __fish_git_needs_command -a diff -d 'Show changes between commits and working tree'
12	12	-> complete -c git -n '__fish_git_using_command diff' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_ranges)'
12	12	-> complete -c git -n '__fish_git_using_command diff' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_complete_stashes)'
6	6	-> complete -c git -n '__fish_git_using_command diff' -l cached -d 'Show diff of changes in the index'
6	6	-> complete -c git -n '__fish_git_using_command diff' -l staged -d 'Show diff of changes in the index'
6	6	-> complete -c git -n '__fish_git_using_command diff' -l no-index -d 'Compare two paths on the filesystem'
6	6	-> complete -c git -n '__fish_git_using_command diff' -l exit-code -d 'Exit with 1 if there were differences or 0 if no differences'
6	6	-> complete -c git -n '__fish_git_using_command diff' -l quiet -d 'Disable all output of the program, implies --exit-code'
36	36	-> complete -c git -n '__fish_git_using_command diff' -s 1 -l base -d 'Compare the working tree with the "base" version'
8	8	-> complete -c git -n '__fish_git_using_command diff' -s 2 -l ours -d 'Compare the working tree with the "our branch"'
7	7	-> complete -c git -n '__fish_git_using_command diff' -s 3 -l theirs -d 'Compare the working tree with the "their branch"'
7	7	-> complete -c git -n '__fish_git_using_command diff' -s 0 -d 'Omit diff output for unmerged entries and just show "Unmerged"'
27	27	-> complete -c git -n '__fish_git_using_command diff' -n 'not __fish_git_contains_opt cached staged' -a '(
    set -l kinds modified
    contains -- -- (commandline -opc) && set -a kinds deleted modified-staged-deleted
    __fish_git_files $kinds
)'
11	11	-> complete -c git -n '__fish_git_using_command diff' -n '__fish_git_contains_opt cached staged' -fa '(__fish_git_files all-staged)'
2	2	-> function __fish_git_diffmerge_tools -a cmd...
6	6	-> complete -c git -n __fish_git_needs_command -a difftool -d 'Open diffs in a visual tool'
14	14	-> complete -c git -n '__fish_git_using_command difftool' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_ranges)'
6	6	-> complete -c git -n '__fish_git_using_command difftool' -l cached -d 'Visually show diff of changes in the index'
19	19	-> complete -f -c git -n '__fish_git_using_command difftool' -a '(
    set -l kinds modified
    contains -- -- (commandline -opc) && set -a kinds deleted modified-staged-deleted
    __fish_git_files $kinds
)'
7	7	-> complete -f -c git -n '__fish_git_using_command difftool' -s g -l gui -d 'Use `diff.guitool` instead of `diff.tool`'
7	7	-> complete -f -c git -n '__fish_git_using_command difftool' -s d -l dir-diff -d 'Perform a full-directory diff'
6	6	-> complete -c git -n '__fish_git_using_command difftool' -l prompt -d 'Prompt before each invocation of the diff tool'
6	6	-> complete -f -c git -n '__fish_git_using_command difftool' -s y -l no-prompt -d 'Do not prompt before launching a diff tool'
6	6	-> complete -f -c git -n '__fish_git_using_command difftool' -l symlinks -d 'Use symlinks in dir-diff mode'
10	10	-> complete -f -c git -n '__fish_git_using_command difftool' -s t -l tool -d 'Use the specified diff tool' -a "(__fish_git_diffmerge_tools difftool)"
7	7	-> complete -f -c git -n '__fish_git_using_command difftool' -l tool-help -d 'Print a list of diff tools that may be used with `--tool`'
6	6	-> complete -f -c git -n '__fish_git_using_command difftool' -l trust-exit-code -d 'Exit when an invoked diff tool returns a non-zero exit code'
7	7	-> complete -f -c git -n '__fish_git_using_command difftool' -s x -l extcmd -d 'Specify a custom command for viewing diffs'
6	6	-> complete -f -c git -n '__fish_git_using_command difftool' -l no-gui -d 'Overrides --gui setting'
6	6	-> complete -f -c git -n __fish_git_needs_command -a gc -d 'Collect garbage (unreachable commits etc)'
6	6	-> complete -f -c git -n '__fish_git_using_command gc' -l aggressive -d 'Aggressively optimize the repository'
6	6	-> complete -f -c git -n '__fish_git_using_command gc' -l auto -d 'Checks any housekeeping is required and then run'
6	6	-> complete -f -c git -n '__fish_git_using_command gc' -l prune -d 'Prune loose objects older than date'
6	6	-> complete -f -c git -n '__fish_git_using_command gc' -l no-prune -d 'Do not prune any loose objects'
6	6	-> complete -f -c git -n '__fish_git_using_command gc' -l quiet -d 'Be quiet'
5	5	-> complete -f -c git -n '__fish_git_using_command gc' -l force -d 'Force `git gc` to run'
6	6	-> complete -f -c git -n '__fish_git_using_command gc' -l keep-largest-pack -d 'Ignore `gc.bigPackThreshold`'
5	5	-> complete -c git -n __fish_git_needs_command -a grep -d 'Print lines matching a pattern'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l cached -d 'Search blobs registered in the index file'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l no-index -d 'Search files in the current directory not managed by Git'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l untracked -d 'Search also in untracked files'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l no-exclude-standard -d 'Also search in ignored files by not honoring the .gitignore mechanism'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l exclude-standard -d 'Do not search ignored files specified via the .gitignore mechanism'
8	8	-> complete -f -c git -n '__fish_git_using_command grep' -l recurse-submodules -d 'Recursively search in each submodule that is active and checked out in the repository'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s a -l text -d 'Process binary files as if they were text'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l textconv -d 'Honor textconv filter settings'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l no-textconv -d 'Do not honor textconv filter settings'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s i -l ignore-case -d 'Ignore case differences between the patterns and the files'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -s I -d 'Don\'t match the pattern in binary files'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s r -l recursive -d 'Descend into levels of directories endlessly'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l no-recursive -d 'Do not descend into directories'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -s w -l word-regexp -d 'Match the pattern only at word boundary'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -s v -l invert-match -d 'Select non-matching lines'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -l full-name -d 'Forces paths to be output relative to the project top directory'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s E -l extended-regexp -d 'Use POSIX extended regexp for patterns'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s G -l basic-regexp -d 'Use POSIX basic regexp for patterns'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s P -l perl-regexp -d 'Use Perl-compatible regular expressions for patterns'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s F -l fixed-strings -d 'Don\'t interpret pattern as a regex'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s n -l line-number -d 'Prefix the line number to matching lines'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -l column -d 'Prefix the 1-indexed byte-offset of the first match from the start of the matching line'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s l -l files-with-matches -d 'Show only the names of files that contain matches'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s L -l files-without-match -d 'Show only the names of files that do not contain matches'
9	9	-> complete -f -c git -n '__fish_git_using_command grep' -s z -l null -d 'Use \\0 as the delimiter for pathnames in the output, and print them verbatim'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s o -l only-matching -d 'Print only the matched parts of a matching line'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -s c -l count -d 'Instead of showing every matched line, show the number of lines that match'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l no-color -d 'Turn off match highlighting, even when the configuration file gives the default to color output'
8	8	-> complete -f -c git -n '__fish_git_using_command grep' -l break -d 'Print an empty line between matches from different files'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -l heading -d 'Show the filename above the matches in that file instead of at the start of each shown line'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s p -l show-function -d 'Show the line that contains the function name of the match, unless the match is a function name itself'
8	8	-> complete -f -c git -n '__fish_git_using_command grep' -s W -l function-context -d 'Show the surrounding text from the line containing a function name up to the one before the next function name'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -s e -d 'The next parameter is the pattern'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l and -d 'Combine patterns using and'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l or -d 'Combine patterns using or'
6	6	-> complete -f -c git -n '__fish_git_using_command grep' -l not -d 'Combine patterns using not'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -l all-match -d 'Only match files that can match all the pattern expressions when giving multiple'
7	7	-> complete -f -c git -n '__fish_git_using_command grep' -s q -l quiet -d 'Just exit with status 0 when there is a match and with non-zero status when there isn\'t'
14	14	-> complete -c git -n '__fish_git_using_command grep' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_refs)'
7	7	-> complete -f -c git -n __fish_git_needs_command -a init -d 'Create an empty git repository'
7	7	-> complete -f -c git -n '__fish_git_using_command init' -s q -l quiet -d 'Only print error and warning messages'
6	6	-> complete -f -c git -n '__fish_git_using_command init' -l bare -d 'Create a bare repository'
7	7	-> complete -c git -n __fish_git_needs_command -a shortlog -d 'Show commit shortlog'
6	6	-> complete -c git -n __fish_git_needs_command -a log -d 'Show commit logs'
8	8	-> complete -c git -n '__fish_git_using_command log' -a '(__fish_git ls-files)'
11	11	-> complete -c git -n '__fish_git_using_command log' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_ranges)'
7	7	-> complete -c git -n '__fish_git_using_command log' -l follow -d 'Continue listing file history beyond renames'
6	6	-> complete -c git -n '__fish_git_using_command log' -l no-decorate -d 'Don\'t print ref names'
15	15	-> complete -f -c git -n '__fish_git_using_command log' -l decorate -a 'short\tHide\ prefixes full\tShow\ full\ ref\ names auto\tHide\ prefixes\ if\ printed\ to\ terminal no\tDon\\\'t\ display\ ref' -d 'Print out ref names'
6	6	-> complete -c git -n '__fish_git_using_command log' -l source -d 'Print ref name by which each commit was reached'
7	7	-> complete -c git -n '__fish_git_using_command log' -l use-mailmap
5	5	-> complete -c git -n '__fish_git_using_command log' -l full-diff
5	5	-> complete -c git -n '__fish_git_using_command log' -l log-size
6	6	-> complete -x -c git -n '__fish_git_using_command log' -s L
7	7	-> complete -x -c git -n '__fish_git_using_command log' -s n -l max-count -d 'Limit the number of commits before starting to show the commit output'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l skip -d 'Skip given number of commits'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l since -d 'Show commits more recent than specified date'
7	7	-> complete -x -c git -n '__fish_git_using_command log' -l after -d 'Show commits more recent than specified date'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l until -d 'Show commits older than specified date'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l before -d 'Show commits older than specified date'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l author -d 'Limit commits from given author'
7	7	-> complete -x -c git -n '__fish_git_using_command log' -l committer -d 'Limit commits from given committer'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l grep-reflog -d 'Limit commits to ones with reflog entries matching given pattern'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l grep -d 'Limit commits with message that match given pattern'
6	6	-> complete -c git -n '__fish_git_using_command log' -l all-match -d 'Limit commits to ones that match all given --grep'
6	6	-> complete -c git -n '__fish_git_using_command log' -l invert-grep -d 'Limit commits to ones with message that don\'t match --grep'
7	7	-> complete -c git -n '__fish_git_using_command log' -l regexp-ignore-case -s i -d 'Case insensitive match'
6	6	-> complete -c git -n '__fish_git_using_command log' -l basic-regexp -d 'Patterns are basic regular expressions (default)'
6	6	-> complete -c git -n '__fish_git_using_command log' -l extended-regexp -s E -d 'Patterns are extended regular expressions'
7	7	-> complete -c git -n '__fish_git_using_command log' -l fixed-strings -s F -d 'Patterns are fixed strings'
6	6	-> complete -c git -n '__fish_git_using_command log' -l perl-regexp -d 'Patterns are Perl-compatible regular expressions'
5	5	-> complete -c git -n '__fish_git_using_command log' -l remove-empty -d 'Stop when given path disappears from tree'
7	7	-> complete -c git -n '__fish_git_using_command log' -l merges -d 'Print only merge commits'
9	9	-> complete -c git -n '__fish_git_using_command log' -l no-merges -d 'Don\'t print commits with more than one parent'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l min-parents -d 'Show only commit with at least the given number of parents'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l max-parents -d 'Show only commit with at most the given number of parents'
8	8	-> complete -c git -n '__fish_git_using_command log' -l no-min-parents -d 'Show only commit without a minimum number of parents'
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-max-parents -d 'Show only commit without a maximum number of parents'
7	7	-> complete -c git -n '__fish_git_using_command log' -l first-parent -d 'Follow only the first parent commit upon seeing a merge commit'
7	7	-> complete -c git -n '__fish_git_using_command log' -l not -d 'Reverse meaning of ^ prefix'
6	6	-> complete -c git -n '__fish_git_using_command log' -l all -d 'Show log for all branches, tags, and remotes'
6	6	-> complete -f -c git -n '__fish_git_using_command log' -l branches -d 'Show log for all matching branches'
6	6	-> complete -f -c git -n '__fish_git_using_command log' -l tags -d 'Show log for all matching tags'
6	6	-> complete -f -c git -n '__fish_git_using_command log' -l remotes -d 'Show log for all matching remotes'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l glob -d 'Show log for all matching branches, tags, and remotes'
6	6	-> complete -x -c git -n '__fish_git_using_command log' -l exclude -d 'Do not include refs matching given glob pattern'
6	6	-> complete -c git -n '__fish_git_using_command log' -l reflog -d 'Show log for all reflogs entries'
5	5	-> complete -c git -n '__fish_git_using_command log' -l ingnore-missing -d 'Ignore invalid object names'
5	5	-> complete -c git -n '__fish_git_using_command log' -l bisect
7	7	-> complete -c git -n '__fish_git_using_command log' -l stdin -d 'Read commits from stdin'
8	8	-> complete -c git -n '__fish_git_using_command log' -l cherry-mark -d 'Mark equivalent commits with = and inequivalent with +'
6	6	-> complete -c git -n '__fish_git_using_command log' -l cherry-pick -d 'Omit equivalent commits'
5	5	-> complete -c git -n '__fish_git_using_command log' -l left-only
5	5	-> complete -c git -n '__fish_git_using_command log' -l right-only
6	6	-> complete -c git -n '__fish_git_using_command log' -l cherry
5	5	-> complete -c git -n '__fish_git_using_command log' -l walk-reflogs -s g
5	5	-> complete -c git -n '__fish_git_using_command log' -l merge
5	5	-> complete -c git -n '__fish_git_using_command log' -l boundary
5	5	-> complete -c git -n '__fish_git_using_command log' -l simplify-by-decoration
5	5	-> complete -c git -n '__fish_git_using_command log' -l full-history
5	5	-> complete -c git -n '__fish_git_using_command log' -l dense
5	5	-> complete -c git -n '__fish_git_using_command log' -l sparse
5	5	-> complete -c git -n '__fish_git_using_command log' -l simplify-merges
5	5	-> complete -c git -n '__fish_git_using_command log' -l ancestry-path
5	5	-> complete -c git -n '__fish_git_using_command log' -l date-order
5	5	-> complete -c git -n '__fish_git_using_command log' -l author-date-order
5	5	-> complete -c git -n '__fish_git_using_command log' -l topo-order
5	5	-> complete -c git -n '__fish_git_using_command log' -l reverse
8	8	-> complete -f -c git -n '__fish_git_using_command log' -l no-walk -a "sorted unsorted"
7	7	-> complete -c git -n '__fish_git_using_command log' -l do-walk
6	6	-> complete -c git -n '__fish_git_using_command log' -l format
5	5	-> complete -c git -n '__fish_git_using_command log' -l abbrev-commit
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-abbrev-commit
5	5	-> complete -c git -n '__fish_git_using_command log' -l oneline
8	8	-> complete -x -c git -n '__fish_git_using_command log' -l encoding -a '(__fish_print_encodings)'
6	6	-> complete -f -c git -n '__fish_git_using_command log' -l expand-tabs
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-expand-tabs
7	7	-> complete -f -c git -n '__fish_git_using_command log' -l notes
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-notes
5	5	-> complete -f -c git -n '__fish_git_using_command log' -l show-notes
5	5	-> complete -c git -n '__fish_git_using_command log' -l standard-notes
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-standard-notes
5	5	-> complete -c git -n '__fish_git_using_command log' -l show-signature
5	5	-> complete -c git -n '__fish_git_using_command log' -l relative-date
21	21	-> complete -x -c git -n '__fish_git_using_command log' -l date -a '
  relative
  local
  iso
  iso-local
  iso8601
  iso8601-local
  iso-strict
  iso-strict-local
  iso8601-strict
  iso8601-strict-local
  rfc-local
  rfc2822-local
  short
  short-local
  raw
  human
  unix
  format:
  default
  default-local
'
6	6	-> complete -c git -n '__fish_git_using_command log' -l parents
5	5	-> complete -c git -n '__fish_git_using_command log' -l children
5	5	-> complete -c git -n '__fish_git_using_command log' -l left-right
5	5	-> complete -c git -n '__fish_git_using_command log' -l graph
5	5	-> complete -f -c git -n '__fish_git_using_command log' -l show-linear-break
6	6	-> complete -c git -n '__fish_git_using_command log' -s c
5	5	-> complete -c git -n '__fish_git_using_command log' -l cc
5	5	-> complete -c git -n '__fish_git_using_command log' -s m
5	5	-> complete -c git -n '__fish_git_using_command log' -s r
7	7	-> complete -c git -n '__fish_git_using_command log' -s t
6	6	-> complete -c git -n '__fish_git_using_command log' -l patch -s p
5	5	-> complete -c git -n '__fish_git_using_command log' -s u
6	6	-> complete -c git -n '__fish_git_using_command log' -l no-patch -s s
8	8	-> complete -x -c git -n '__fish_git_using_command log' -l unified -s U
5	5	-> complete -c git -n '__fish_git_using_command log' -l raw
5	5	-> complete -c git -n '__fish_git_using_command log' -l patch-with-raw
5	5	-> complete -c git -n '__fish_git_using_command log' -l indent-heuristic
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-indent-heuristic
5	5	-> complete -c git -n '__fish_git_using_command log' -l compaction-heuristic
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-compaction-heuristic
5	5	-> complete -c git -n '__fish_git_using_command log' -l minimal
5	5	-> complete -c git -n '__fish_git_using_command log' -l patience
5	5	-> complete -c git -n '__fish_git_using_command log' -l histogram
6	6	-> complete -f -x -c git -n '__fish_git_using_command log' -l stat
5	5	-> complete -c git -n '__fish_git_using_command log' -l numstat
5	5	-> complete -c git -n '__fish_git_using_command log' -l shortstat
5	5	-> complete -c git -n '__fish_git_using_command log' -l summary
5	5	-> complete -c git -n '__fish_git_using_command log' -l patch-with-stat
5	5	-> complete -c git -n '__fish_git_using_command log' -s z
5	5	-> complete -c git -n '__fish_git_using_command log' -l name-only
5	5	-> complete -c git -n '__fish_git_using_command log' -l name-status
7	7	-> complete -f -c git -n '__fish_git_using_command log' -l color -a 'always never auto'
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-color
8	8	-> complete -f -c git -n '__fish_git_using_command log' -l word-diff -a '
  color
  plain
  porcelain
  none
'
5	5	-> complete -f -c git -n '__fish_git_using_command log' -l color-words
5	5	-> complete -c git -n '__fish_git_using_command log' -l no-renames
5	5	-> complete -c git -n '__fish_git_using_command log' -l check
5	5	-> complete -c git -n '__fish_git_using_command log' -l full-index
5	5	-> complete -c git -n '__fish_git_using_command log' -l binary
6	6	-> complete -f -c git -n '__fish_git_using_command log' -l abbrev
5	5	-> complete -f -c git -n '__fish_git_using_command log' -s l
1	1	-> function __fish__git_append_letters_nosep...
25	25	-> complete -x -c git -n '__fish_git_using_command log' -l diff-filter -a '(__fish__git_append_letters_nosep a\tExclude\ added c\tExclude\ copied d\tExclude\ deleted m\tExclude\ modified r\tExclude\ renamed t\tExclude\ type\ changed u\tExclude\ unmerged x\tExclude\ unknown b\tExclude\ broken A\tAdded C\tCopied D\tDeleted M\tModified R\tRenamed T\tType\ Changed U\tUnmerged X\tUnknown B\tBroken)'
6	6	-> complete -c git -n __fish_git_needs_command -a ls-files -d 'Show information about files'
5	5	-> complete -c git -n '__fish_git_using_command ls-files'
8	8	-> complete -c git -n '__fish_git_using_command ls-files' -s c -l cached -d 'Show cached files in the output'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -s d -l deleted -d 'Show deleted files in the output'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -s m -l modified -d 'Show modified files in the output'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -s o -l others -d 'Show other (i.e. untracked) files in the output'
7	7	-> complete -c git -n '__fish_git_using_command ls-files' -s i -l ignored -d 'Show only ignored files in the output'
8	8	-> complete -c git -n '__fish_git_using_command ls-files' -s s -l stage -d "Show staged contents' mode bits, object name and stage number in the output"
7	7	-> complete -c git -n '__fish_git_using_command ls-files' -l directory -d 'If a whole directory is classified as "other", show just its name'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -l no-empty-directory -d 'Do not list empty directories'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -s u -l unmerged -d 'Show unmerged files in the output'
7	7	-> complete -c git -n '__fish_git_using_command ls-files' -s k -l killed -d 'Show files on the filesystem that need to be removed for checkout-index to succeed'
5	5	-> complete -c git -n '__fish_git_using_command ls-files' -s z -d 'Use \0 delimiter'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -s x -l exclude -d 'Skip untracked files matching pattern'
8	8	-> complete -c git -n '__fish_git_using_command ls-files' -s X -l exclude-from -d 'Read exclude patterns from <file>; 1 per line'
7	7	-> complete -c git -n '__fish_git_using_command ls-files' -l exclude-per-directory -d 'Read extra exclude patterns that apply only to the dir and its subdirs in <file>'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -l exclude-standard -d 'Add the standard Git exclusions'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -l error-unmatch -d 'If any <file> does not appear in the index, treat this as an error'
7	7	-> complete -c git -n '__fish_git_using_command ls-files' -l with-tree
5	5	-> complete -c git -n '__fish_git_using_command ls-files' -s t -d 'Identifies the file status'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -s v -d 'Show file status, use lowercase letters for files assumed unchanged'
5	5	-> complete -c git -n '__fish_git_using_command ls-files' -s f -d 'Show file status, use lowercase letters for files marked as fsmonitor valid'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -l full-name -d 'Force paths to be output relative to the project top directory'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -l recurse-submodules -d 'Recursively calls ls-files on each submodule in the repository'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -l abbrev -d 'Show only a partial prefix'
8	8	-> complete -c git -n '__fish_git_using_command ls-files' -l debug -d 'After each line that describes a file, add more data about its cache entry'
6	6	-> complete -c git -n '__fish_git_using_command ls-files' -l eol -d 'Show <eolinfo> and <eolattr> of files'
8	8	-> complete -f -c git -n __fish_git_needs_command -a mailinfo -d 'Extracts patch and authorship from an e-mail'
7	7	-> complete -f -c git -n '__fish_git_using_command mailinfo am' -s k -d 'Do not remove email cruft from subject'
7	7	-> complete -f -c git -n '__fish_git_using_command mailinfo' -s b -d 'Only strip bracket pairs containing \'PATCH\''
7	7	-> complete -f -c git -n '__fish_git_using_command mailinfo am' -s u -d 'Do not reencode author name and email'
8	8	-> complete -x -c git -n '__fish_git_using_command mailinfo' -l encoding -d 'Re-encode to given charset'
7	7	-> complete -f -c git -n '__fish_git_using_command mailinfo' -s n -d 'Disable all charset re-encoding of metadata'
7	7	-> complete -f -c git -n '__fish_git_using_command mailinfo am' -s m -l message-id -d 'Copy message id to the end of commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command mailinfo' -l scissors -d 'Remove everything above scissor line'
6	6	-> complete -f -c git -n '__fish_git_using_command mailinfo' -l no-scissors -d 'Ignore scissor lines'
9	9	-> complete -x -c git -n '__fish_git_using_command mailinfo' -l quoted-cr -a 'nowarn warn strip' -d 'Action when processed email message end with CRLF instead of LF'
6	6	-> complete -c git -n __fish_git_needs_command -a mailsplit -d 'mbox splitter'
6	6	-> complete -F -c git -n '__fish_git_using_command mailsplit' -s o -d 'Directory to place individual messages'
6	6	-> complete -f -c git -n '__fish_git_using_command mailsplit' -s b -d 'Treat files not starting with From line as single mail message'
6	6	-> complete -x -c git -n '__fish_git_using_command mailsplit' -s d -d 'File name prefix digit precision'
6	6	-> complete -x -c git -n '__fish_git_using_command mailsplit' -s f -d 'Skip first n numbers'
6	6	-> complete -f -c git -n '__fish_git_using_command mailsplit am' -l keep-cr -d 'Do not remove \\r from lines starting with \\n\\r'
6	6	-> complete -f -c git -n '__fish_git_using_command mailsplit' -l mboxrd -d 'Input is of mboxrd form'
7	7	-> complete -f -c git -n __fish_git_needs_command -a maintenance -d 'Run tasks to optimize Git repository data'
7	7	-> complete -f -c git -n '__fish_git_using_command maintenance' -a register -d 'Initialize Git config vars for maintenance'
7	7	-> complete -f -c git -n '__fish_git_using_command maintenance' -a run -d 'Run one or more maintenance tasks'
7	7	-> complete -f -c git -n '__fish_git_using_command maintenance' -a start -d 'Start maintenance'
6	6	-> complete -f -c git -n '__fish_git_using_command maintenance' -a stop -d 'Halt background maintenance'
7	7	-> complete -f -c git -n '__fish_git_using_command maintenance' -a unregister -d 'Remove repository from background maintenance'
6	6	-> complete -f -c git -n '__fish_git_using_command maintenance' -l quiet -d 'Supress logs'
9	9	-> complete -x -c git -n '__fish_git_using_command maintenance' -l task -a 'commit-graph prefetch gc loose-objects incremental-repack pack-refs' -d 'Tasks to run'
7	7	-> complete -f -c git -n '__fish_git_using_command maintenance' -l auto -d 'Run maintenance only when necessary'
6	6	-> complete -f -c git -n '__fish_git_using_command maintenance' -l schedule -d 'Run maintenance on certain intervals'
6	6	-> complete -f -c git -n __fish_git_needs_command -a merge -d 'Join multiple development histories'
10	10	-> complete -f -c git -n '__fish_git_using_command merge' -ka '(__fish_git_branches)'
8	8	-> complete -f -c git -n '__fish_git_using_command merge' -l commit -d "Autocommit the merge"
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l no-commit -d "Don't autocommit the merge"
7	7	-> complete -f -c git -n '__fish_git_using_command merge' -s e -l edit -d 'Edit auto-generated merge message'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l no-edit -d "Don't edit auto-generated merge message"
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l ff -d "Don't generate a merge commit if merge is fast-forward"
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l no-ff -d "Generate a merge commit even if merge is fast-forward"
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l ff-only -d 'Refuse to merge unless fast-forward possible'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -s S -l gpg-sign -d 'GPG-sign the merge commit'
8	8	-> complete -f -c git -n '__fish_git_using_command merge' -l log -d 'Populate the log message with one-line descriptions'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l no-log -d "Don't populate the log message with one-line descriptions"
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l signoff -d 'Add Signed-off-by line at the end of the merge commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l no-signoff -d 'Do not add a Signed-off-by line at the end of the merge commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l stat -d "Show diffstat of the merge"
8	8	-> complete -f -c git -n '__fish_git_using_command merge' -s n -l no-stat -d "Don't show diffstat of the merge"
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l squash -d "Squash changes from other branch as a single commit"
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l no-squash -d "Don't squash changes"
6	6	-> complete -x -c git -n '__fish_git_using_command merge' -s s -l strategy -d 'Use the given merge strategy'
8	8	-> complete -r -c git -n '__fish_git_using_command merge' -s X -l strategy-option -d 'Pass given option to the merge strategy'
7	7	-> complete -f -c git -n '__fish_git_using_command merge' -l verify-signatures -d 'Abort merge if other branch tip commit is not signed with a valid key'
7	7	-> complete -f -c git -n '__fish_git_using_command merge' -l no-verify-signatures -d 'Do not abort merge if other branch tip commit is not signed with a valid key'
10	10	-> complete -f -c git -n '__fish_git_using_command merge' -s q -l quiet -d 'Be quiet'
7	7	-> complete -f -c git -n '__fish_git_using_command merge' -s v -l verbose -d 'Be verbose'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l progress -d 'Force progress status'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l no-progress -d 'Force no progress status'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l allow-unrelated-histories -d 'Allow merging even when branches do not share a common history'
7	7	-> complete -x -c git -n '__fish_git_using_command merge' -s m -d 'Set the commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l rerere-autoupdate -d 'If possible, use previous conflict resolutions'
6	6	-> complete -f -c git -n '__fish_git_using_command merge' -l no-rerere-autoupdate -d 'Do not use previous conflict resolutions'
7	7	-> complete -f -c git -n '__fish_git_using_command merge' -l abort -d 'Abort the current conflict resolution process'
8	8	-> complete -f -c git -n '__fish_git_using_command merge' -l continue -d 'Conclude current conflict resolution process'
6	6	-> complete -f -c git -n __fish_git_needs_command -a merge-base -d 'Find a common ancestor for a merge'
7	7	-> complete -f -c git -n '__fish_git_using_command merge-base' -ka '(__fish_git_branches)'
7	7	-> complete -f -c git -n '__fish_git_using_command merge-base' -s a -l all -d 'Output all merge bases for the commits, instead of just one'
8	8	-> complete -f -c git -n '__fish_git_using_command merge-base' -l octopus -d 'Compute the best common ancestors of all supplied commits'
6	6	-> complete -f -c git -n '__fish_git_using_command merge-base' -l independent -d 'Print a minimal subset of the supplied commits with the same ancestors'
6	6	-> complete -f -c git -n '__fish_git_using_command merge-base' -l is-ancestor -d 'Check if the first commit is an ancestor of the second commit'
6	6	-> complete -f -c git -n '__fish_git_using_command merge-base' -l fork-point -d 'Find the point at which a branch forked from another branch ref'
6	6	-> complete -f -c git -n __fish_git_needs_command -a mergetool -d 'Run merge conflict resolution tool'
11	11	-> complete -f -c git -n '__fish_git_using_command mergetool' -s t -l tool -d "Use specific merge resolution program" -a "(__fish_git_diffmerge_tools mergetool)"
8	8	-> complete -f -c git -n '__fish_git_using_command mergetool' -l tool-help -d 'Print a list of merge tools that may be used with `--tool`'
10	10	-> complete -f -c git -n '__fish_git_using_command mergetool' -a "(__fish_git_files unmerged)"
7	7	-> complete -f -c git -n '__fish_git_using_command mergetool' -s y -l no-prompt -d 'Do not prompt before launching a diff tool'
6	6	-> complete -f -c git -n '__fish_git_using_command mergetool' -l prompt -d 'Prompt before each invocation of the merge resolution program'
6	6	-> complete -c git -n '__fish_git_using_command mergetool' -s O -d 'Process files in the order specified in the file passed as argument'
5	5	-> complete -c git -n __fish_git_needs_command -a mv -d 'Move or rename a file'
8	8	-> complete -f -c git -n '__fish_git_using_command mv' -a '(__fish_git ls-files)'
7	7	-> complete -f -c git -n '__fish_git_using_command mv' -s f -l force -d 'Force rename/moving even if target exists'
6	6	-> complete -f -c git -n '__fish_git_using_command mv' -s k -d 'Skip rename/move which can lead to error'
7	7	-> complete -f -c git -n '__fish_git_using_command mv' -s n -l dry-run -d 'Only show what would happen'
7	7	-> complete -f -c git -n '__fish_git_using_command mv' -s v -l verbose -d 'Report names of files as they are changed'
6	6	-> set -l notescommands add copy append edit show merge remove
6	6	-> complete -c git -n __fish_git_needs_command -a notes -d 'Add or inspect object notes'
15	15	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a list -d 'List notes for given object'
12	12	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a add -d 'Add notes for a given object'
11	11	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a copy -d 'Copy notes from object1 to object2'
14	14	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a append -d 'Append to the notes of existing object'
11	11	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a edit -d 'Edit notes for a given object'
11	11	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a show -d 'Show notes for given object'
13	13	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a merge -d 'Merge the given notes ref to current notes ref'
11	11	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a remove -d 'Remove notes for given object'
11	11	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a prune -d 'Remove notes for non-existing/unreachable objects'
11	11	-> complete -f -c git -n "__fish_git_using_command notes" -n "not __fish_seen_subcommand_from $notescommands" -a get-ref -d 'Print current notes ref'
12	12	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from $notescommands" -ka '(__fish_git_commits)'
9	9	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from add copy" -s f -l force -d 'Overwrite existing notes'
9	9	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from add append edit" -l allow-empty -d 'Allow empty note'
9	9	-> complete -r -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from add append" -s F -l file -d 'Read note message from file'
10	10	-> complete -x -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from add append" -s m -l message -d 'Use this note message'
11	11	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from add append" -s C -l reuse-message -a '(__fish_git_commits)' -d 'Copy note from object'
13	13	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from add append" -s c -l reedit-message -a '(__fish_git_commits)' -d 'Copy and edit note from object'
8	8	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from copy remove" -l stdin -d 'Read object names from stdin'
10	10	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from merge remove prune" -s v -l verbose -d 'Be more verbose'
11	11	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from merge remove prune" -s q -l quiet -d 'Operate quietly'
20	20	-> complete -x -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from merge" -s s -l strategy -d 'Merge strategy to use to resolve conflicts' -a "
	manual\t'Instruct the user to resolve merge conflicts'
	ours\t'Resolve conflicts in favour of local version'
	theirs\t'Resolve conflicts in favour of remote version'
	union\t'Resolve conflicts by concatenating local and remote versions'
	cat_sort_uniq\t'Concatenate, sort and remove duplicate lines'
	"
9	9	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from merge" -l commit -d 'Finalize git notes merge'
9	9	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from merge" -l abort -d 'Abort git notes merge'
9	9	-> complete -f -c git -n "__fish_git_using_command notes" -n "__fish_seen_subcommand_from remove" -l ignore-missing -d 'Do not throw error on deleting non-existing object note'
7	7	-> complete -f -c git -n __fish_git_needs_command -a prune -d 'Prune unreachable objects from the database'
7	7	-> complete -f -c git -n '__fish_git_using_command prune' -s n -l dry-run -d 'Just report what it would remove'
7	7	-> complete -f -c git -n '__fish_git_using_command prune' -s v -l verbose -d 'Report all removed objects'
6	6	-> complete -f -c git -n '__fish_git_using_command prune' -l progress -d 'Show progress'
6	6	-> complete -f -c git -n __fish_git_needs_command -a pull -d 'Fetch from and merge with another repo or branch'
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -s q -l quiet -d 'Be quiet'
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -s v -l verbose -d 'Be verbose'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l all -d 'Fetch all remotes'
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -s a -l append -d 'Append ref names and object names'
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -s f -l force -d 'Force update of local branches'
8	8	-> complete -f -c git -n '__fish_git_using_command pull' -s k -l keep -d 'Keep downloaded pack'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l no-tags -d 'Disable automatic tag following'
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -s p -l prune -d 'Remove remote-tracking references that no longer exist on the remote'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l progress -d 'Force progress status'
12	12	-> complete -f -c git -n '__fish_git_using_command pull' -n 'not __fish_git_branch_for_remote' -a '(__fish_git_remotes)' -d 'Remote alias'
9	9	-> complete -f -c git -n '__fish_git_using_command pull' -n __fish_git_branch_for_remote -ka '(__fish_git_branch_for_remote)'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l commit -d "Autocommit the merge"
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l no-commit -d "Don't autocommit the merge"
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -s e -l edit -d 'Edit auto-generated merge message'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l no-edit -d "Don't edit auto-generated merge message"
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l ff -d "Don't generate a merge commit if merge is fast-forward"
8	8	-> complete -f -c git -n '__fish_git_using_command pull' -l no-ff -d "Generate a merge commit even if merge is fast-forward"
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l ff-only -d 'Refuse to merge unless fast-forward possible'
8	8	-> complete -f -c git -n '__fish_git_using_command pull' -s S -l gpg-sign -d 'GPG-sign the merge commit'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l log -d 'Populate the log message with one-line descriptions'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l no-log -d "Don't populate the log message with one-line descriptions"
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l signoff -d 'Add Signed-off-by line at the end of the merge commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l no-signoff -d 'Do not add a Signed-off-by line at the end of the merge commit message'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l stat -d "Show diffstat of the merge"
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -s n -l no-stat -d "Don't show diffstat of the merge"
8	8	-> complete -f -c git -n '__fish_git_using_command pull' -l squash -d "Squash changes from upstream branch as a single commit"
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l no-squash -d "Don't squash changes"
7	7	-> complete -x -c git -n '__fish_git_using_command pull' -s s -l strategy -d 'Use the given merge strategy'
7	7	-> complete -r -c git -n '__fish_git_using_command pull' -s X -l strategy-option -d 'Pass given option to the merge strategy'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l verify-signatures -d 'Abort merge if upstream branch tip commit is not signed with a valid key'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l no-verify-signatures -d 'Do not abort merge if upstream branch tip commit is not signed with a valid key'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l allow-unrelated-histories -d 'Allow merging even when branches do not share a common history'
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -s r -l rebase -d 'Rebase the current branch on top of the upstream branch'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l no-rebase -d 'Do not rebase the current branch on top of the upstream branch'
6	6	-> complete -f -c git -n '__fish_git_using_command pull' -l autostash -d 'Before starting rebase, stash local changes, and apply stash when done'
7	7	-> complete -f -c git -n '__fish_git_using_command pull' -l no-autostash -d 'Do not stash local changes before starting rebase'
6	6	-> complete -f -c git -n __fish_git_needs_command -a range-diff -d 'Compare two commit ranges'
7	7	-> complete -f -c git -n '__fish_git_using_command range-diff' -ka '(__fish_git_ranges)'
6	6	-> complete -f -c git -n '__fish_git_using_command range-diff' -l creation-factor -d 'Percentage by which creation is weighted'
6	6	-> complete -f -c git -n '__fish_git_using_command range-diff' -l no-dual-color -d 'Use simple diff colors'
6	6	-> complete -f -c git -n __fish_git_needs_command -a push -d 'Push changes elsewhere'
10	10	-> complete -f -c git -n '__fish_git_using_command push' -n 'not __fish_git_branch_for_remote' -a '(__fish_git_remotes)' -d 'Remote alias'
10	10	-> complete -f -c git -n '__fish_git_using_command push' -n __fish_git_branch_for_remote -ka '(__fish_git_tags)' -d Tag
8	8	-> complete -f -c git -n '__fish_git_using_command push' -n __fish_git_branch_for_remote -ka '(__fish_git_branches)'
8	8	-> complete -f -c git -n '__fish_git_using_command push' -n __fish_git_branch_for_remote -ka '(__fish_git_heads)'
21	21	-> complete -f -c git -n '__fish_git_using_command push' -n __fish_git_branch_for_remote -n 'string match -q "+*" -- (commandline -ct)' -ka '+(__fish_git_branches | string replace -r \t".*" "")' -d 'Force-push branch'
18	18	-> complete -f -c git -n '__fish_git_using_command push' -n __fish_git_branch_for_remote -n 'string match -q ":*" -- (commandline -ct)' -ka ':(__fish_git_branch_for_remote | string replace -r \t".*" "")' -d 'Delete remote branch'
21	21	-> complete -f -c git -n '__fish_git_using_command push' -n __fish_git_branch_for_remote -n 'string match -q "+*:*" -- (commandline -ct)' -ka '(commandline -ct | string replace -r ":.*" ""):(__fish_git_branch_for_remote | string replace -r \t".*" "")' -d 'Force-push local branch to remote branch'
20	20	-> complete -f -c git -n '__fish_git_using_command push' -n __fish_git_branch_for_remote -n 'string match -q "*:*" -- (commandline -ct)' -ka '(commandline -ct | string replace -r ":.*" ""):(__fish_git_branch_for_remote | string replace -r \t".*" "")' -d 'Push local branch to remote branch'
11	11	-> complete -f -c git -n '__fish_git_using_command push' -l all -d 'Push all refs under refs/heads/'
7	7	-> complete -f -c git -n '__fish_git_using_command push' -l prune -d "Remove remote branches that don't have a local counterpart"
6	6	-> complete -f -c git -n '__fish_git_using_command push' -l mirror -d 'Push all refs under refs/'
7	7	-> complete -f -c git -n '__fish_git_using_command push' -l delete -d 'Delete all listed refs from the remote repository'
6	6	-> complete -f -c git -n '__fish_git_using_command push' -l tags -d 'Push all refs under refs/tags'
6	6	-> complete -f -c git -n '__fish_git_using_command push' -l follow-tags -d 'Push all usual refs plus the ones under refs/tags'
7	7	-> complete -f -c git -n '__fish_git_using_command push' -s n -l dry-run -d 'Do everything except actually send the updates'
6	6	-> complete -f -c git -n '__fish_git_using_command push' -l porcelain -d 'Produce machine-readable output'
8	8	-> complete -f -c git -n '__fish_git_using_command push' -s f -l force -d 'Force update of remote refs'
6	6	-> complete -f -c git -n '__fish_git_using_command push' -l force-with-lease -d 'Force update of remote refs, stopping if other\'s changes would be overwritten'
6	6	-> complete -f -c git -n '__fish_git_using_command push' -l force-if-includes -d 'Force an update only if the tip of the remote-tracking ref has been integrated locally'
6	6	-> complete -f -c git -n '__fish_git_using_command push' -s u -l set-upstream -d 'Add upstream (tracking) reference'
6	6	-> complete -f -c git -n '__fish_git_using_command push' -s q -l quiet -d 'Be quiet'
8	8	-> complete -f -c git -n '__fish_git_using_command push' -s v -l verbose -d 'Be verbose'
6	6	-> complete -f -c git -n '__fish_git_using_command push' -l progress -d 'Force progress status'
6	6	-> complete -f -c git -n __fish_git_needs_command -a rebase -d 'Reapply commit sequence on a new base'
8	8	-> complete -f -c git -n '__fish_git_using_command rebase' -a '(__fish_git_remotes)' -d 'Remote alias'
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -ka '(__fish_git_branches)'
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -a '(__fish_git_heads)' -d Head
9	9	-> complete -f -c git -n '__fish_git_using_command rebase' -ka '(__fish_git_tags)' -d Tag -k
8	8	-> complete -f -c git -n '__fish_git_using_command rebase' -a '(__fish_git_recent_commits)' -k
8	8	-> complete -f -c git -n '__fish_git_using_command rebase' -n __fish_git_is_rebasing -l continue -d 'Restart the rebasing process'
9	9	-> complete -f -c git -n '__fish_git_using_command rebase' -n __fish_git_is_rebasing -l abort -d 'Abort the rebase operation'
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -n __fish_git_is_rebasing -l edit-todo -d 'Edit the todo list'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l keep-empty -d "Keep the commits that don't change anything"
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -n __fish_git_is_rebasing -l skip -d 'Restart the rebasing process by skipping the current patch'
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -s m -l merge -d 'Use merging strategies to rebase'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -s q -l quiet -d 'Be quiet'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -s v -l verbose -d 'Be verbose'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l stat -d "Show diffstat of the rebase"
8	8	-> complete -f -c git -n '__fish_git_using_command rebase' -s n -l no-stat -d "Don't show diffstat of the rebase"
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l verify -d "Allow the pre-rebase hook to run"
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -l no-verify -d "Don't allow the pre-rebase hook to run"
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -s f -l force-rebase -d 'Force the rebase'
8	8	-> complete -f -c git -n '__fish_git_using_command rebase' -l committer-date-is-author-date -d "Use the author date as the committer date"
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l ignore-date -d "Use the committer date as the author date"
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -s i -l interactive -d 'Interactive mode'
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -s p -l preserve-merges -d 'Try to recreate merges'
68	68	-> complete -f -c git -n '__fish_git_using_command rebase' -s r -l rebase-merges -a 'rebase-cousins no-rebase-cousins' -d 'Preserve branch structure'
8	8	-> complete -f -c git -n '__fish_git_using_command rebase' -l root -d 'Rebase all reachable commits'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l autosquash -d 'Automatic squashing'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l no-autosquash -d 'No automatic squashing'
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -l autostash -d 'Before starting rebase, stash local changes, and apply stash when done'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l no-autostash -d 'Do not stash local changes before starting rebase'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l no-ff -d 'No fast-forward'
9	9	-> complete -f -c git -n '__fish_git_using_command rebase' -l onto -d 'Rebase current branch onto given upstream or newbase' -ka '(__fish_git_branches)'
6	6	-> complete -f -c git -n '__fish_git_using_command rebase' -l update-refs -d 'Update any branches that point to commits being rebased'
7	7	-> complete -f -c git -n '__fish_git_using_command rebase' -l no-update-refs -d 'Don\'t update any branches that point to commits being rebased'
6	6	-> complete -r -c git -n '__fish_git_using_command rebase' -l exec -d 'Execute shellscript'
4	4	-> set -l reflogcommands show expire delete exists
6	6	-> complete -f -c git -n __fish_git_needs_command -a reflog -d 'Manage reflog information'
7	7	-> complete -f -c git -n '__fish_git_using_command reflog' -ka '(__fish_git_branches)'
8	8	-> complete -f -c git -n '__fish_git_using_command reflog' -ka '(__fish_git_heads)' -d Head
12	12	-> complete -f -c git -n "__fish_git_using_command reflog" -n "not __fish_seen_subcommand_from $reflogcommands" -a "$reflogcommands"
6	6	-> complete -c git -n __fish_git_needs_command -a reset -d 'Reset current HEAD to the specified state'
6	6	-> complete -f -c git -n '__fish_git_using_command reset' -l hard -d 'Reset the index and the working tree'
6	6	-> complete -f -c git -n '__fish_git_using_command reset' -l soft -d 'Reset head without touching the index or the working tree'
6	6	-> complete -f -c git -n '__fish_git_using_command reset' -l mixed -d 'The default: reset the index but not the working tree'
12	12	-> complete -c git -n '__fish_git_using_command reset' -n 'not contains -- -- (commandline -opc)' -ka '(__fish_git_branches)'
13	13	-> complete -f -c git -n '__fish_git_using_command reset' -n 'not contains -- -- (commandline -opc)' -a '(__fish_git_files all-staged modified)'
12	12	-> complete -f -c git -n '__fish_git_using_command reset' -n 'contains -- -- (commandline -opc)' -a '(__fish_git_files all-staged deleted modified)'
11	11	-> complete -f -c git -n '__fish_git_using_command reset' -n 'not contains -- -- (commandline -opc)' -a '(__fish_git_reflog)' -d Reflog
5	5	-> complete -f -c git -n __fish_git_needs_command -a restore -d 'Restore working tree files'
9	9	-> complete -f -c git -n '__fish_git_using_command restore' -r -s s -l source -d 'Specify the source tree used to restore the working tree' -ka '(__fish_git_refs)'
7	7	-> complete -f -c git -n '__fish_git_using_command restore' -s p -l patch -d 'Interactive mode'
9	9	-> complete -f -c git -n '__fish_git_using_command restore' -s W -l worktree -d 'Restore working tree (default)'
7	7	-> complete -f -c git -n '__fish_git_using_command restore' -s S -l staged -d 'Restore the index'
6	6	-> complete -f -c git -n '__fish_git_using_command restore' -l ours -d 'When restoring files, use stage #2 (ours)'
6	6	-> complete -f -c git -n '__fish_git_using_command restore' -l theirs -d 'When restoring files, use stage #3 (theirs)'
7	7	-> complete -f -c git -n '__fish_git_using_command restore' -s m -l merge -d 'Recreate the conflicted merge in the unmerged paths when restoring files'
7	7	-> complete -f -c git -n '__fish_git_using_command restore' -l ignore-unmerged -d 'When restoring files, do not abort the operation if there are unmerged entries'
6	6	-> complete -f -c git -n '__fish_git_using_command restore' -l ignore-skip-worktree-bits -d 'Ignore the sparse-checkout file and unconditionally restore any files in <pathspec>'
6	6	-> complete -f -c git -n '__fish_git_using_command restore' -l overlay -d 'Never remove files when restoring'
6	6	-> complete -f -c git -n '__fish_git_using_command restore' -l no-overlay -d 'Remove files when restoring (default)'
13	13	-> complete -f -c git -n '__fish_git_using_command restore' -n 'not __fish_git_contains_opt -s S staged' -a '(__fish_git_files modified deleted modified-staged-deleted unmerged)'
12	12	-> complete -f -c git -n '__fish_git_using_command restore' -n '__fish_git_contains_opt -s S staged' -a '(__fish_git_files added modified-staged deleted-staged renamed copied)'
7	7	-> complete -F -c git -n '__fish_git_using_command restore' -n '__fish_git_contains_opt -s s source'
6	6	-> complete -f -c git -n __fish_git_needs_command -a switch -d 'Switch to a branch'
9	9	-> complete -f -c git -n '__fish_git_using_command switch' -ka '(__fish_git_unique_remote_branches)' -d 'Unique Remote Branch'
8	8	-> complete -f -c git -n '__fish_git_using_command switch' -ka '(__fish_git_branches)'
7	7	-> complete -f -c git -n '__fish_git_using_command switch' -s c -l create -d 'Create a new branch'
7	7	-> complete -f -c git -n '__fish_git_using_command switch' -s C -l force-create -d 'Force create a new branch'
10	10	-> complete -f -c git -n '__fish_git_using_command switch' -s d -l detach -rka '(__fish_git_recent_commits --all)'
9	9	-> complete -f -c git -n '__fish_git_using_command switch' -s d -l detach -d 'Switch to a commit for inspection and discardable experiment' -rka '(__fish_git_refs)'
6	6	-> complete -f -c git -n '__fish_git_using_command switch' -l guess -d 'Guess branch name from remote branch (default)'
6	6	-> complete -f -c git -n '__fish_git_using_command switch' -l no-guess -d 'Do not guess branch name from remote branch'
8	8	-> complete -f -c git -n '__fish_git_using_command switch' -s f -l force -l discard-changes -d 'Proceed even if the index or the working tree differs from HEAD'
9	9	-> complete -f -c git -n '__fish_git_using_command switch' -s m -l merge -d 'Merge the current branch and contents of the working tree into a new branch'
6	6	-> complete -f -c git -n '__fish_git_using_command switch' -s t -l track -d 'Track remote branch when creating a new branch'
6	6	-> complete -f -c git -n '__fish_git_using_command switch' -l no-track -d 'Do not track remote branch when creating a new branch'
6	6	-> complete -f -c git -n '__fish_git_using_command switch' -r -l orphan -d 'Create a new orphan branch'
6	6	-> complete -f -c git -n '__fish_git_using_command switch' -l ignore-other-worktrees -d 'Force check out of the reference'
7	7	-> complete -f -c git -n '__fish_git_using_command switch' -l recurse-submodules -d 'Update the work trees of submodules'
6	6	-> complete -f -c git -n '__fish_git_using_command switch' -l no-recurse-submodules -d 'Do not update the work trees of submodules'
7	7	-> complete -f -c git -n '__fish_git_using_command restore switch' -s q -l quiet -d 'Suppress messages'
7	7	-> complete -f -c git -n '__fish_git_using_command restore switch' -l progress -d 'Report progress status to stderr (default)'
7	7	-> complete -f -c git -n '__fish_git_using_command restore switch' -l no-progress -d 'Do not report progress status to stderr'
7	7	-> complete -f -c git -n '__fish_git_using_command restore switch' -l 'conflict=merge' -d 'Same as --merge, but specify \'merge\' as the conflicting hunk style (default)'
7	7	-> complete -f -c git -n '__fish_git_using_command restore switch' -l 'conflict=diff3' -d 'Same as --merge, but specify \'diff3\' as the conflicting hunk style'
6	6	-> complete -f -c git -n __fish_git_needs_command -a rev-parse -d 'Parse revision names or give repo information'
7	7	-> complete -f -c git -n '__fish_git_using_command rev-parse' -ka '(__fish_git_branches)'
7	7	-> complete -f -c git -n '__fish_git_using_command rev-parse' -a '(__fish_git_heads)' -d Head
7	7	-> complete -c git -n '__fish_git_using_command rev-parse' -ka '(__fish_git_tags)' -d Tag
6	6	-> complete -c git -n '__fish_git_using_command rev-parse' -l abbrev-ref -d 'Output non-ambiguous short object names'
6	6	-> complete -f -c git -n __fish_git_needs_command -a revert -d 'Revert an existing commit'
7	7	-> complete -f -c git -n '__fish_git_using_command revert' -ka '(__fish_git_commits)'
6	6	-> complete -f -c git -n '__fish_git_using_command revert' -l continue -d 'Continue the operation in progress'
6	6	-> complete -f -c git -n '__fish_git_using_command revert' -l abort -d 'Cancel the operation'
6	6	-> complete -f -c git -n '__fish_git_using_command revert' -l skip -d 'Skip the current commit and continue with the rest of the sequence'
6	6	-> complete -f -c git -n '__fish_git_using_command revert' -l quit -d 'Forget about the current operation in progress'
6	6	-> complete -f -c git -n '__fish_git_using_command revert' -l no-edit -d 'Do not start the commit message editor'
6	6	-> complete -f -c git -n '__fish_git_using_command revert' -s n -l no-commit -d 'Apply changes to index but don\'t create a commit'
7	7	-> complete -f -c git -n '__fish_git_using_command revert' -s s -l signoff -d 'Add a Signed-off-by trailer at the end of the commit message'
7	7	-> complete -f -c git -n '__fish_git_using_command revert' -l rerere-autoupdate -d 'Allow the rerere mechanism to update the index with the result of auto-conflict resolution'
6	6	-> complete -f -c git -n '__fish_git_using_command revert' -l no-rerere-autoupdate -d 'Prevent the rerere mechanism from updating the index with auto-conflict resolution'
7	7	-> complete -c git -n __fish_git_needs_command -a rm -d 'Remove files from the working tree and/or staging area'
6	6	-> complete -c git -n '__fish_git_using_command rm' -l cached -d 'Unstage files from the index'
10	10	-> complete -c git -n '__fish_git_using_command rm' -n '__fish_git_contains_opt cached' -f -a '(__fish_git_files all-staged)'
5	5	-> complete -c git -n '__fish_git_using_command rm' -l ignore-unmatch -d 'Exit with a zero status even if no files matched'
6	6	-> complete -c git -n '__fish_git_using_command rm' -s r -d 'Allow recursive removal'
7	7	-> complete -c git -n '__fish_git_using_command rm' -s q -l quiet -d 'Be quiet'
7	7	-> complete -c git -n '__fish_git_using_command rm' -s f -l force -d 'Override the up-to-date check'
6	6	-> complete -c git -n '__fish_git_using_command rm' -s n -l dry-run -d 'Dry run'
6	6	-> complete -c git -n '__fish_git_using_command rm' -l sparse -d 'Allow updating index entries outside of the sparse-checkout cone'
5	5	-> complete -f -c git -n __fish_git_needs_command -a status -d 'Show the working tree status'
6	6	-> complete -f -c git -n '__fish_git_using_command status' -s s -l short -d 'Give the output in the short-format'
7	7	-> complete -f -c git -n '__fish_git_using_command status' -s b -l branch -d 'Show the branch and tracking info even in short-format'
6	6	-> complete -f -c git -n '__fish_git_using_command status' -l porcelain -d 'Give the output in a stable, easy-to-parse format'
6	6	-> complete -f -c git -n '__fish_git_using_command status' -s z -d 'Terminate entries with null character'
8	8	-> complete -f -c git -n '__fish_git_using_command status' -s u -l untracked-files -x -a 'no normal all' -d 'The untracked files handling mode'
9	9	-> complete -f -c git -n '__fish_git_using_command status' -l ignore-submodules -x -a 'none untracked dirty all' -d 'Ignore changes to submodules'
7	7	-> complete -f -c git -n '__fish_git_using_command status' -s v -l verbose -d 'Also show the textual changes that are staged to be committed'
6	6	-> complete -f -c git -n '__fish_git_using_command status' -l no-ahead-behind -d 'Do not display detailed ahead/behind upstream-branch counts'
7	7	-> complete -f -c git -n '__fish_git_using_command status' -l renames -d 'Turn on rename detection regardless of user configuration'
6	6	-> complete -f -c git -n '__fish_git_using_command status' -l no-renames -d 'Turn off rename detection regardless of user configuration'
6	6	-> complete -f -c git -n __fish_git_needs_command -a stripspace -d 'Remove unnecessary whitespace'
7	7	-> complete -f -c git -n '__fish_git_using_command stripspace' -s s -l strip-comments -d 'Strip all lines starting with comment character'
6	6	-> complete -f -c git -n '__fish_git_using_command stripspace' -s c -l comment-lines -d 'Prepend comment character to each line'
7	7	-> complete -f -c git -n __fish_git_needs_command -a tag -d 'Create, list, delete or verify a tag object signed with GPG'
22	22	-> complete -f -c git -n '__fish_git_using_command tag' -n '__fish_not_contain_opt -s d' -n '__fish_not_contain_opt -s v' -n 'test (count (commandline -opc | string match -r -v \'^-\')) -eq 3' -ka '(__fish_git_branches)'
7	7	-> complete -f -c git -n '__fish_git_using_command tag' -s a -l annotate -d 'Make an unsigned, annotated tag object'
6	6	-> complete -f -c git -n '__fish_git_using_command tag' -s s -l sign -d 'Make a GPG-signed tag'
6	6	-> complete -f -c git -n '__fish_git_using_command tag' -s d -l delete -d 'Remove a tag'
6	6	-> complete -f -c git -n '__fish_git_using_command tag' -s v -l verify -d 'Verify signature of a tag'
6	6	-> complete -f -c git -n '__fish_git_using_command tag' -s f -l force -d 'Force overwriting existing tag'
6	6	-> complete -f -c git -n '__fish_git_using_command tag' -s l -l list -d 'List tags'
8	8	-> complete -f -c git -n '__fish_git_using_command tag' -l contains -xka '(__fish_git_commits)' -d 'List tags that contain a commit'
12	12	-> complete -f -c git -n '__fish_git_using_command tag' -n '__fish_git_contains_opt -s d delete -s v verify -s f force' -ka '(__fish_git_tags)' -d Tag
6	6	-> complete -c git -n __fish_git_needs_command -a update-index -d 'Register file contents in the working tree to the index'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l add -d 'Add specified files to the index'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l remove -d 'Remove specified files from the index'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l refresh -d 'Refresh current index'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -s q -d 'Continue refresh after error'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l ignore-submodules -d 'Do not try to update submodules'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l unmerged -d 'Continue on unmerged changes in the index'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l ignore-missing -d 'Ignores missing files during a refresh'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l index-info -d 'Read index information from stdin'
9	9	-> complete -x -c git -n '__fish_git_using_command update-index' -l chmod -a '+x\tAdd\ execute\ permissions -x\tRemove\ execute\ permissions' -d 'Set execute permissions'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l assume-unchanged -d 'Set the "assume unchanged" bit for the paths'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l no-assume-unchanged -d 'Unset the "assume unchanged" bit'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l really-refresh -d 'Refresh but check stat info unconditionally'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l skip-worktree -d 'Set the "fsmonitor valid" bit'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l no-skip-worktree -d 'Unset the "fsmonitor valid" bit'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l fsmonitor-valid -d 'Set the "fsmonitor valid" bit'
8	8	-> complete -f -c git -n '__fish_git_using_command update-index' -l no-fsmonitor-valid -d 'Unset the "fsmonitor valid" bit'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -s g -l again -d 'Run git update-index on paths with differing index'
7	7	-> complete -f -c git -n '__fish_git_using_command update-index' -l unresolve -d 'Restores the state of a file during a merge'
7	7	-> complete -r -c git -n '__fish_git_using_command update-index' -l info-only -d 'Do not create objects in the object database'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l force-remove -d 'Forcefully remove the file from the index'
8	8	-> complete -f -c git -n '__fish_git_using_command update-index' -l replace -d 'Replace conflicting entries'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l stdin -d 'Read list of paths from stdin'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l verbose -d 'Report changes to index'
8	8	-> complete -x -c git -n '__fish_git_using_command update-index' -l index-version -a "2\t\t3\t\t4" -d 'Set index-version'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -s z -d 'Seperate paths with NUL instead of LF'
7	7	-> complete -f -c git -n '__fish_git_using_command update-index' -l split-index -d 'Enable split index mode'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l no-split-index -d 'Disable split index mode'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l untracked-cache -d 'Enable untracked cache feature'
7	7	-> complete -f -c git -n '__fish_git_using_command update-index' -l no-untracked-cache -d 'Disable untracked cache feature'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l test-untracked-cache -d 'Only perform tests on the working directory'
7	7	-> complete -f -c git -n '__fish_git_using_command update-index' -l force-untracked-cache -d 'Same as --untracked-cache'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l fsmonitor -d 'Enable files system monitor feature'
6	6	-> complete -f -c git -n '__fish_git_using_command update-index' -l no-fsmonitor -d 'Disable files system monitor feature'
5	5	-> set -l git_worktree_commands add list lock move prune remove unlock
8	8	-> complete -c git -n __fish_git_needs_command -a worktree -d 'Manage multiple working trees'
13	13	-> complete -f -c git -n "__fish_git_using_command worktree" -n "not __fish_seen_subcommand_from $git_worktree_commands" -a add -d 'Create a working tree'
12	12	-> complete -f -c git -n "__fish_git_using_command worktree" -n "not __fish_seen_subcommand_from $git_worktree_commands" -a list -d 'List details of each worktree'
12	12	-> complete -f -c git -n "__fish_git_using_command worktree" -n "not __fish_seen_subcommand_from $git_worktree_commands" -a lock -d 'Lock a working tree'
12	12	-> complete -f -c git -n "__fish_git_using_command worktree" -n "not __fish_seen_subcommand_from $git_worktree_commands" -a move -d 'Move a working tree to a new location'
12	12	-> complete -f -c git -n "__fish_git_using_command worktree" -n "not __fish_seen_subcommand_from $git_worktree_commands" -a prune -d 'Prune working tree information in $GIT_DIR/worktrees'
12	12	-> complete -f -c git -n "__fish_git_using_command worktree" -n "not __fish_seen_subcommand_from $git_worktree_commands" -a remove -d 'Remove a working tree'
12	12	-> complete -f -c git -n "__fish_git_using_command worktree" -n "not __fish_seen_subcommand_from $git_worktree_commands" -a unlock -d 'Unlock a working tree'
10	10	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add move remove' -s f -l force -d 'Override safeguards'
9	9	-> complete -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add'
10	10	-> complete -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -ka '(__fish_git_branches)'
10	10	-> complete -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -ka '(__fish_git_heads)' -d Head
11	11	-> complete -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -ka '(__fish_git_tags)' -d Tag
10	10	-> complete -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -ka '(__fish_git_unique_remote_branches)' -d 'Unique Remote Branch'
9	9	-> complete -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -ka '(__fish_git_local_branches)'
9	9	-> complete -x -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -s b -d 'Create a new branch'
8	8	-> complete -x -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -s B -d 'Create a new branch even if it already exists'
9	9	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -l detach -d 'Detach HEAD in the new working tree'
8	8	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -l checkout -d 'Checkout <commit-ish> after creating working tree'
10	10	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -l no-checkout -d 'Suppress checkout'
8	8	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -l guess-remote
8	8	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -l no-guess-remote
8	8	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -l track -d 'Mark <commit-ish> as "upstream" from the new branch'
10	10	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -l no-track -d 'Don\'t mark <commit-ish> as "upstream" from the new branch'
8	8	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -l lock -d 'Lock working tree after creation'
9	9	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from add' -s q -l quiet -d 'Suppress feedback messages'
10	10	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from list' -l porcelain -d 'Output in an easy-to-parse format for scripts'
10	10	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from lock' -a '(__fish_git_complete_worktrees)' -d Worktree
9	9	-> complete -x -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from lock' -l reason -d 'An explanation why the working tree is locked'
16	16	-> complete -x -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from move' -n 'not __fish_any_arg_in (__fish_git_complete_worktrees)' -a '(__fish_git_complete_worktrees)' -d Worktree
13	13	-> complete -x -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from move' -n '__fish_any_arg_in (__fish_git_complete_worktrees)' -a '(__fish_complete_directories)'
9	9	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from prune' -s n -l dry-run -d 'Do not remove anything'
10	10	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from prune' -s v -l verbose -d 'Report all removals'
9	9	-> complete -x -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from prune' -l expire -d 'Only expire unused working trees older than <time>'
10	10	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from remove' -a '(__fish_git_complete_worktrees)' -d Worktree
10	10	-> complete -f -c git -n '__fish_git_using_command worktree' -n '__fish_seen_subcommand_from unlock' -a '(__fish_git_complete_worktrees)' -d Worktree
6	6	-> complete -c git -n __fish_git_needs_command -a stash -d 'Stash away changes'
9	9	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a list -d 'List stashes'
9	9	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a show -d 'Show the changes recorded in the stash'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a pop -d 'Apply and remove a single stashed state'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a apply -d 'Apply a single stashed state'
9	9	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a clear -d 'Remove all stashed states'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a drop -d 'Remove a single stashed state from the stash list'
10	10	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a create -d 'Create a stash'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a save -d 'Save a new stash'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a branch -d 'Create a new branch from a stash'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n __fish_git_stash_not_using_subcommand -a push -d 'Create a new stash with given files'
10	10	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command apply' -ka '(__fish_git_complete_stashes)'
9	9	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command branch' -ka '(__fish_git_complete_stashes)'
11	11	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command drop' -ka '(__fish_git_complete_stashes)'
10	10	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command pop' -ka '(__fish_git_complete_stashes)'
10	10	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command show' -ka '(__fish_git_complete_stashes)'
12	12	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command push' -a '(__fish_git_files modified deleted modified-staged-deleted)'
9	9	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command push' -s a -l all -d 'Stash ignored and untracked files'
9	9	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command push' -s k -l keep-index -d 'Keep changes in index intact'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command push' -s p -l patch -d 'Interactively select hunks'
9	9	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command push' -s m -l message -d 'Add a description'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command push' -l no-keep-index -d 'Don\'t keep changes in index intact'
8	8	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command push' -s S -l staged -d 'Stash only staged changes'
9	9	-> complete -f -c git -n '__fish_git_using_command stash' -n '__fish_git_stash_using_command push' -s u -l include-untracked -d 'Stash untracked files'
6	6	-> complete -f -c git -n __fish_git_needs_command -a config -d 'Set and read git configuration variables'
6	6	-> complete -f -c git -n __fish_git_needs_command -a format-patch -d 'Generate patch series to send upstream'
8	8	-> complete -f -c git -n '__fish_git_using_command format-patch' -ka '(__fish_git_branches)'
8	8	-> complete -c git -n '__fish_git_using_command format-patch' -s o -l output-directory -xa '(__fish_complete_directories)'
8	8	-> complete -f -c git -n '__fish_git_using_command format-patch' -s p -l no-stat -d "Generate plain patches without diffstat"
8	8	-> complete -f -c git -n '__fish_git_using_command format-patch' -s s -l no-patch -d "Suppress diff output"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch' -l minimal -d "Spend more time to create smaller diffs"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch' -l patience -d "Generate diff with the 'patience' algorithm"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch' -l histogram -d "Generate diff with the 'histogram' algorithm"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch' -l stdout -d "Print all commits to stdout in mbox format"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch' -l numstat -d "Show number of added/deleted lines in decimal notation"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch' -l shortstat -d "Output only last line of the stat"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch' -l summary -d "Output a condensed summary of extended header information"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch' -l no-renames -d "Disable rename detection"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch' -l full-index -d "Show full blob object names"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch' -l binary -d "Output a binary diff for use with git apply"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l find-copies-harder -d "Also inspect unmodified files as source for a copy"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch' -l text -s a -d "Treat all files as text"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l ignore-space-at-eol -d "Ignore changes in whitespace at EOL"
8	8	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l ignore-space-change -s b -d "Ignore changes in amount of whitespace"
9	9	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l ignore-all-space -s w -d "Ignore whitespace when comparing lines"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l ignore-blank-lines -d "Ignore changes whose lines are all blank"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l function-context -s W -d "Show whole surrounding functions of changes"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l ext-diff -d "Allow an external diff helper to be executed"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l no-ext-diff -d "Disallow external diff helpers"
7	7	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l no-textconv -d "Disallow external text conversion filters for binary files (Default)"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l textconv -d "Allow external filters for binary files (Resulting diff is unappliable)"
8	8	-> complete -f -c git -n '__fish_git_using_command format-patch log' -l no-prefix -d "Do not show source or destination prefix"
6	6	-> complete -f -c git -n '__fish_git_using_command format-patch' -l numbered -s n -d "Name output in [Patch n/m] format, even with a single patch"
9	9	-> complete -f -c git -n '__fish_git_using_command format-patch' -l no-numbered -s N -d "Name output in [Patch] format, even with multiple patches"
6	6	-> set -l submodulecommands add status init deinit update set-branch set-url summary foreach sync absorbgitdirs
8	8	-> complete -f -c git -n __fish_git_needs_command -a submodule -d 'Initialize, update or inspect submodules'
15	15	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a add -d 'Add a submodule'
18	18	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a status -d 'Show submodule status'
14	14	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a init -d 'Initialize all submodules'
13	13	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a deinit -d 'Unregister the given submodules'
15	15	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a update -d 'Update all submodules'
13	13	-> complete -x -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a set-branch -d 'Set the default remote tracking branch'
13	13	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a set-url -d 'Sets the URL of the specified submodule'
13	13	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a summary -d 'Show commit summary'
13	13	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a foreach -d 'Run command on each submodule'
14	14	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a sync -d 'Sync submodules\' URL with .gitmodules'
13	13	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -a absorbgitdirs -d 'Move submodule\'s git directory to current .git/module directory'
15	15	-> complete -f -c git -n "__fish_git_using_command submodule" -n "not __fish_seen_subcommand_from $submodulecommands" -s q -l quiet -d "Only print error messages"
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from update' -l init -d "Initialize all submodules"
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from update' -l checkout -d "Checkout the superproject's commit on a detached HEAD in the submodule"
10	10	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from update' -l merge -d "Merge the superproject's commit into the current branch of the submodule"
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from update' -l rebase -d "Rebase current branch onto the superproject's commit"
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from update' -s N -l no-fetch -d "Don't fetch new objects from the remote"
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from update' -l remote -d "Instead of using superproject's SHA-1, use the state of the submodule's remote-tracking branch"
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from update' -l force -d "Discard local changes when switching to a different commit & always run checkout"
8	8	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from add' -l force -d "Also add ignored submodule path"
8	8	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from deinit' -l force -d "Remove even with local changes"
8	8	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from deinit' -l all -d "Remove all submodules"
14	14	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from deinit' -n 'not contains -- -- (commandline -opc)' -a '(__fish_git_submodules)' -d Submodule
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from set-branch' -s b -l branch -d "Specify the branch to use"
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from set-branch' -s d -l default -d "Use default branch of the submodule"
9	9	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from status summary' -l cached -d "Use the commit stored in the index"
14	14	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from status' -n 'not contains -- -- (commandline -opc)' -a '(__fish_git_submodules)' -d Submodule
10	10	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from summary' -l files -d "Compare the commit in the index with submodule HEAD"
10	10	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from foreach update status' -l recursive -d "Traverse submodules recursively"
11	11	-> complete -f -c git -n '__fish_git_using_command submodule' -n '__fish_seen_subcommand_from foreach' -a "(__fish_complete_subcommand --fcs-skip=3)"
6	6	-> complete -f -c git -n __fish_git_needs_command -a whatchanged -d 'Show logs with difference each commit introduces'
7	7	-> complete -c git -n __fish_git_needs_command -a '(__fish_git_aliases)'
5	5	-> complete -f -c git -n __fish_git_needs_command -a clean -d 'Remove untracked files from the working tree'
7	7	-> complete -f -c git -n '__fish_git_using_command clean' -s f -l force -d 'Force run'
7	7	-> complete -f -c git -n '__fish_git_using_command clean' -s i -l interactive -d 'Show what would be done and clean files interactively'
8	8	-> complete -f -c git -n '__fish_git_using_command clean' -s n -l dry-run -d 'Don\'t actually remove anything, just show what would be done'
6	6	-> complete -f -c git -n '__fish_git_using_command clean' -s q -l quiet -d 'Be quiet, only report errors'
6	6	-> complete -f -c git -n '__fish_git_using_command clean' -s d -d 'Remove untracked directories in addition to untracked files'
6	6	-> complete -f -c git -n '__fish_git_using_command clean' -s x -d 'Remove ignored files, as well'
6	6	-> complete -f -c git -n '__fish_git_using_command clean' -s X -d 'Remove only ignored files'
5	5	-> complete -f -c git -n __fish_git_needs_command -a blame -d 'Show what last modified each line of a file'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -s b -d 'Show blank SHA-1 for boundary commits'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -l root -d 'Do not treat root commits as boundaries'
8	8	-> complete -f -c git -n '__fish_git_using_command blame' -l show-stats -d 'Include additional statistics'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -s L -d 'Annotate only the given line range'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -s l -d 'Show long rev'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -s t -d 'Show raw timestamp'
6	6	-> complete -r -c git -n '__fish_git_using_command blame' -s S -d 'Use revisions from named file instead of calling rev-list'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -l reverse -d 'Walk history forward instead of backward'
7	7	-> complete -f -c git -n '__fish_git_using_command blame' -s p -l porcelain -d 'Show in a format designed for machine consumption'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -l line-porcelain -d 'Show the porcelain format'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -l incremental -d 'Show the result incrementally'
6	6	-> complete -r -c git -n '__fish_git_using_command blame' -l contents -d 'Instead of working tree, use the contents of the named file'
6	6	-> complete -x -c git -n '__fish_git_using_command blame' -l date -d 'Specifies the format used to output dates'
7	7	-> complete -f -c git -n '__fish_git_using_command blame' -s M -d 'Detect moved or copied lines within a file'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -s C -d 'Detect lines moved or copied from other files modified in the same commit'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -s h -d 'Show help message'
7	7	-> complete -f -c git -n '__fish_git_using_command blame' -s c -d 'Use the same output mode as git-annotate'
7	7	-> complete -f -c git -n '__fish_git_using_command blame' -s f -l show-name -d 'Show the filename in the original commit'
7	7	-> complete -f -c git -n '__fish_git_using_command blame' -s n -l show-number -d 'Show the line number in the original commit'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -s s -d 'Suppress the author name and timestamp from the output'
7	7	-> complete -f -c git -n '__fish_git_using_command blame' -s e -l show-email -d 'Show the author email instead of author name'
6	6	-> complete -f -c git -n '__fish_git_using_command blame' -s w -d 'Ignore whitespace changes'
6	6	-> complete -f -c git -n __fish_git_needs_command -a help -d 'Display help information about Git'
8	8	-> complete -f -c git -n '__fish_git_using_command help' -a '(__fish_git_help_all_concepts)'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a add -d 'Add file contents to the index'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a am -d 'Apply a series of patches from a mailbox'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a apply -d 'Apply a patch on a git index file and a working tree'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a archive -d 'Create an archive of files from a named tree'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a bisect -d 'Find the change that introduced a bug by binary search'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a blame -d 'Show what revision and author last modified each line of a file'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a branch -d 'List, create, or delete branches'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a checkout -d 'Checkout and switch to a branch'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a cherry-pick -d 'Apply the change introduced by an existing commit'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a clean -d 'Remove untracked files from the working tree'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a clone -d 'Clone a repository into a new directory'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a commit -d 'Record changes to the repository'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a config -d 'Set and read git configuration variables'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a count-objects -d 'Count unpacked number of objects and their disk consumption'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a describe -d 'Give an object a human-readable name'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a diff -d 'Show changes between commits, commit and working tree, etc'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a daemon -d 'A really simple server for Git repositories'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a difftool -d 'Open diffs in a visual tool'
8	8	-> complete -f -c git -n '__fish_git_using_command help' -a fetch -d 'Download objects and refs from another repository'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a filter-branch -d 'Rewrite branches'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a format-patch -d 'Generate patch series to send upstream'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a gc -d 'Cleanup unnecessary files and optimize the local repository'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a grep -d 'Print lines matching a pattern'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a init -d 'Create an empty git repository or reinitialize an existing one'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a log -d 'Show commit logs'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a ls-files -d 'Show information about files in the index and the working tree'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a mailinfo -d 'Extracts patch and authorship from a single e-mail message'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a mailsplit -d 'Simple UNIX mbox splitter program'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a maintenance -d 'Run tasks to optimize Git repository data'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a merge -d 'Join two or more development histories together'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a merge-base -d 'Find as good common ancestors as possible for a merge'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a mergetool -d 'Run merge conflict resolution tools to resolve merge conflicts'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a mv -d 'Move or rename a file, a directory, or a symlink'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a notes -d 'Add or inspect object notes'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a prune -d 'Prune all unreachable objects from the object database'
9	9	-> complete -f -c git -n '__fish_git_using_command help' -a pull -d 'Fetch from and merge with another repository or a local branch'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a push -d 'Update remote refs along with associated objects'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a range-diff -d 'Compare two commit ranges (e.g. two versions of a branch)'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a rebase -d 'Forward-port local commits to the updated upstream head'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a reflog -d 'Manage reflog information'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a remote -d 'Manage set of tracked repositories'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a reset -d 'Reset current HEAD to the specified state'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a restore -d 'Restore working tree files'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a revert -d 'Revert an existing commit'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a rev-parse -d 'Pick out and massage parameters'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a rm -d 'Remove files from the working tree and from the index'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a show -d 'Shows the last commit of a branch'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a show-branch -d 'Shows the commits on branches'
6	6	-> complete -f -c git -n '__fish_git_using_command help' -a stash -d 'Stash away changes'
8	8	-> complete -f -c git -n '__fish_git_using_command help' -a status -d 'Show the working tree status'
12	12	-> complete -f -c git -n '__fish_git_using_command help' -a submodule -d 'Initialize, update or inspect submodules'
8	8	-> complete -f -c git -n '__fish_git_using_command help' -a stripspace -d 'Remove unnecessary whitespace'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a switch -d 'Switch to a branch'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a tag -d 'Create, list, delete or verify a tag object signed with GPG'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a update-index -d 'Register file contents in the working tree to the index'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a whatchanged -d 'Show logs with difference each commit introduces'
7	7	-> complete -f -c git -n '__fish_git_using_command help' -a worktree -d 'Manage multiple working trees'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l global -d 'Get/set global configuration'
7	7	-> complete -f -c git -n '__fish_git_using_command config' -l system -d 'Get/set system configuration'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l local -d 'Get/set local repo configuration'
7	7	-> complete -F -c git -n '__fish_git_using_command config' -s f -l file -d 'Read config from file' -r
7	7	-> complete -F -c git -n '__fish_git_using_command config' -l blob -d 'Read config from blob' -r
2	2	-> function __fish_git_complete_key_values...
12	12	-> complete -c git -n '__fish_git_using_command config' -n '__fish_is_nth_token 2' -kfa '(__fish_git_config_keys)'
10	10	-> complete -c git -n '__fish_git_using_command config' -n '__fish_is_nth_token 3' -fa '(__fish_git_complete_key_values)'
8	8	-> complete -f -c git -n '__fish_git_using_command config' -l get -d 'Get config with name' -kra '(__fish_git_config_keys)'
10	10	-> complete -f -c git -n '__fish_git_using_command config' -l get-all -d 'Get all values matching key' -ka '(__fish_git_config_keys)'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l get-urlmatch -d 'Get value specific for the section url' -r
8	8	-> complete -f -c git -n '__fish_git_using_command config' -l replace-all -d 'Replace all matching variables' -kra '(__fish_git_config_keys)'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l add -d 'Add a new variable' -r
8	8	-> complete -f -c git -n '__fish_git_using_command config' -l unset -d 'Remove a variable' -ka '(__fish_git_config_keys)'
9	9	-> complete -f -c git -n '__fish_git_using_command config' -l unset-all -d 'Remove matching variables' -ka '(__fish_git_config_keys)'
7	7	-> complete -f -c git -n '__fish_git_using_command config' -l rename-section -d 'Rename section' -r
7	7	-> complete -f -c git -n '__fish_git_using_command config' -s l -l list -d 'List all variables'
8	8	-> complete -f -c git -n '__fish_git_using_command config' -s e -l edit -d 'Open configuration in an editor'
7	7	-> complete -f -c git -n '__fish_git_using_command config' -s t -l type -d 'Value is of given type'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l bool -d 'Value is \'true\' or \'false\''
7	7	-> complete -f -c git -n '__fish_git_using_command config' -l int -d 'Value is a decimal number'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l bool-or-int -d 'Value is --bool or --int'
7	7	-> complete -f -c git -n '__fish_git_using_command config' -l path -d 'Value is a path'
7	7	-> complete -f -c git -n '__fish_git_using_command config' -l expiry-date -d 'Value is an expiry date'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -s z -l null -d 'Terminate values with NUL byte'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l name-only -d 'Show variable names only'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l includes -d 'Respect include directives'
6	6	-> complete -f -c git -n '__fish_git_using_command config' -l show-origin -d 'Show origin of configuration'
9	9	-> complete -f -c git -n '__fish_git_using_command config' -n '__fish_seen_argument get' -l default -d 'Use default value when missing entry'
6	6	-> complete -f -c git -n __fish_git_needs_command -a for-each-ref -d "Format and output info on each ref"
6	6	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -l count -d "Limit to n results"
4	4	-> set -l for_each_ref_interpreters shell perl python tcl
122	174	-> for intr in $for_each_ref_interpreters...
15	15	--> complete -f -c git -n '__fish_git_using_command for-each-ref' \
        -n "not __fish_seen_argument --$for_each_ref_interpreters" \
        -l $intr -d "%(fieldname) placeholders are $intr scripts"
12	12	--> complete -f -c git -n '__fish_git_using_command for-each-ref' \
        -n "not __fish_seen_argument --$for_each_ref_interpreters" \
        -l $intr -d "%(fieldname) placeholders are $intr scripts"
11	11	--> complete -f -c git -n '__fish_git_using_command for-each-ref' \
        -n "not __fish_seen_argument --$for_each_ref_interpreters" \
        -l $intr -d "%(fieldname) placeholders are $intr scripts"
14	14	--> complete -f -c git -n '__fish_git_using_command for-each-ref' \
        -n "not __fish_seen_argument --$for_each_ref_interpreters" \
        -l $intr -d "%(fieldname) placeholders are $intr scripts"
7	7	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -x -l format -d "Format string with %(fieldname) placeholders"
10	10	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -f -l color -d "When to color" -a "always never auto"
8	8	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -l points-at -d "Only list refs pointing at object" -ka '(__fish_git_commits)'
9	9	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -x -l merged -d "Only list refs reachable from specified commit" -ka '(__fish_git_commits)'
9	9	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -x -l no-merged -d "Only list refs not reachable from specified commit" -ka '(__fish_git_commits)'
10	10	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -x -l contains -d "Only list refs containing the specified commit" -ka '(__fish_git_commits)'
9	9	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -x -l no-contains -d "Only list refs not containing the specified commit" -ka '(__fish_git_commits)'
6	6	-> complete -f -c git -n '__fish_git_using_command for-each-ref' -x -l ignore-case -d "Sorting and filtering refs are case insensitive"
4	4	-> set -l sortcommands branch for-each-ref tag
1	1	-> function __fish_git_sort_keys...
11	11	-> complete -f -c git -n "__fish_seen_subcommand_from $sortcommands" -l sort -d 'Sort results by' -a "(__fish_git_sort_keys)"
8	8	-> complete -c git -n __fish_git_needs_command -a '(__fish_git_custom_commands)' -d 'Custom command'
1	1	-> function __fish_git_complete_custom_command -a subcommand...
3	3	-> set -l __fish_git_custom_commands_completion
114	6328	-> for file in (path filter -xZ $PATH/git-* | path basename)...
5168	5168	--> path filter -xZ $PATH/git-* | path basename
8	8	--> contains -- $file $__fish_git_custom_commands_completion
35	69	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
34	34	---> string replace -r '^git-' '' -- $file | string escape
17	17	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
5	5	--> set -a __fish_git_custom_commands_completion $file
5	5	--> contains -- $file $__fish_git_custom_commands_completion
30	59	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
29	29	---> string replace -r '^git-' '' -- $file | string escape
15	15	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
5	5	--> set -a __fish_git_custom_commands_completion $file
5	5	--> contains -- $file $__fish_git_custom_commands_completion
28	58	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
30	30	---> string replace -r '^git-' '' -- $file | string escape
15	15	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
5	5	--> set -a __fish_git_custom_commands_completion $file
5	5	--> contains -- $file $__fish_git_custom_commands_completion
28	55	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
27	27	---> string replace -r '^git-' '' -- $file | string escape
14	14	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
4	4	--> set -a __fish_git_custom_commands_completion $file
5	5	--> contains -- $file $__fish_git_custom_commands_completion
28	54	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
26	26	---> string replace -r '^git-' '' -- $file | string escape
14	14	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
5	5	--> set -a __fish_git_custom_commands_completion $file
5	5	--> contains -- $file $__fish_git_custom_commands_completion
26	53	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
27	27	---> string replace -r '^git-' '' -- $file | string escape
14	14	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
5	5	--> set -a __fish_git_custom_commands_completion $file
5	5	--> contains -- $file $__fish_git_custom_commands_completion
26	52	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
26	26	---> string replace -r '^git-' '' -- $file | string escape
13	13	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
4	4	--> set -a __fish_git_custom_commands_completion $file
6	6	--> contains -- $file $__fish_git_custom_commands_completion
27	52	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
25	25	---> string replace -r '^git-' '' -- $file | string escape
13	13	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
4	4	--> set -a __fish_git_custom_commands_completion $file
6	6	--> contains -- $file $__fish_git_custom_commands_completion
26	51	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
25	25	---> string replace -r '^git-' '' -- $file | string escape
17	17	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
5	5	--> set -a __fish_git_custom_commands_completion $file
7	7	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
7	7	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
0	0	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
30	62	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
32	32	---> string replace -r '^git-' '' -- $file | string escape
14	14	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
5	5	--> set -a __fish_git_custom_commands_completion $file
8	8	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
27	54	--> set -l cmd (string replace -r '^git-' '' -- $file | string escape)
27	27	---> string replace -r '^git-' '' -- $file | string escape
13	13	--> complete -c git -f -n "__fish_git_using_command $cmd" -a "(__fish_git_complete_custom_command $cmd)"
4	4	--> set -a __fish_git_custom_commands_completion $file
8	8	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
7	7	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
7	7	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
7	7	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
6	6	--> contains -- $file $__fish_git_custom_commands_completion
1	1	--> continue
26	61	> __direnv_export_eval_2 'git reset --hard origin/master'
5	20	-> if set -q __direnv_export_again...
15	15	--> set -q __direnv_export_again
15	15	-> functions --erase __direnv_cd_hook
10	27	> __fish_disable_bracketed_paste 'git reset --hard origin/master'
17	17	-> printf "\e[?2004l"
6	14	> __fish_disable_focus 'git reset --hard origin/master'
8	8	-> echo -n \e\[\?1004l
10	526	> fish_title git\ reset\ --hard\ origin/master
11	516	-> if not set -q INSIDE_EMACS...
8	8	--> not set -q INSIDE_EMACS
5	5	--> set -l ssh
3	3	--> set -q SSH_TTY
3	489	--> if set -q argv[1]...
4	4	---> set -q argv[1]
82	482	---> echo -- $ssh (string sub -l 20 -- $argv[1]) (prompt_pwd -d 1 -D 1)
11	11	----> string sub -l 20 -- $argv[1]
21	389	----> prompt_pwd -d 1 -D 1
9	9	-----> set -l options h/help d/dir-length= D/full-length-dirs=
20	20	-----> argparse -n prompt_pwd $options -- $argv
1	4	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
4	4	-----> set -q argv[1]
7	7	-----> set argv $PWD
3	3	-----> set -ql _flag_d
6	6	-----> set -l fish_prompt_pwd_dir_length $_flag_d
3	3	-----> set -q fish_prompt_pwd_dir_length
3	3	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
4	4	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
3	3	-----> set -q fish_prompt_pwd_full_dirs
11	300	-----> for path in $argv...
38	49	------> set -l realhome (string escape --style=regex -- ~)
11	11	-------> string escape --style=regex -- ~
35	58	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
23	23	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
5	182	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
9	9	-------> test "$fish_prompt_pwd_dir_length" -eq 0
5	5	-------> set -l full
5	106	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	--------> test $fish_prompt_pwd_full_dirs -gt 0
36	86	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
36	50	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
14	14	----------> math $fish_prompt_pwd_full_dirs - 1
5	5	--------> set tmp $all[1]
5	5	--------> set full $all[2..]
38	57	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
19	19	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
2999	2999	> git reset --hard origin/master
32	64	> __fish_enable_focus 'git reset --hard origin/master'
32	32	-> echo -n \e\[\?1004h
17	2015	> __direnv_export_eval
1973	1973	-> "/nix/store/bri4plkp5ykp2dvjs1bwzd92z7grp6h7-direnv-2.34.0/bin/direnv" export fish | source
4	25	-> if test "$direnv_fish_mode" != "disable_arrow"...
14	14	--> test "$direnv_fish_mode" != "disable_arrow"
7	7	--> function __direnv_cd_hook --on-variable PWD...
9	18	> __fish_enable_bracketed_paste
9	9	-> printf "\e[?2004h"
8	39	> fish_mode_prompt
13	31	-> fish_default_mode_prompt
2	18	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
11	11	---> test "$fish_key_bindings" = fish_vi_key_bindings
5	5	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
19	6799	> fish_prompt
8	8	-> set -l last_pipestatus $pipestatus
5	5	-> set -lx __fish_last_status $status
28	37	-> set -l normal (set_color normal)
9	9	--> set_color normal
3	3	-> set -q fish_color_status
8	8	-> set -l color_cwd $fish_color_cwd
5	5	-> set -l suffix '>'
2	42	-> if functions -q fish_is_root_user...
6	6	--> functions -q fish_is_root_user
8	34	--> fish_is_root_user
1	15	---> if test "$EUID" = 0 2>/dev/null...
14	14	----> test "$EUID" = 0 2>/dev/null
2	9	---> if contains -- $USER root toor Administrator...
7	7	----> contains -- $USER root toor Administrator
2	2	---> return 1
4	4	-> set -l bold_flag --bold
4	4	-> set -q __fish_prompt_status_generation
1	6	-> if test $__fish_prompt_status_generation = $status_generation...
5	5	--> test $__fish_prompt_status_generation = $status_generation
6	6	-> set __fish_prompt_status_generation $status_generation
31	41	-> set -l status_color (set_color $fish_color_status)
10	10	--> set_color $fish_color_status
30	41	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
11	11	--> set_color $bold_flag $fish_color_status
31	112	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
29	81	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
6	6	---> set -l last_status
3	10	---> if set -q __fish_last_status...
3	3	----> set -q __fish_last_status
4	4	----> set last_status $__fish_last_status
5	5	---> set -l left_brace $argv[1]
4	4	---> set -l right_brace $argv[2]
4	4	---> set -l separator $argv[3]
5	5	---> set -l brace_sep_color $argv[4]
4	4	---> set -l status_color $argv[5]
5	5	---> set -e argv[1 2 3 4 5]
2	4	---> if not set -q argv[1]...
2	2	----> not set -q argv[1]
1	5	---> if not contains $last_status 0 141...
4	4	----> not contains $last_status 0 141
112	6458	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
14	216	--> prompt_login
2	6	---> if not set -q __fish_machine...
4	4	----> not set -q __fish_machine
0	3	---> if set -q __fish_machine[1]...
3	3	----> set -q __fish_machine[1]
5	5	---> set -l color_host $fish_color_host
1	4	---> if set -q SSH_TTY...
3	3	----> set -q SSH_TTY
124	184	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
8	8	----> set_color $fish_color_user
6	6	----> set_color normal
6	6	----> set_color $color_host
11	33	----> prompt_hostname
22	22	-----> string replace -r -- "\..*" "" $hostname
7	7	----> set_color normal
8	8	--> set_color $color_cwd
20	311	--> prompt_pwd
6	6	---> set -l options h/help d/dir-length= D/full-length-dirs=
9	9	---> argparse -n prompt_pwd $options -- $argv
0	4	---> if set -q _flag_help...
4	4	----> set -q _flag_help
2	2	---> set -q argv[1]
4	4	---> set argv $PWD
2	2	---> set -ql _flag_d
3	3	---> set -q fish_prompt_pwd_dir_length
4	4	---> set -l fish_prompt_pwd_dir_length 1
3	3	---> set -l fulldirs 0
2	2	---> set -ql _flag_D
2	2	---> set -q fish_prompt_pwd_full_dirs
3	3	---> set -l fish_prompt_pwd_full_dirs 1
9	247	---> for path in $argv...
28	38	----> set -l realhome (string escape --style=regex -- ~)
10	10	-----> string escape --style=regex -- ~
30	47	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
17	17	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
4	153	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
6	6	-----> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-----> set -l full
5	90	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	------> test $fish_prompt_pwd_full_dirs -gt 0
33	71	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
28	38	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
10	10	--------> math $fish_prompt_pwd_full_dirs - 1
5	5	------> set tmp $all[1]
4	4	------> set full $all[2..]
32	49	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
17	17	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
10	5811	--> fish_vcs_prompt
55	5801	---> fish_git_prompt $argv
3	132	----> if not command -sq git...
129	129	-----> not command -sq git
1	7	----> if functions -q __fish_git_prompt_ready...
6	6	-----> functions -q __fish_git_prompt_ready
77	1577	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1500	1500	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
5	5	----> set -l git_dir $repo_info[1]
4	4	----> set -l inside_gitdir $repo_info[2]
4	4	----> set -l bare_repo $repo_info[3]
4	4	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
3	3	----> set -l sha $repo_info[5]
35	1456	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
23	1421	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
4	4	------> set -l inside_gitdir $argv[2]
4	4	------> set -l bare_repo $argv[3]
2	2	------> set -q argv[5]
4	4	------> set -l sha $argv[5]
3	3	------> set -l branch
2	2	------> set -l operation
2	2	------> set -l detached no
2	2	------> set -l bare
2	2	------> set -l step
2	2	------> set -l total
17	46	------> if test -d $git_dir/rebase-merge...
7	7	-------> test -d $git_dir/rebase-merge
2	22	-------> if test -d $git_dir/rebase-apply...
4	4	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
4	4	--------> test -f $git_dir/CHERRY_PICK_HEAD
4	4	--------> test -f $git_dir/REVERT_HEAD
4	4	--------> test -f $git_dir/BISECT_LOG
0	6	------> if test -n "$step" -a -n "$total"...
6	6	-------> test -n "$step" -a -n "$total"
5	1295	------> if test -z "$branch"...
2	2	-------> test -z "$branch"
0	1288	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
43	1288	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1245	1245	---------> command git symbolic-ref HEAD 2>/dev/null
0	6	------> if test true = $inside_gitdir...
6	6	-------> test true = $inside_gitdir
4	4	------> echo $operation
4	4	------> echo $branch
3	3	------> echo $detached
2	2	------> echo $bare
4	4	----> set -l r $rbc[1]
3	3	----> set -l b $rbc[2]
3	3	----> set -l detached $rbc[3]
2	2	----> set -l dirtystate
2	2	----> set -l stagedstate
2	2	----> set -l invalidstate
2	2	----> set -l stashstate
3	3	----> set -l untrackedfiles
3	3	----> set -l c $rbc[4]
2	2	----> set -l p
3	3	----> set -l informative_status
2	2	----> set -q __fish_git_prompt_status_order
1	5	----> if not set -q ___fish_git_prompt_init...
4	4	-----> not set -q ___fish_git_prompt_init
10	10	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
3	3	----> set -l informative
3	3	----> set -l dirty
2	2	----> set -l untracked
526	2188	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1662	1662	-----> read -lz key value
3	17	----> if not set -q dirty[1]...
5	5	-----> not set -q dirty[1]
9	9	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
7	7	----> contains dirtystate $__fish_git_prompt_status_order
1	10	----> if not set -q untracked[1]...
3	3	-----> not set -q untracked[1]
6	6	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
6	6	----> contains untrackedfiles $__fish_git_prompt_status_order
4	55	----> if test true = $inside_worktree...
6	6	-----> test true = $inside_worktree
6	35	-----> if test "$informative" = true...
4	4	------> test "$informative" = true
2	7	------> begin...
5	5	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
4	13	------> if not test "$dirty" = true...
3	3	-------> not test "$dirty" = true
3	3	-------> test "$untracked" = true
3	3	-------> test "$dirty" = true
1	5	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
4	4	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
2	10	-----> if set -q __fish_git_prompt_showupstream...
3	3	------> set -q __fish_git_prompt_showupstream
5	5	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
9	9	----> set -l branch_color $___fish_git_prompt_color_branch
5	5	----> set -l branch_done $___fish_git_prompt_color_branch_done
2	6	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
4	4	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
4	4	----> set -l f ""
15	38	----> for i in $__fish_git_prompt_status_order...
0	6	-----> if test -n "$$i"...
6	6	------> test -n "$$i"
0	5	-----> if test -n "$$i"...
5	5	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
45	53	----> set b (string replace refs/heads/ '' -- $b)
8	8	-----> string replace refs/heads/ '' -- $b
1	17	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
16	16	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
3	25	----> if test -n "$b"...
5	5	-----> test -n "$b"
6	6	-----> set b "$branch_color$b$branch_done"
3	11	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
4	4	------> test -z "$dirtystate$untrackedfiles$stagedstate"
4	4	------> test -n "$___fish_git_prompt_char_cleanstate"
0	4	----> if test -n "$c"...
4	4	-----> test -n "$c"
0	3	----> if test -n "$r"...
3	3	-----> test -n "$r"
0	3	----> if test -n "$p"...
3	3	-----> test -n "$p"
1	4	----> if test -n "$f"...
3	3	-----> test -n "$f"
6	6	----> set -l format $argv[1]
3	11	----> if test -z "$format"...
4	4	-----> test -z "$format"
4	4	-----> set format " (%s)"
21	21	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
14	6911	> fish_right_prompt
6886	6886	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
11	11	-> printf " "
9	477	> fish_title
5	468	-> if not set -q INSIDE_EMACS...
7	7	--> not set -q INSIDE_EMACS
5	5	--> set -l ssh
3	3	--> set -q SSH_TTY
6	448	--> if set -q argv[1]...
2	2	---> set -q argv[1]
36	43	---> set -l command (status current-command)
7	7	----> status current-command
3	12	---> if test "$command" = fish...
6	6	----> test "$command" = fish
3	3	----> set command
62	385	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
6	6	----> string sub -l 20 -- $command
19	317	----> prompt_pwd -d 1 -D 1
6	6	-----> set -l options h/help d/dir-length= D/full-length-dirs=
15	15	-----> argparse -n prompt_pwd $options -- $argv
1	4	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
2	2	-----> set -q argv[1]
4	4	-----> set argv $PWD
2	2	-----> set -ql _flag_d
5	5	-----> set -l fish_prompt_pwd_dir_length $_flag_d
3	3	-----> set -q fish_prompt_pwd_dir_length
3	3	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
4	4	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
9	246	-----> for path in $argv...
35	44	------> set -l realhome (string escape --style=regex -- ~)
9	9	-------> string escape --style=regex -- ~
36	50	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
14	14	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
4	143	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
5	5	-------> test "$fish_prompt_pwd_dir_length" -eq 0
3	3	-------> set -l full
2	86	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
3	3	--------> test $fish_prompt_pwd_full_dirs -gt 0
32	72	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
32	40	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
8	8	----------> math $fish_prompt_pwd_full_dirs - 1
5	5	--------> set tmp $all[1]
4	4	--------> set full $all[2..]
33	45	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
12	12	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
24	55	> __direnv_export_eval_2 'git log'
3	19	-> if set -q __direnv_export_again...
16	16	--> set -q __direnv_export_again
12	12	-> functions --erase __direnv_cd_hook
13	26	> __fish_disable_bracketed_paste 'git log'
13	13	-> printf "\e[?2004l"
8	17	> __fish_disable_focus 'git log'
9	9	-> echo -n \e\[\?1004l
9	504	> fish_title git\ log
10	495	-> if not set -q INSIDE_EMACS...
7	7	--> not set -q INSIDE_EMACS
5	5	--> set -l ssh
3	3	--> set -q SSH_TTY
3	470	--> if set -q argv[1]...
4	4	---> set -q argv[1]
87	463	---> echo -- $ssh (string sub -l 20 -- $argv[1]) (prompt_pwd -d 1 -D 1)
10	10	----> string sub -l 20 -- $argv[1]
25	366	----> prompt_pwd -d 1 -D 1
9	9	-----> set -l options h/help d/dir-length= D/full-length-dirs=
20	20	-----> argparse -n prompt_pwd $options -- $argv
2	6	-----> if set -q _flag_help...
4	4	------> set -q _flag_help
3	3	-----> set -q argv[1]
5	5	-----> set argv $PWD
2	2	-----> set -ql _flag_d
6	6	-----> set -l fish_prompt_pwd_dir_length $_flag_d
2	2	-----> set -q fish_prompt_pwd_dir_length
6	6	-----> set -l fulldirs 0
3	3	-----> set -ql _flag_D
5	5	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
10	272	-----> for path in $argv...
41	52	------> set -l realhome (string escape --style=regex -- ~)
11	11	-------> string escape --style=regex -- ~
40	63	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
23	23	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
6	147	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
8	8	-------> test "$fish_prompt_pwd_dir_length" -eq 0
4	4	-------> set -l full
5	85	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
3	3	--------> test $fish_prompt_pwd_full_dirs -gt 0
28	70	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
31	42	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
11	11	----------> math $fish_prompt_pwd_full_dirs - 1
4	4	--------> set tmp $all[1]
3	3	--------> set full $all[2..]
29	44	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
15	15	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
320625	320625	> git log
36	80	> __fish_enable_focus 'git log'
44	44	-> echo -n \e\[\?1004h
24	2228	> __direnv_export_eval
2163	2163	-> "/nix/store/bri4plkp5ykp2dvjs1bwzd92z7grp6h7-direnv-2.34.0/bin/direnv" export fish | source
10	41	-> if test "$direnv_fish_mode" != "disable_arrow"...
23	23	--> test "$direnv_fish_mode" != "disable_arrow"
8	8	--> function __direnv_cd_hook --on-variable PWD...
10	23	> __fish_enable_bracketed_paste
13	13	-> printf "\e[?2004h"
12	64	> fish_mode_prompt
23	52	-> fish_default_mode_prompt
3	29	--> if test "$fish_key_bindings" = fish_vi_key_bindings...
19	19	---> test "$fish_key_bindings" = fish_vi_key_bindings
7	7	---> test "$fish_key_bindings" = fish_hybrid_key_bindings
21	7097	> fish_prompt
11	11	-> set -l last_pipestatus $pipestatus
7	7	-> set -lx __fish_last_status $status
40	50	-> set -l normal (set_color normal)
10	10	--> set_color normal
5	5	-> set -q fish_color_status
9	9	-> set -l color_cwd $fish_color_cwd
7	7	-> set -l suffix '>'
3	53	-> if functions -q fish_is_root_user...
7	7	--> functions -q fish_is_root_user
11	43	--> fish_is_root_user
1	19	---> if test "$EUID" = 0 2>/dev/null...
18	18	----> test "$EUID" = 0 2>/dev/null
1	10	---> if contains -- $USER root toor Administrator...
9	9	----> contains -- $USER root toor Administrator
3	3	---> return 1
5	5	-> set -l bold_flag --bold
5	5	-> set -q __fish_prompt_status_generation
2	9	-> if test $__fish_prompt_status_generation = $status_generation...
7	7	--> test $__fish_prompt_status_generation = $status_generation
7	7	-> set __fish_prompt_status_generation $status_generation
41	52	-> set -l status_color (set_color $fish_color_status)
11	11	--> set_color $fish_color_status
37	49	-> set -l statusb_color (set_color $bold_flag $fish_color_status)
12	12	--> set_color $bold_flag $fish_color_status
39	137	-> set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)
35	98	--> __fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus
9	9	---> set -l last_status
4	12	---> if set -q __fish_last_status...
3	3	----> set -q __fish_last_status
5	5	----> set last_status $__fish_last_status
6	6	---> set -l left_brace $argv[1]
4	4	---> set -l right_brace $argv[2]
9	9	---> set -l separator $argv[3]
5	5	---> set -l brace_sep_color $argv[4]
4	4	---> set -l status_color $argv[5]
5	5	---> set -e argv[1 2 3 4 5]
1	4	---> if not set -q argv[1]...
3	3	----> not set -q argv[1]
0	5	---> if not contains $last_status 0 141...
5	5	----> not contains $last_status 0 141
135	6670	-> echo -n -s (prompt_login)' ' (set_color $color_cwd) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
16	296	--> prompt_login
0	5	---> if not set -q __fish_machine...
5	5	----> not set -q __fish_machine
2	5	---> if set -q __fish_machine[1]...
3	3	----> set -q __fish_machine[1]
6	6	---> set -l color_host $fish_color_host
0	3	---> if set -q SSH_TTY...
3	3	----> set -q SSH_TTY
190	261	---> echo -n -s (set_color $fish_color_user) "$USER" (set_color normal) @ (set_color $color_host) (prompt_hostname) (set_color normal)
11	11	----> set_color $fish_color_user
7	7	----> set_color normal
5	5	----> set_color $color_host
9	37	----> prompt_hostname
28	28	-----> string replace -r -- "\..*" "" $hostname
11	11	----> set_color normal
10	10	--> set_color $color_cwd
21	379	--> prompt_pwd
8	8	---> set -l options h/help d/dir-length= D/full-length-dirs=
11	11	---> argparse -n prompt_pwd $options -- $argv
2	5	---> if set -q _flag_help...
3	3	----> set -q _flag_help
3	3	---> set -q argv[1]
5	5	---> set argv $PWD
3	3	---> set -ql _flag_d
3	3	---> set -q fish_prompt_pwd_dir_length
5	5	---> set -l fish_prompt_pwd_dir_length 1
3	3	---> set -l fulldirs 0
2	2	---> set -ql _flag_D
2	2	---> set -q fish_prompt_pwd_full_dirs
3	3	---> set -l fish_prompt_pwd_full_dirs 1
11	305	---> for path in $argv...
42	53	----> set -l realhome (string escape --style=regex -- ~)
11	11	-----> string escape --style=regex -- ~
38	57	----> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
19	19	-----> string replace -r '^'"$realhome"'($|/)' '~$1' $path
4	184	----> if test "$fish_prompt_pwd_dir_length" -eq 0...
8	8	-----> test "$fish_prompt_pwd_dir_length" -eq 0
5	5	-----> set -l full
5	113	-----> if test $fish_prompt_pwd_full_dirs -gt 0...
5	5	------> test $fish_prompt_pwd_full_dirs -gt 0
36	93	------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
43	57	-------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
14	14	--------> math $fish_prompt_pwd_full_dirs - 1
6	6	------> set tmp $all[1]
4	4	------> set full $all[2..]
36	54	-----> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
18	18	------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
11	5850	--> fish_vcs_prompt
55	5839	---> fish_git_prompt $argv
2	160	----> if not command -sq git...
158	158	-----> not command -sq git
0	6	----> if functions -q __fish_git_prompt_ready...
6	6	-----> functions -q __fish_git_prompt_ready
76	1543	----> set -l repo_info (command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null)
1467	1467	-----> command git rev-parse --git-dir --is-inside-git-dir --is-bare-repository --is-inside-work-tree HEAD 2>/dev/null
6	6	----> test -n "$repo_info"
5	5	----> set -l git_dir $repo_info[1]
4	4	----> set -l inside_gitdir $repo_info[2]
4	4	----> set -l bare_repo $repo_info[3]
3	3	----> set -l inside_worktree $repo_info[4]
2	2	----> set -q repo_info[5]
3	3	----> set -l sha $repo_info[5]
37	1477	----> set -l rbc (__fish_git_prompt_operation_branch_bare $repo_info)
26	1440	-----> __fish_git_prompt_operation_branch_bare $repo_info
5	5	------> set -l git_dir $argv[1]
3	3	------> set -l inside_gitdir $argv[2]
3	3	------> set -l bare_repo $argv[3]
2	2	------> set -q argv[5]
3	3	------> set -l sha $argv[5]
3	3	------> set -l branch
3	3	------> set -l operation
2	2	------> set -l detached no
2	2	------> set -l bare
2	2	------> set -l step
2	2	------> set -l total
18	47	------> if test -d $git_dir/rebase-merge...
7	7	-------> test -d $git_dir/rebase-merge
3	22	-------> if test -d $git_dir/rebase-apply...
5	5	--------> test -d $git_dir/rebase-apply
4	4	--------> test -f $git_dir/MERGE_HEAD
4	4	--------> test -f $git_dir/CHERRY_PICK_HEAD
3	3	--------> test -f $git_dir/REVERT_HEAD
3	3	--------> test -f $git_dir/BISECT_LOG
1	6	------> if test -n "$step" -a -n "$total"...
5	5	-------> test -n "$step" -a -n "$total"
4	1313	------> if test -z "$branch"...
3	3	-------> test -z "$branch"
1	1306	-------> if not set branch (command git symbolic-ref HEAD 2>/dev/null)...
41	1305	--------> not set branch (command git symbolic-ref HEAD 2>/dev/null)
1264	1264	---------> command git symbolic-ref HEAD 2>/dev/null
0	6	------> if test true = $inside_gitdir...
6	6	-------> test true = $inside_gitdir
4	4	------> echo $operation
3	3	------> echo $branch
3	3	------> echo $detached
2	2	------> echo $bare
4	4	----> set -l r $rbc[1]
3	3	----> set -l b $rbc[2]
3	3	----> set -l detached $rbc[3]
3	3	----> set -l dirtystate
3	3	----> set -l stagedstate
2	2	----> set -l invalidstate
2	2	----> set -l stashstate
2	2	----> set -l untrackedfiles
3	3	----> set -l c $rbc[4]
2	2	----> set -l p
3	3	----> set -l informative_status
2	2	----> set -q __fish_git_prompt_status_order
1	5	----> if not set -q ___fish_git_prompt_init...
4	4	-----> not set -q ___fish_git_prompt_init
9	9	----> set -l space "$___fish_git_prompt_color$___fish_git_prompt_char_stateseparator$___fish_git_prompt_color_done"
2	2	----> set -l informative
2	2	----> set -l dirty
3	3	----> set -l untracked
538	2208	----> command git config -z --get-regexp 'bash\.(showInformativeStatus|showDirtyState|showUntrackedFiles)' 2>/dev/null | while read -lz key value
        switch $key
            case bash.showinformativestatus
                set informative $value
            case bash.showdirtystate
                set dirty $value
            case bash.showuntrackedfiles
                set untracked $value
        end
    end
1670	1670	-----> read -lz key value
3	17	----> if not set -q dirty[1]...
5	5	-----> not set -q dirty[1]
9	9	-----> contains -- "$__fish_git_prompt_showdirtystate" yes true 1
7	7	----> contains dirtystate $__fish_git_prompt_status_order
3	11	----> if not set -q untracked[1]...
3	3	-----> not set -q untracked[1]
5	5	-----> contains -- "$__fish_git_prompt_showuntrackedfiles" yes true 1
6	6	----> contains untrackedfiles $__fish_git_prompt_status_order
6	54	----> if test true = $inside_worktree...
5	5	-----> test true = $inside_worktree
5	34	-----> if test "$informative" = true...
4	4	------> test "$informative" = true
2	7	------> begin...
5	5	-------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
4	13	------> if not test "$dirty" = true...
3	3	-------> not test "$dirty" = true
3	3	-------> test "$untracked" = true
3	3	-------> test "$dirty" = true
1	5	------> if contains -- "$__fish_git_prompt_showstashstate" yes true 1...
4	4	-------> contains -- "$__fish_git_prompt_showstashstate" yes true 1
2	9	-----> if set -q __fish_git_prompt_showupstream...
3	3	------> set -q __fish_git_prompt_showupstream
4	4	------> contains -- "$__fish_git_prompt_show_informative_status" yes true 1
9	9	----> set -l branch_color $___fish_git_prompt_color_branch
6	6	----> set -l branch_done $___fish_git_prompt_color_branch_done
1	6	----> if contains -- "$__fish_git_prompt_showcolorhints" yes true 1...
5	5	-----> contains -- "$__fish_git_prompt_showcolorhints" yes true 1
4	4	----> set -l f ""
15	39	----> for i in $__fish_git_prompt_status_order...
1	7	-----> if test -n "$$i"...
6	6	------> test -n "$$i"
1	5	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
1	4	-----> if test -n "$$i"...
3	3	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
0	4	-----> if test -n "$$i"...
4	4	------> test -n "$$i"
45	54	----> set b (string replace refs/heads/ '' -- $b)
9	9	-----> string replace refs/heads/ '' -- $b
1	17	----> if string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"...
16	16	-----> string match -qr '^\d+$' "$__fish_git_prompt_shorten_branch_len"
3	24	----> if test -n "$b"...
5	5	-----> test -n "$b"
6	6	-----> set b "$branch_color$b$branch_done"
2	10	-----> if test -z "$dirtystate$untrackedfiles$stagedstate"...
4	4	------> test -z "$dirtystate$untrackedfiles$stagedstate"
4	4	------> test -n "$___fish_git_prompt_char_cleanstate"
1	4	----> if test -n "$c"...
3	3	-----> test -n "$c"
1	4	----> if test -n "$r"...
3	3	-----> test -n "$r"
1	4	----> if test -n "$p"...
3	3	-----> test -n "$p"
1	4	----> if test -n "$f"...
3	3	-----> test -n "$f"
6	6	----> set -l format $argv[1]
4	11	----> if test -z "$format"...
3	3	-----> test -z "$format"
4	4	-----> set format " (%s)"
23	23	----> printf "%s$format%s" "$___fish_git_prompt_color_prefix" "$___fish_git_prompt_color_prefix_done$c$b$f$r$p$informative_status$___fish_git_prompt_color_suffix" "$___fish_git_prompt_color_suffix_done"
13	6769	> fish_right_prompt
6747	6747	-> /nix/store/n64wp2008d83frv3319fa6pyv87bp49x-any-nix-shell-1.2.1/bin/nix-shell-info
9	9	-> printf " "
9	418	> fish_title
13	409	-> if not set -q INSIDE_EMACS...
6	6	--> not set -q INSIDE_EMACS
4	4	--> set -l ssh
3	3	--> set -q SSH_TTY
3	383	--> if set -q argv[1]...
3	3	---> set -q argv[1]
31	36	---> set -l command (status current-command)
5	5	----> status current-command
2	10	---> if test "$command" = fish...
5	5	----> test "$command" = fish
3	3	----> set command
54	331	---> echo -- $ssh (string sub -l 20 -- $command) (prompt_pwd -d 1 -D 1)
5	5	----> string sub -l 20 -- $command
15	272	----> prompt_pwd -d 1 -D 1
5	5	-----> set -l options h/help d/dir-length= D/full-length-dirs=
13	13	-----> argparse -n prompt_pwd $options -- $argv
1	4	-----> if set -q _flag_help...
3	3	------> set -q _flag_help
2	2	-----> set -q argv[1]
4	4	-----> set argv $PWD
2	2	-----> set -ql _flag_d
4	4	-----> set -l fish_prompt_pwd_dir_length $_flag_d
1	1	-----> set -q fish_prompt_pwd_dir_length
2	2	-----> set -l fulldirs 0
2	2	-----> set -ql _flag_D
4	4	-----> set -l fish_prompt_pwd_full_dirs $_flag_D
2	2	-----> set -q fish_prompt_pwd_full_dirs
7	212	-----> for path in $argv...
26	34	------> set -l realhome (string escape --style=regex -- ~)
8	8	-------> string escape --style=regex -- ~
28	40	------> set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $path)
12	12	-------> string replace -r '^'"$realhome"'($|/)' '~$1' $path
5	131	------> if test "$fish_prompt_pwd_dir_length" -eq 0...
5	5	-------> test "$fish_prompt_pwd_dir_length" -eq 0
3	3	-------> set -l full
3	77	-------> if test $fish_prompt_pwd_full_dirs -gt 0...
3	3	--------> test $fish_prompt_pwd_full_dirs -gt 0
28	63	--------> set -l all (string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp)
27	35	---------> string split -m (math $fish_prompt_pwd_full_dirs - 1) -r / $tmp
8	8	----------> math $fish_prompt_pwd_full_dirs - 1
4	4	--------> set tmp $all[1]
4	4	--------> set full $all[2..]
29	41	-------> string join / -- (string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp) $full
12	12	--------> string replace -ar -- '(\.?[^/]{'"$fish_prompt_pwd_dir_length"'})[^/]*/' '$1/' $tmp
25	58	> __fish_disable_bracketed_paste
33	33	-> printf "\e[?2004l"
