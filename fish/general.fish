# prompt
if test "$TERM" = "dumb"
    function fish_prompt
	echo "\$ "
    end
else
    function fish_prompt
	set -l exit_code $status
	set -l hostname_prefix ""
	set -l directory_prefix ""
	
	# if we're not who we logged in as, display the username
	if test $LOGNAME != $USER
	    set hostname_prefix "at "
	    set directory_prefix "in "
	    
	    if test $USER = "root"
		set_color red
	    else
		set_color green
	    end
	    printf "%s " $USER
	    set_color normal
	end
	
	# if we're ssh'ed, display the hostname
	if test -n "$SSH_CONNECTION"
	    set directory_prefix "in "
	    
	    printf "%s" $hostname_prefix
	    set_color yellow
	    printf "%s " (hostname)
	    set_color normal
	end
	
	# print PWD
	printf "%s" $directory_prefix
	set_color blue
	printf "%s " (prompt_pwd)
	set_color normal
	
	# prompt char - red if previous command failed
	if test $exit_code -ne 0
	    set_color red
	end
	printf "> "
	set_color normal
    end
end

# homebin path
fish_add_path ~/bin

# umask
umask 022

# don't show annoying welcome message
set fish_greeting ""

# colorized manpages
set -x LESS_TERMCAP_mb (printf "\033[01;31m")
set -x LESS_TERMCAP_md (printf "\033[01;31m")
set -x LESS_TERMCAP_me (printf "\033[0m")
set -x LESS_TERMCAP_se (printf "\033[0m")
set -x LESS_TERMCAP_so (printf "\033[01;44;33m")
set -x LESS_TERMCAP_ue (printf "\033[0m")
set -x LESS_TERMCAP_us (printf "\033[01;32m")

# global variables
set -gx DIFFPROG diffmerge
set -gx EDITOR figure_out_editor_variable
set -gx MANPATH /usr/share/man
