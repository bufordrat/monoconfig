# show all directories in prompt
# set -x fish_prompt_pwd_dir_length 1


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

