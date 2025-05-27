# prompt
if test "$TERM" = "dumb"
    function fish_prompt
	echo "\$ "
    end
else
    function fish_prompt
	echo "trampless\$ "
    end
end
