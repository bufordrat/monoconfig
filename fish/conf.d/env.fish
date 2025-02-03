# environment variables
set -x FZF_DEFAULT_COMMAND "rg --files-with-matches ."
set -x FZF_DEFAULT_OPTS "--color=16,fg+:4 --ansi"
set -x FZF_CTRL_T_COMMAND $FZF_DEFAULT_COMMAND
set -x FZF_CTRL_R_OPTS "--reverse"
set -x GIT_DUET_CO_AUTHORED_BY 1
set -x MANPATH /usr/share/man
set -x DIFFPROG diffmerge
set -x JAVA_HOME /usr/lib/jvm/default
if test (hostname) = "cs-vm"
  set -x REQUESTS_CA_BUNDLE "/etc/ssl/certs/ca-certificates.crt"
end

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

# fzf
set fzf_bin = ~/.fzf.fish
if test -e $fzf_bin
  source $fzf_bin
end
