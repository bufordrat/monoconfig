if status --is-login; and not set -q SSH_CLIENT
   eval (ssh-agent -c)
   ssh-add
   gpg --decrypt --pinentry-mode loopback dummy.gpg > /dev/null
   emacs --daemon
end

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/teichman/.ghcup/bin # ghcup-env

if test -n "$EMACS"
  set -x TERM eterm-color
end

function fish_title
  true
end

set -x DIFFPROG diffmerge
set -x EDITOR figure_out_editor_variable

eval (opam env)

set -U fish_user_paths /home/teichman/.local/share/gem/ruby/3.3.0/bin $fish_user_paths
