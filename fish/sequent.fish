# universal variables
set -U fish_user_paths /home/teichman/.local/share/gem/ruby/3.3.0/bin $fish_user_paths

# start ssh-agent and gpg-agent upon login
if status --is-login; and not set -q SSH_CLIENT
   eval (ssh-agent -c)
   ssh-add
   gpg --decrypt --pinentry-mode loopback dummy.gpg > /dev/null
   emacs --daemon
end

# opam stuff
eval (opam env)
