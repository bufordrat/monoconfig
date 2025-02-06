# start ssh-agent and gpg-agent upon login
if status --is-login; and not set -q SSH_CLIENT
   eval (ssh-agent -c)
   ssh-add
   gpg --decrypt --pinentry-mode loopback dummy.gpg > /dev/null
   emacs --daemon
end
