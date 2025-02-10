# start ssh-agent and gpg-agent upon login
if status --is-login; and not set -q SSH_CLIENT
   eval (ssh-agent -c)
   ssh-add
   while not gpg -d --pinentry=loopback dummy.gpg 1> /dev/null 2>&1 ; : ; end
   emacs --daemon
end
