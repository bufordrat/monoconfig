# opam stuff
eval (opam env)

# ghcup
fish_add_path ~/.ghcup/bin

# agda
fish_add_path ~/.cabal/bin

# more local binary paths
fish_add_path ~/.local/bin

# make fish aware of ssh agent
set -gx SSH_AUTH_SOCK $XDG_RUNTIME_DIR/ssh-agent.socket

# linux man pages
set -gx MANPATH /usr/share/man
