# fire up ssh-agent and gpg-agent
source ~/.config/fish/ssh_gpg.fish

# opam stuff
eval (opam env)

# more local binary paths
fish_add_path ~/.local/bin
fish_add_path ~/.ghcup/bin
fish_add_path ~/.cabal/bin
