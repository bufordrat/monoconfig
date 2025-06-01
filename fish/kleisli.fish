# fire up ssh-agent and gpg-agent
source ~/.config/fish/ssh_gpg.fish

# opam stuff
eval (opam env)

# ghcup
fish_add_path ~/.ghcup/bin

# cabal
fish_add_path ~/.cabal/bin

# binaries installed by cabal
fish_add_path ~/.local/bin
