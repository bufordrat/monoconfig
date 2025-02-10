# fire up ssh-agent and gpg-agent
source ~/.config/fish/ssh_gpg.fish

# opam stuff
eval (opam env)

# ghcup stuff
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin $PATH /home/teichman/.ghcup/bin

# more local binary paths
fish_add_path ~/.local/bin

