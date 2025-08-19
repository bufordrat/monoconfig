# add homebrew to path
fish_add_path /opt/homebrew/opt/make/libexec/gnubin
fish_add_path /opt/homebrew/bin

# add ~/local/bin to path for Agda
fish_add_path ~/.local/bin

# opam stuff
eval (opam env)

# ghcup stuff
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /Users/teichman/.ghcup/bin $PATH # ghcup-env
