# global variables
set -U fish_user_paths /opt/homebrew/bin $fish_user_paths

# opam stuff
eval (opam env)

# ghcup stuff
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME ; set -gx PATH $HOME/.cabal/bin /Users/teichman/.ghcup/bin $PATH # ghcup-env
