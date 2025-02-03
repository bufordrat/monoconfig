###########
# coreutils
###########

# make xclip use system clipboard
function xclip
  command xclip -selection clipboard $argv
end
