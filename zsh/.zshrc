. ~/zshrc/general_zshrc
if [ -e ~/zshrc/$(uname -n | cut -d. -f1)_zshrc ] 
then . ~/zshrc/$(uname -n | cut -d. -f1)_zshrc
else :
fi
