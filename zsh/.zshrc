UNAMEOS=$(uname -o | cut -d. -f1)
if [[ "$UNAMEOS" == localhost ]]
then HOST=pixel10
else HOST=$UNAMEOS
fi
. ~/zshrc/general_zshrc
if [ -e ~/zshrc/"$HOST"_zshrc ] 
then . ~/zshrc/"$HOST"_zshrc
else :
fi
