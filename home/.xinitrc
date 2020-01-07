# this file is only run if the line: allow-user-xsession is in /etc/X11/Xsession.options
(sleep 2; xkbcomp -w0 -I$HOME/.xkb $HOME/.xkb/keymap/kbd $DISPLAY) &
disown -a

date >> ~/logins
