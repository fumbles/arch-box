#!/bin/sh

#~/.xinitrc


#exec cd-launch-session startxfce4
xsetroot -cursor_name left_ptr 
/home/daniel/bin/mpcstat &	##helps with notify queue
/home/daniel/bin/mpc-notify/notify.py &
habak -ms /home/daniel/Pictures/Lgic_Designmod2.png
#habak -ms /home/daniel/Pictures/fision-1280-800.jpg
gnome-screensaver &
guake &
#exec xmonad

case $1 in
gnome) exec ck-launch-session startxfce4
	;;
awesome) exec awesome
	;;
openbox) exec openbox-session
	;;
gnome) exec gnome-session
	;;
xmonad) exec xmonad
	;;
esac
