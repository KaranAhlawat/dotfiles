#!usr/bin/sh

exec dbus-launch --exit-with-session emacs -mm -l ~/.emacs.d/exwm/config.el
