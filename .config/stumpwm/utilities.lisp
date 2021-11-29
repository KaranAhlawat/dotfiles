(in-package :stumpwm)

(ql:quickload :dbus)

(load-module "stump-nm")

(define-key *root-map* (kbd "W") "nm-list-wireless-networks")

(load-module "notify")

(notify:notify-server-toggle)

(load "~/.config/stumpwm/color.lisp")

(setf notify:*notify-server-title-color* "^2"
      notify:*notify-server-body-color* "^7")
