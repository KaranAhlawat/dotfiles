(load "~/.config/stumpwm/color.lisp")

(ql:quickload :clx-truetype)
(load-module "ttf-fonts")

(set-font `(,(make-instance 'xft:font :family "JetBrainsMono Nerd Font" :subfamily "Regular" :size 12 :antialiasing t)
             ,(make-instance 'xft:font :family "JetBrainsMono Nerd Font" :subfamily "Regular" :size 12 :antialiasing t)))
