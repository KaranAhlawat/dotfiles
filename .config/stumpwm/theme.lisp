(load "~/.config/stumpwm/color.lisp")

(ql:quickload :clx-truetype)
(load-module "ttf-fonts")

(set-font `(,(make-instance 'xft:font :family "JetBrainsMono Nerd Font" :subfamily "Regular" :size 12 :antialiasing t)
             ,(make-instance 'xft:font :family "JetBrainsMono Nerd Font" :subfamily "Regular" :size 12 :antialiasing t)))


(set-border-color phundrak-nord1)
(set-focus-color phundrak-nord1)
(set-unfocus-color phundrak-nord3)
(set-float-focus-color phundrak-nord1)
(set-float-unfocus-color phundrak-nord3)

(set-fg-color phundrak-nord4)
(set-bg-color phundrak-nord1)

(setf *normal-border-width*       0
      *float-window-border*       0
      *float-window-title-height* 15
      *window-border-style*       :none
      *window-format*             "%n:%t")

(setf *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity*   :top)


(load-module "swm-gaps")

(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)

(when *initializing*
  (swm-gaps:toggle-gaps))
