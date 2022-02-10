(load "~/.config/stumpwm/color.lisp")

(ql:quickload :clx-truetype)
(load-module "ttf-fonts")

(set-font `(,(make-instance 'xft:font :family "Liga SFMono Nerd Font" :subfamily "Medium" :size 12 :antialiasing t)
            ,(make-instance 'xft:font :family "Liga SFMono Nerd Font" :subfamily "Medium" :size 12 :antialiasing t)))


(set-border-color k-tnight15)
(set-focus-color k-tnight15)
(set-unfocus-color k-tnight14)
(set-float-focus-color k-tnight15)
(set-float-unfocus-color k-tnight14)

(set-fg-color k-tnight11)
(set-bg-color k-tnight17)

(setf *normal-border-width*       0
      *float-window-border*       0
      *float-window-title-height* 15
      *window-border-style*       :none
      *window-format*             "%n:%t")

(setf *input-window-gravity*     :top
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity*   :top)


;; (load-module "swm-gaps")

;; (setf swm-gaps:*head-gaps-size*  0
;;       swm-gaps:*inner-gaps-size* 5
;;       swm-gaps:*outer-gaps-size* 0)

;; (when *initializing*
;;   (swm-gaps:toggle-gaps))
