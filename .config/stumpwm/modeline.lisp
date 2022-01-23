(setf *mode-line-timeout* 2)
(setf *time-modeline-string* " %a %b%d|%I:%M %p")

(setf *group-format* "%t")

(setf *window-format* "%n : %25t")

(load "~/.config/stumpwm/color.lisp")

(setf *mode-line-background-color* k-tnight17
      *mode-line-foreground-color* k-tnight10)

(setf *mode-line-border-color* k-tnight10
      *mode-line-border-width* 0
      *mode-line-pad-x* 20
      *mode-line-pad-y* 2
      *mode-line-position* :top)

(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "net")
(load-module "wifi")
(load-module "stumptray")

(setf cpu::*cpu-modeline-fmt*        "%c"
      cpu::*cpu-usage-modeline-fmt*  "^f1 ^f0^[~A~@d%^]"
      mem::*mem-modeline-fmt*        "^f1^f0%p"
      wifi::*wifi-modeline-fmt*      "^f1  %e"
      wifi::*use-colors*             nil
      *hiddn-window-color*           "^**"
      *mode-line-highlight-template* "[~A]")  ;;" ~A "

(setf stumptray:*tray-placeholder-pixels-per-space* 14)

(defvar *mode-line-formatter-list*
  '(("%g") ("%W") ("^>") ("%d") ("%I") ("%C") ("%B") ("%T"))
  "List of formatters for the modeline.")

(defvar *modeline-colors*
  `(("." ".")
    (,k-tnight10 ,k-tnight17)
    (,k-tnight17 ,k-tnight01)
    (,k-tnight17 ,k-tnight17)
    (,k-tnight17 ,k-tnight00)
    (,k-tnight17 ,k-tnight09)
    (,k-tnight17 ,k-tnight06)
    (,k-tnight17 ,k-tnight11)
    (,k-tnight10 ,k-tnight17)))

(defun generate-modeline (elements colors)
  "Generate a modeline for StumpWM. The user has to specify a list of colors manually, with the first element being a dummy element"
  (when (and elements colors)
    (let* ((current-element (car elements))
           (formatter       (car current-element))
           (prev-colors     (car colors))
           (curr-colors     (cadr colors))
           (curr-fg         (car curr-colors))
           (curr-bg         (cadr curr-colors))
           (prev-bg         (cadr prev-colors)))
      (cons
       (format nil
               " ^(:fg \"~A\")^(:bg \"~A\")^(:fg \"~A\") "
               prev-bg
               curr-bg
               curr-fg)
       (cons (format nil "~A" formatter)
             (generate-modeline (cdr elements) (cdr colors)))))))

(defcommand reload-modeline () ()
            "Reload modeline."
            (setf *screen-mode-line-format*
                 (cdr (generate-modeline *mode-line-formatter-list* *modeline-colors*))))

(reload-modeline)


