(setf *mode-line-timeout* 2)
(setf *time-modeline-string* " %a %b %Y|%I:%M %p")

(setf *group-format* "%t")

(setf *window-format* "%n : %25t")

(load "~/.config/stumpwm/color.lisp")

(setf *mode-line-background-color* phundrak-nord1
      *mode-line-foreground-color* phundrak-nord5)

(setf *mode-line-border-color* phundrak-nord1
      *mode-line-border-width* 0)

(load-module "battery-portable")
(load-module "cpu")
(load-module "mem")
(load-module "net")
(load-module "wifi")
(load-module "stumptray")

(setf cpu::*cpu-modeline-fmt*        "%c"
      cpu::*cpu-usage-modeline-fmt*  "^f1 ^f0^[~A~@d%^]"
      mem::*mem-modeline-fmt*        "^f1^f0%p"
      wifi::*wifi-modeline-fmt*      "^f1  %e"
      wifi::*use-colors*             nil
      *hiddn-window-color*           "^**"
      *mode-line-highlight-template* "[~A]")

(setf stumptray:*tray-placeholder-pixels-per-space* 10)

(defvar *mode-line-formatter-list*
  '(("%g") ("%W") ("^>") ("%d") ("%I") ("%B") ("%T"))
  "List of formatters for the modeline.")

(defun generate-modeline (elements &optional not-invertedp)
  "Generate a modeline for StumpWM."
  (when elements
    (cons (if not-invertedp
            (format nil
                    " ^(:fg \"~A\")^(:bg \"~A\")^f1^f0^(:fg \"~A\") "
                    phundrak-nord1
                    phundrak-nord15
                    phundrak-nord3)
            (format nil
                    " ^(:fg \"~A\")^(:bg \"~A\")^f1^f0^** "
                    phundrak-nord15
                    phundrak-nord1))
          (let* ((current-element (car elements))
                 (formatter       (car current-element))
                 (commandp        (cdr current-element)))
            (cons (if commandp
                     `(:eval (run-shell-command ,formatter t))
                     (format nil "~A" formatter))
                   (generate-modeline (cdr elements) (not not-invertedp)))))))

(defcommand reload-modeline () ()
            "Reload modeline."
            (setf *screen-mode-line-format*
                  (cdr (generate-modeline *mode-line-formatter-list*))))

(reload-modeline)


