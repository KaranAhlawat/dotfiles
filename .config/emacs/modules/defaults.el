;;; defaults.el -*- lexical-binding: t; -*-

(straight-use-package 'no-littering)
(straight-use-package 'autothemer)

(require 'no-littering)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq global-auto-revert-non-file-buffers t)

(setq native-comp-async-report-warnings-errors nil)

(defun k-defaults/display-startup-time ()
  (message "Emacs loaded in %s with %d GCs."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract after-init-time before-init-time)))
	   gcs-done))

(add-hook 'emacs-startup-hook #'k-defaults/display-startup-time)
(global-auto-revert-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 2
              cursor-type 'box)
(setq tab-always-indent 'complete)

(fset 'yes-or-no-p 'y-or-n-p)

(setq kill-do-not-save-duplicates t)

(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)
(setq scroll-conservatively 101)
(setq scroll-margin 1)
(setq scroll-preserve-screen-position t)
(setq custom-theme-directory "~/.config/emacs/modules/")

(blink-cursor-mode -1)

(provide 'defaults)
				
