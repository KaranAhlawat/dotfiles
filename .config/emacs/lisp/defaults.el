;;; defaults.el --- Some sensible defaults I like -*- lexical-binding: t; -*-
;;; Commentary:
;;; Setup defaults
;;; Code:

;; Littering is bad. Stop it. Get some help.
(use-package no-littering
  :straight t
  :config
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(require 'autorevert)
(setq global-auto-revert-non-file-buffers t)

;; Measure the startup time of  emacs
(defun conf/display-startup-time ()
  "Dispaly the time it took for Emacs to start in the minibuffer."
  (message "Emacs loaded in %s with %d GCs."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'conf/display-startup-time)
(global-auto-revert-mode 1)

;; Try to tame the TAB
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default cursor-type t)
(setq-default tab-always-indent 't)
(setq-default tab-first-completion 'eol)

;; Shorten yes or no to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; Save space in the kill ring
(setq kill-do-not-save-duplicates t)

;; Some scroll related settings
(setq auto-window-vscroll nil)
(setq scroll-margin 1)
(setq scroll-preserve-screen-position t)
(pixel-scroll-precision-mode)

;; .dir-locals
(setq
 enable-local-variables t
 enable-dir-local-variables t)

;; Exec path is taken from shell
(use-package exec-path-from-shell
  :straight t
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  :config (exec-path-from-shell-initialize))

(use-package whitespace
  :straight (:type built-in)
  :init
  (setq whitespace-style '(face newline newline-mark))
  (setq whitespace-display-mappings
        '((newline-mark 10 [10])))
  :config
  (global-whitespace-mode))

(provide 'defaults)
;; defaults.el ends here
