;;; keybinds.el -*- leixcal-binding: t; -*-

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-x C-p") 'find-file-at-point)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "M-i") 'consult-imenu)
(global-set-key (kbd "C-h a") 'consult-apropos)
(global-set-key (kbd "C-c q f") 'delete-frame)
(global-set-key (kbd "C-c q e") 'kill-emacs)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-\\") 'backward-delete-ws-till-char)

;; Setup of smartparens mode
(require 'smartparens)

(defun wrap-with-parens (&optional args)
  (interactive "p")
  (sp-wrap-with-pair "("))

(defun wrap-with-brackets (&optional args)
  (interactive "p")
  (sp-wrap-with-pair "["))

(defun wrap-with-braces (&optional args)
  (interactive "p")
  (sp-wrap-with-pair "{"))

(defun wrap-with-double-quotes (&optional args)
  (interactive "p")
  (sp-wrap-with-pair "\""))


(dolist (cmd '(("C-M-a" . sp-beginning-of-sexp)
               ("C-M-e" . sp-end-of-sexp)
               ("C-<down>" . sp-down-sexp)
               ("C-<up>"   . sp-up-sexp)
               ("M-<down>" . sp-backward-down-sexp)
               ("M-<up>"   . sp-backward-up-sexp)

               ("C-M-f" . sp-forward-sexp)
               ("C-M-b" . sp-backward-sexp)

               ("C-M-n" . sp-next-sexp)
               ("C-M-p" . sp-previous-sexp)

               ("C-S-f" . sp-forward-symbol)
               ("C-S-b" . sp-backward-symbol)

               ("C-<right>" . sp-forward-slurp-sexp)
               ("M-<right>" . sp-forward-barf-sexp)
               ("C-<left>"  . sp-backward-slurp-sexp)
               ("M-<left>"  . sp-backward-barf-sexp)

               ("C-M-t" . sp-transpose-sexp)
               ("C-M-k" . sp-kill-sexp)
               ("C-k"   . sp-kill-hybrid-sexp)
               ("M-k"   . sp-backward-kill-sexp)
               ("C-M-w" . sp-copy-sexp)

               ("M-<backspace>" . backward-kill-word)
               ("C-<backspace>" . sp-backward-kill-word)

               ("M-[" . sp-backward-unwrap-sexp)
               ("M-]" . sp-unwrap-sexp)

               ("C-x C-t" . sp-transpose-hybrid-sexp)

               ("C-c ("  . wrap-with-parens)
               ("C-c ["  . wrap-with-brackets)
               ("C-c {"  . wrap-with-braces)
               ("C-c \"" . wrap-with-double-quotes)))
                
  (let ((key (car cmd))
        (fn  (cdr cmd)))
    (define-key smartparens-mode-map (kbd key) fn)))

(provide 'keybinds)
