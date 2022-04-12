;;; completion.el -*- lexical-binding: t; -*-

(straight-use-package 'vertico)
(straight-use-package 'consult)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'savehist)
(straight-use-package 'corfu)
(straight-use-package 'cape)
(straight-use-package 'dabbrev)
(straight-use-package 'tempel)

(defun k-completion/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      (if (string-match-p "/." (minibuffer-contents))
	  (zap-up-to-char (- arg) ?/)
	(delete-minibuffer-contents))
    (backward-kill-word arg)))

(require 'vertico)

(setq vertico-cycle nil)

(vertico-mode 1)

(savehist-mode)

(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

(require 'orderless)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(setq corfu-cycle t)
(setq corfu-auto t)
(setq corfu-quit-at-boundary nil)
(setq corfu-quit-no-match t)
(setq corfu-echo-documentation nil)
(setq corfu-auto-prefix 1)
(setq corfu-preselect-first t)
(corfu-global-mode)

(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-keyword)
(add-to-list 'completion-at-point-functions #'cape-tex)

(setq register-preview-delay 0)
(setq register-preview-function #'consult-register-format)
(setq consult-project-root-function
      (lambda ()
        (when-let (project (project-current))
          (car (project-roots project)))))

(require 'tempel)
(define-key tempel-map (kbd "TAB") 'tempel-next)
(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook 'org-mode-hook 'tempel-setup-capf)
(add-hook 'LaTeX-mode-hook 'tempel-setup-capf)
(with-eval-after-load "latex"
  (define-key LaTeX-mode-map (kbd "C-t") 'tempel-expand))

(provide 'completion)
