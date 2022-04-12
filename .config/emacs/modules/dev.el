;;; dev.el -*- lexical-binding: t; -*-

(straight-use-package 'magit)
(straight-use-package 'magit-gitflow)
(straight-use-package 'eglot)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'eldoc-box)
(straight-use-package 'smartparens)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'flycheck)

(require 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

(define-key magit-mode-map (kbd "s") 'magit-status)

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(require 'eglot)

(setq eglot-sync-connect 1
      eglot-connect-timeout 10
      eglot-autoshutdown t
      eglot-send-changes-idle-time 0.5
      eglot-auto-display-help-buffer nil)

(setq eglot-stay-out-of '(eshell flymake))

(define-key eglot-mode-map (kbd "C-l r") 'eglot-rename)
(define-key eglot-mode-map (kbd "M-RET") 'eglot-code-actions)
(define-key eglot-mode-map (kbd "C-l f t") 'eglot-find-typeDefinition)
(define-key eglot-mode-map (kbd "C-l f d") 'eglot-find-declaration)
(define-key eglot-mode-map (kbd "C-l f m") 'eglot-find-implementation)
(define-key eglot-mode-map (kbd "C-l b") 'eglot-format-buffer)

(add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
(setq eglot-confirm-server-initiated-edits nil)

(add-to-list 'eglot-server-programs '(latex-mode . ("texlab")))
(add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))

(add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'scala-mode-hook 'eglot-ensure)
(add-hook 'rustic-mode-hook 'eglot-ensure)
(add-hook 'LaTeX-mode-hook 'eglot-ensure)
(add-hook 'go-mode 'eglot-ensure)

(add-hook 'python-mode-hook (lambda () (tree-sitter-hl-mode 1)))
(add-hook 'go-mode-hook (lambda () (tree-sitter-hl-mode 1)))
(add-hook 'c++-mode-hook (lambda () (tree-sitter-hl-mode 1)))
(add-hook 'c-mode-hook (lambda () (tree-sitter-hl-mode 1)))

(defun k-conf/eglot-scala-fmt ()
  "Use `scala-format-buffer' in scala mode rather than relying on LSP"
  (define-key eglot-mode-map (kbd "C-l s") 'scala-format-buffer))

(add-hook 'scala-mode-hook 'k-conf/eglot-scala-fmt)

(require 'eldoc)
(add-hook 'eglot-connect-hook 'eldoc-mode)

(add-hook 'eldoc-mode-hook 'eldoc-box-hover-mode)

(require 'smartparens-config)
(smartparens-global-mode 1)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-file nil)
(exec-path-from-shell-initialize)

(require 'flycheck)
(add-hook 'go-mode-hook 'flycheck-mode)

(provide 'dev)
