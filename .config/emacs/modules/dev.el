;;; dev.el -*- lexical-binding: t; -*-

(straight-use-package 'magit)
(straight-use-package 'magit-gitflow)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-metals)
(straight-use-package 'lsp-pyright)
(straight-use-package 'consult-lsp)
(straight-use-package 'eldoc-box)
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(straight-use-package 'smartparens)
(straight-use-package 'exec-path-from-shell)

(require 'magit)
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
(define-key magit-mode-map (kbd "s") 'magit-status)

(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

;; LSP Mode setup
(require 'lsp-mode)
(require 'lsp-metals)
(require 'lsp-go)
(require 'lsp-pyright)
(require 'lsp-clojure)
(require 'lsp-volar)

(define-key lsp-mode-map (kbd "C-l") lsp-command-map)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(setq lsp-headerline-breadcrumb-enable nil
      lsp-signature-render-documentation nil
      lsp-go-analyses
      '((fieldalignment . t)
        (nilness        . t)
        (unusedwrite    . t)
        (unusedparams   . t))
      lsp-modeline-code-actions-enable nil
      lsp-keep-workspace-alive nil
      lsp-completion-provider :none
      lsp-enable-snippet nil
      lsp-enable-xref t
      lsp-enable-imenu t
      lsp-diagnostics-provider :flymake)

(add-hook 'scala-mode-hook 'lsp-deferred)
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'sql-mode-hook 'lsp-deferred)
(add-hook 'nim-mode-hook 'lsp-deferred)
(add-hook 'clojure-mode-hook 'lsp-deferred)
(add-hook 'python-mode-hook 'lsp-deferred)
(add-hook 'rjsx-mode-hook 'lsp-deferred)
(add-hook 'css-mode-hook  'lsp-deferred)
(add-hook 'web-mode-hook 'lsp-deferred)

(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setf
             (alist-get 'lsp-capf completion-category-defaults)
             '((styles . (orderless flex))))))



(require 'tree-sitter)
(require 'tree-sitter-hl)

(defun k-conf/enable-tree-hl () (tree-sitter-hl-mode 1))

(add-hook 'python-mode-hook #'k-conf/enable-tree-hl)
(add-hook 'go-mode-hook #'k-conf/enable-tree-hl)
(add-hook 'c++-mode-hook #'k-conf/enable-tree-hl)
(add-hook 'c-mode-hook #'k-conf/enable-tree-hl)
(add-hook 'scala-mode-hook #'k-conf/enable-tree-hl)
(add-hook 'rustic-mode-hook #'k-conf/enable-tree-hl)

(require 'eldoc)
(add-hook 'lsp-mode-hook 'eldoc-mode)
(add-hook 'eldoc-mode-hook 'eldoc-box-hover-mode)

(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
(smartparens-global-mode 1)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-check-startup-file nil)
(exec-path-from-shell-initialize)

;; Flymake setup
(require 'flymake)

(add-hook 'prog-mode-hook 'flymake-mode)
(remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
(define-key flymake-mode-map
            (kbd "M-g d")
            #'flymake-show-buffer-diagnostics)
(define-key flymake-mode-map
            (kbd "M-g M-d")
            #'flymake-show-project-diagnostics)
(define-key flymake-mode-map (kbd "M-g f") #'consult-flymake)

(provide 'dev)

;;; dev.el ends here.
