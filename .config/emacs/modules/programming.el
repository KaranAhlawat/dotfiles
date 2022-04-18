;;; programming.el -*- lexical-binding: t; -*-

(straight-use-package 'parinfer-rust-mode)
(straight-use-package 'python-mode)
(straight-use-package 'sbt-mode)
(straight-use-package 'scala-mode)
(straight-use-package 'rustic)
(straight-use-package 'cider)
(straight-use-package 'auctex)
(straight-use-package 'go-mode)
(straight-use-package 'gotest)
(straight-use-package 'yaml-mode)

(require 'parinfer-rust-mode)
(setq parinfer-rust-auto-download t)
(add-hook 'emacs-lisp-mode-hook 'parinfer-rust-mode)
(add-hook 'common-lisp-mode-hook 'parinfer-rust-mode)
(add-hook 'lisp-mode-hook 'parinfer-rust-mode)
(add-hook 'clojure-mode-hook 'parinfer-rust-mode)
(add-hook 'cider-mode-hook 'parinfer-rust-mode)

(push '("\\.cl\\'" . common-lisp-mode) auto-mode-alist)
(push '("\\.py\\'" . python-mode) auto-mode-alist)
(push '("\\.rs\\'" . rustic-mode) auto-mode-alist)
(push '("\\.go\\'" . go-mode) auto-mode-alist)
(push '("\\.yaml\\'" . yaml-mode) auto-mode-alist)

;; Scala
(push '("scala" . scala-mode) interpreter-mode-alist)

(substitute-key-definition
 'minibuffer-complete-word
 'self-insert-command
 minibuffer-local-completion-map)
(setq sbt:program-options '("-Dsbt.supershell=false"))

;; Rust setup
(require 'rustic)
(setq rustic-analyzer-command '("~/.local/bin/rust-analyzer"))
(setq rustic-lsp-client 'eglot)
(setq rustic-enable-detached-file-support t)

;; Latex mode
(with-eval-after-load "latex"
  (push '(output-pdf "PDF Tools") TeX-view-program-selection))

;; GO setup
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda () (setq-local tab-width 4)))

(provide 'programming)
