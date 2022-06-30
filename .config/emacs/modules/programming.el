;;; programming.el -*- lexical-binding: t; -*-

(straight-use-package 'python-mode)
(straight-use-package 'blacken)
(straight-use-package 'poetry)
(straight-use-package 'sbt-mode)
(straight-use-package
 '(scala-mode :type git
              :host github
              :repo "Kazark/emacs-scala-mode"
              :branch "scala3"))
(straight-use-package 'rustic)
(straight-use-package 'cider)
(straight-use-package 'go-mode)
(straight-use-package 'gotest)
(straight-use-package 'yaml-mode)
(straight-use-package 'sqlformat)
(straight-use-package
 '(elfmt :type git :host github :repo "riscy/elfmt"))

(elfmt-global-mode)

(push '("\\.py\\'"   . python-mode) auto-mode-alist)
(push '("\\.rs\\'"   . rustic-mode) auto-mode-alist)
(push '("\\.go\\'"   . go-mode) auto-mode-alist)
(push '("\\.yaml\\'" . yaml-mode) auto-mode-alist)
(push '("\\.clj\\'"  . clojure-mode) auto-mode-alist)

;; Scala
(push '("scala" . scala-mode) interpreter-mode-alist)

(substitute-key-definition 'minibuffer-complete-word 'self-insert-command
                           minibuffer-local-completion-map)
(setq sbt:program-options '("-Dsbt.supershell=false"))

;; Rust setup
(require 'rustic)
(require 'lsp-rust)
(setq rustic-lsp-client 'lsp-mode)
(setq rustic-enable-detached-file-support t)
(add-hook 'rustic-mode-hook #'lsp-rust-analyzer-inlay-hints-mode)

;; Latex mode
(with-eval-after-load "latex"
  (push '(output-pdf "PDF Tools") TeX-view-program-selection))

;; GO setup
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda () (setq-local tab-width 4)))

;; Poetry setup
(setq poetry-tracking-strategy 'switch-buffer)

;; Blacken setup
(add-hook 'python-mode-hook 'blacken-mode)

;; SQL Formatter setup (remember to install pgFormat/pg_format)
(require 'sqlformat)
(setq sqlformat-command 'pgformatter)
(add-hook 'sql-mode-hook 'sqlformat-on-save-mode)
(add-hook 'sql-mode-hook 'sql-highlight-postgres-keywords)
(setq lsp-sqls-workspace-config-path "root")

;; Cider Setup
(require 'cider)
(setq cider-repl-pop-to-buffer-on-connect nil
      cider-repl-display-help-banner nil
      cider-shadow-cljs-command "shadow-cljs")

(provide 'programming)
