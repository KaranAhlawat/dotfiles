(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(define-key magit-mode-map (kbd "s") 'magit-status)

(use-package project-x
  :load-path "~/.config/emacs/project-x/"
  :after project
  :config
  (setq project-x-local-identifier '(".project" "mix.exs" ".gitignore" "Cargo.toml")))

(use-package flycheck
  :defer t
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d"))

(use-package sly
    :defer t
    :config
    (setq inferior-lisp-program "sbcl"))

(use-package parinfer-rust-mode
  :hook
  (emacs-lisp-mode . parinfer-rust-mode)
  (lisp-mode . parinfer-rust-mode)
  (common-lisp-mode . parinfer-rust-mode)
  :init
  (setq parinfer-rust-auto-download t))

(use-package elixir-mode
  :defer t
  :config
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package alchemist
  :defer t
  :hook (elixir-mode . alchemist-mode))

(use-package js2-mode
  :mode ("\\.js\\'")
  :defer t
  :config
  (setq js2-basic-offset 2))

(use-package eglot
  :ensure t
  :defer t
  :hook
  ((elixir-mode js2-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode . ("/usr/lib/elixir-ls/language_server.sh")))
  (add-to-list 'eglot-ignored-server-capabilities :hoverProvider))

(use-package highlight-numbers
  :defer t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(defface tree-sitter-hl-face:warning
  '((default :inherit font-lock-warning-face))
  "Face for parser errors"
  :group 'tree-sitter-hl-faces)

(defun korv/tree-sitter-common ()
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))
  (setq tree-sitter-hl-use-font-lock-keywords nil)
  (tree-sitter-mode +1)
  (tree-sitter-hl-mode +1))

(defun korv/elixir-tree-sitter ()
  (setq
   tree-sitter-hl-default-patterns
   (read
    (concat
     "["
     (s-replace "#match?" ".match?"
                (f-read-text (expand-file-name "~/Secondary/dev/tree-sitter/elixir/highlights.scm")))
     "]")))

  (korv/tree-sitter-common))

(defun korv/js-tree-sitter ()
  (korv/tree-sitter-common))

(use-package tree-sitter
  :ensure t
  :hook ((elixir-mode . korv/elixir-tree-sitter))
  :hook ((js2-mode . korv/js-tree-sitter))
  :custom-face
  (tree-sitter-hl-face:operator ((t)))
  (tree-sitter-hl-face:variable ((t)))
  (tree-sitter-hl-face:function.method.call ((t)))
  (tree-sitter-hl-face:property ((t)))

  :config
  (setq tree-sitter-debug-highlight-jump-region t)
  (setq tree-sitter-debug-jump-buttons t))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package eldoc
  :hook
  (eglot-connect . eldoc-mode))

(use-package eldoc-box
  :commands (eldoc-box-hover-at-point-mode)
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode))
