(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(korv/leader-keys
  "g"  '(:ignore t :which-key "magit")
  "gs" '(magit-status :which-key))

(korv/leader-keys
  "p"  '(:ignore t :which-key "project")
  "pp" '(project-switch-project :which-key "Switch project")
  "pf" '(project-find-file :which-key "Project find file")
  "pd" '(project-dired :which-key "Dired project root")
  "pe" '(project-eshell :which-key "Eshell project root")
  "pa" '(project-forget-project :which-key "Remove project")
  "pk" '(project-kill-buffers :which-key "Kill project buffers")
  "pc" '(project-async-shell-command :which-key "Async cmd project root"))

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
    (setq inferior-lisp-program "sbcl")
    (add-hook 'sly-mode-hook
              (lambda ()
                (unless (sly-connected-p)
                  (save-excursion (sly))))))

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
  (setq js2-basic-offset 2)
  (setq tab-width 2))

(use-package eglot
  :ensure t
  :defer t
  :hook
  ((elixir-mode js2-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode . ("/usr/lib/elixir-ls/language_server.sh")))
  (add-to-list 'eglot-ignored-server-capabilities :hoverProvider))

(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))
