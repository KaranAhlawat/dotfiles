(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(korv/leader-keys
  "p"  '(:ignore t :which-key "project")
  "pp" '(project-switch-project :which-key "Switch project")
  "pf" '(project-find-file :which-key "Project find file")
  "pd" '(project-dired :which-key "Dired project root")
  "pe" '(project-eshell :which-key "Eshell project root")
  "pa" '(project-forget-project :which-key "Remove project")
  "pk" '(project-kill-buffers :which-key "Kill project buffers")
  "pc" '(project-async-shell-command :which-key "Async cmd project root"))

(use-package sly
    :defer t
    :config
    (setq inferior-lisp-program "sbcl"))

(use-package elixir-mode
  :config
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package alchemist)

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(elixir-mode "/usr/lib/elixir-ls/language_server.sh"))
  (add-to-list 'eglot-ignored-server-capabilities :hoverProvider))
