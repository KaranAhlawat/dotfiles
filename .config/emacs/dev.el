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

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))
