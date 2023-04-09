;;; general.el --- General packages and configurations -*- lexical-binding: t; -*-
;;; Commentary:
;;; General stuff that I don't think belongs to other modules but is
;;; too small to create it's own module
;;; Code:

;; Makes it easier to see where what is
(use-package rainbow-delimiters
  :straight t
  :hook
  (lisp-mode emacs-lisp-mode cider-mode cider-repl-mode clojure-mode))

;; Either act on the whole line or the current region
(use-package whole-line-or-region
  :straight t
  :config
  (whole-line-or-region-global-mode))

(use-package smtpmail
  :straight (:type built-in)
  :config
  (setq
   user-full-name "Karan Ahlawat"
   user-mail-address "ahlawatkaran12@gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-stream-type 'starttls
   smtpmail-smtp-serice 587
   send-mail-function 'smtpmail-send-it))

(use-package project
  :straight (:type built-in)
  :custom
  (project-vc-extra-root-markers
   '("package.json"
     "build.sbt"
     "mix.exs"
     ".project"
     "project.clj"
     "composer.json"
     "pubspec.yaml"
     "pyproject.toml"
     "*.asd"))
  :config
  (setopt
   project-switch-commands
   '((project-find-file "Find file")
     (consult-ripgrep "Find regexp")
     (project-find-dir "Find directory")
     (project-dired "Root dired")
     (project-eshell "Eshell")))

  (keymap-global-set "C-x p ." #'project-dired)
  (keymap-global-set "C-x p <return>" #'project-dired)
  (keymap-global-set "C-x p <delete>" #'project-forget-project))

(use-package dired
  :straight (:type built-in)
  :hook ((dired-mode . dired-hide-details-mode) (dired-mode . hl-line-mode))
  :config
  (setq
   dired-recursive-copies 'always
   dired-recursive-deletes 'always
   delete-by-moving-to-trash t
   dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
   dired-dwim-target t
   dired-auto-revert-buffer #'dired-directory-changed-p
   dired-make-directory-clickable t
   dired-free-space nil
   dired-mouse-drag-files t))

(use-package dired-aux
  :straight (:type built-in)
  :config
  (setq
   dired-isearch-filenames 'dwim
   dired-create-destination-dirs 'ask
   dired-vc-rename-file t
   dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))))

(use-package dired-x
  :straight (:type built-in)
  :config
  (setq
   dired-clean-up-buffers-too t
   dired-clean-confirm-killing-deleted-buffers t
   dired-x-hands-off-my-keys t))

(use-package pulsar
  :straight t
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-blue)
  (add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
  (add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)
  (add-hook 'next-error-hook #'pulsar-pulse-line)
  (pulsar-global-mode +1))

(repeat-mode +1)

(use-package ctrlf
  :straight t
  :init
  (ctrlf-mode +1)
  :config
  (setq ctrlf-default-search-style 'fuzzy))

(provide 'general)
;;; general.el ends here
