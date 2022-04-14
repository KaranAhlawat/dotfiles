;;; ui.el -*- lexical-binding: t; -*-
   
(global-hl-line-mode -1)
(global-display-line-numbers-mode)

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'nov-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'pdf-view-mode (lambda () (display-line-numbers-mode -1)))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-face-attribute 'default nil
		                                  :font "BlexMono Nerd Font Mono"
		                                  :height 130
		                                  :weight 'normal))))
  (set-face-attribute 'default nil
                      :font "BlexMono Nerd Font Mono"
                      :height 130
                      :weight 'normal))

(setq visual-bell t)
(blink-cursor-mode 1)

(straight-use-package 'all-the-icons)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'doom-modeline)
(straight-use-package 'vscode-dark-plus-theme)
(straight-use-package 'modus-themes)
(straight-use-package 'which-key)
(straight-use-package 'page-break-lines)
(straight-use-package 'dashboard)
(straight-use-package 'doom-themes)

(require 'modus-themes)
(modus-themes-load-themes)

(setq modus-themes-bold-constructs t
      modus-themes-italic-constructs t
      modus-themes-syntax '(green-stings)
      modus-themes-links '(neutral-underline bold)
      modus-themes-prompts '(intense)
      modus-themes-mode-line '(3d accented)
      modus-themes-completions '((selection . (accented))
                                 (popup     . (accented bold)))
      modus-themes-fringes 'subtle
      modus-themes-lang-checkers '(straight-underline text-also faint)
      modus-themes-subtle-line-numbers t
      modus-themes-paren-match '(bold)
      modus-themes-region '(no-extend bg-only))

;; (modus-themes-load-vivendi)
(setq doom-nord-brighter-comments nil
      doom-nord-brighter-modeline t)
(load-theme 'doom-nord t)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-banner-logo-title "")
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents . 5)
			                  (projects . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-init-info t)

(with-eval-after-load 'dashboard
  (page-break-lines-mode))

(which-key-mode)
(setq which-key-idle-delay 0.5)

(add-hook 'after-init-hook 'doom-modeline-init)

(setq doom-modeline-minor-modes nil
      doom-modeline-buffer-file-name-style 'truncate-except-project)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(require 'whitespace)
(setq whitespace-style '(face spaces newline tabs space-mark tab-mark newline-mark))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [8629 10])
        (tab-mark 9 [8594 9] [92 9])))
(global-whitespace-mode)

(provide 'ui)
