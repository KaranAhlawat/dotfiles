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
                                      :font "CaskaydiaCove Nerd Font"
                                      :height 140
                                      :weight 'regular))
                (with-selected-frame frame
                  (set-face-attribute 'fixed-pitch nil
                                      :font "CaskaydiaCove Nerd Font"
                                      :height 140
                                      :weight 'regular))))
  (set-face-attribute 'default nil
                      :font "CaskaydiaCove Nerd Font"
                      :height 140
                      :weight 'regular)
  (set-face-attribute 'fixed-pitch nil
                      :font "CaskaydiaCove Nerd Font"
                      :height 140
                      :weight 'regular))

(setq visual-bell t)
(blink-cursor-mode 1)
(pixel-scroll-precision-mode 1)

(straight-use-package 'all-the-icons)
(straight-use-package 'vscode-icon)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'doom-modeline)
(straight-use-package 'vscode-dark-plus-theme)
(straight-use-package 'modus-themes)
(straight-use-package 'which-key)
(straight-use-package 'page-break-lines)
(straight-use-package 'dashboard)
(straight-use-package 'doom-themes)
(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))
(straight-use-package 'dired-sidebar)

(require 'modus-themes)
(modus-themes-load-themes)

(setq modus-themes-bold-constructs nil
      modus-themes-italic-constructs nil
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


(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
;; (modus-themes-load-vivendi)

(require 'vscode-dark-plus-theme)
(setq vscode-dark-plus-scale-org-faces t
      vscode-dark-plus-invert-hl-todo nil
      vscode-dark-plus-box-org-todo nil)
;; (load-theme 'vscode-dark-plus t)

(with-eval-after-load 'doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(load-theme 'doom-tomorrow-night t)

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
(setq dashboard-startup-banner (concat user-emacs-directory "fish.png"))
(setq dashboard-center-content t)
(setq dashboard-show-shortcuts nil)
(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents . 5)
                        (projects . 5)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-init-info t)
(setq dashboard-set-navigator t
      dashboard-path-style 'truncate-beginning
      dashboard-path-max-length 50)

(with-eval-after-load 'dashboard
  (page-break-lines-mode))

(which-key-mode)
(setq which-key-idle-delay 0.5)

;; Doom modeline setup
(add-hook 'after-init-hook 'doom-modeline-init)

(setq doom-modeline-minor-modes nil
      doom-modeline-buffer-file-name-style 'truncate-except-project)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(require 'whitespace)
(setq whitespace-style '(face spaces tabs space-mark tab-mark))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (tab-mark 9 [8594 9] [92 9])))

;; Ligature setup
(require 'ligature)
(ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                     "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                     "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                     "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                     "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                     "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                     ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                     "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                     "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                     "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                     "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
(global-ligature-mode)

;; Dired sidebar setup
(require 'dired-sidebar)
(require 'vscode-icon)
(global-set-key (kbd "C-c C-t") 'dired-sidebar-toggle-sidebar)
(setq dired-sidebar-subtree-line-prefix "  "
      dired-sidebar-theme 'all-the-icons
      dired-sidebar-use-term-integration t
      dired-sidebar-use-one-instance t
      dired-sidebar-use-magit-integration t)

(set-face-italic 'italic nil)
(provide 'ui)
