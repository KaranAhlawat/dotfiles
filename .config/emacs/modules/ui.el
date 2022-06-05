;;; ui.el -*- lexical-binding: t; -*-
   
(global-hl-line-mode -1)
(global-display-line-numbers-mode)

(add-hook 'org-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'eshell-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'nov-mode-hook (lambda () (display-line-numbers-mode -1)))
(add-hook 'pdf-view-mode (lambda () (display-line-numbers-mode -1)))

(defun k/setup-font-faces ()
  "Setup faces for emacs"
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font (font-spec :family "Iosevka SS07 Extended" :size 19 :weight 'regular))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka SS07 Extended" :size 19 :weight 'regular))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Aile" :size 20 :weight 'regular))))

(add-hook 'after-init-hook 'k/setup-font-faces)
(add-hook 'server-after-make-frame-hook 'k/setup-font-faces)

(setq visual-bell t)
(blink-cursor-mode 1)
(pixel-scroll-precision-mode 1)

(straight-use-package 'all-the-icons)
(straight-use-package 'vscode-icon)
(straight-use-package 'all-the-icons-dired)
(straight-use-package 'doom-modeline)
(straight-use-package 'which-key)
(straight-use-package 'page-break-lines)
(straight-use-package 'dashboard)
(straight-use-package '(ligature :type git :host github :repo "mickeynp/ligature.el"))
(straight-use-package 'dired-sidebar)
(straight-use-package 'atom-one-dark-theme)

(setq modus-themes-slanted-constructs t
      modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-scale-headings t
      modus-themes-subtle-line-numbers t
      modus-themes-mode-line '(borderless)
      modus-themes-syntax '(faint)
      modus-themes-lang-checkers '(faint)
      modus-themes-completions '(opinionated)
      modus-themes-operandi-color-overrides
      '((bg-main . "#FAFAFA")
        (fg-main . "#101010"))
      modus-themes-vivendi-color-overrides
      '((bg-main . "#101010")
        (fg-main . "#FAFAFA"))
      modus-themes-org-blocks 'gray-background)

(load-theme 'modus-vivendi t)

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
