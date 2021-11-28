(defvar korv/default-font-size 140)
(defvar korv/default-variable-font-size 140)
(defvar korv/frame-transparency '(100 . 100))
(defvar korv/font-name "LigaSFMono Nerd Font")

(setq gc-cons-threshold (* 50 1000 1000))

(defun korv/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Add the startup function to the emacs-startup-hook
(add-hook 'emacs-startup-hook #'korv/display-startup-time)

(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visual-bell t)

(set-frame-parameter (selected-frame) 'alpha korv/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,korv/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-face-attribute 'default nil :font korv/font-name :height korv/default-font-size)
(set-face-attribute 'fixed-pitch nil :font korv/font-name :height korv/default-font-size)
(set-face-attribute 'variable-pitch nil :font korv/font-name :height korv/default-variable-font-size :weight 'regular)

;; Set <ESC> to escape globally
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Change some global defaults
(column-number-mode)
(global-display-line-numbers-mode t)
(setq tab-always-indent 'complete)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq backup-direcotry-alist '(("." . "~/.cache/emacssaves"))
      create-lockfiles nil
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      tab-width 2)

(setq-default
 auto-save-list-file-prefix nil
 cursor-in-non-selected-windows nil
 fill-column 80
 help-window-select t
 indent-tabs-mode nil
 select-enable-clipboard t
 x-stretch-cursor t)

(blink-cursor-mode 0)
(delete-selection-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-default-coding-systems 'utf-8)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Setup use-package
(require 'use-package)
(setq use-package-always-ensure t)

(use-package no-littering
  :ensure t
  :config
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package modus-themes
  :config
  (setq modus-themes-hl-line '(intense)))

(use-package doom-themes
  :config
  (load-theme 'doom-laserwave t))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package all-the-icons
  :ensure t)

(use-package vertico
  :ensure t
  :bind (:map vertico-map
              ("C-g" . vertico-exit))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-cattegory-overrides '((file (styles partial-completion)))))

(use-package consult
  :init
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key 'any)
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project))))))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preselect-first t)
  (corfu-echo-documentation nil)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :init
  (corfu-global-mode))

;; TODO: Remap keybinds?

(use-package cape
  :bind (("C-c p p" . completion-at-point)
         ("C-c p t" . complete-tag)
         ("C-c p d" . cape-dabbrev)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-C p a" . cape-abbrev))
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package dabbrev)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))

(use-package dashboard
  :custom
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (dashboard-banner-logo "")
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-show-shortcuts t)
  (dashboard-projects-backend 'project-el)
  (dashboard-items '((recents . 5)
                     (projects . 5)))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  :hook
  (dashboard-mode . (lambda () (setq global-hl-line-mode nil)))
  :config
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :after dashboard
  :config
  (page-break-lines-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(global-unset-key (kbd "C-s"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general
  :config
  (general-create-definer korv/leader-keys
    :keymaps '(normal emacs)
    :prefix "C-,"
    :global-prefix ",")

  (korv/leader-keys
   "b" '(:ignore t :which-key "buffers")
   "bs" '(consult-buffer :which-key "Switch buffer")
   "bk" '(kill-current-buffer :which-key "Kill buffer")
   "w" '(:ignore t :which-key "windows")
   "wv" '(evil-window-vsplit :which-key "Vertical split")
   "ws" '(evil-window-split :which-key "Horizontal split")
   "wh" '(evil-window-left :which-key "Focus left")
   "wj" '(evil-window-down :which-key "Focus down")
   "wk" '(evil-window-up :which-key "Focus up")
   "wl" '(evil-window-right :which-key "Focus right")
   "wc" '(delete-window :which-key "Close window")
   "o" '(:ignore t :which-key "open")
   "oe" '(eshell :which-key "Eshell")
   "ot" '(vterm :which-key "Vterm")
   "f" '(:ignore t :which-key "file")
   "ff" '(find-file :which-key "Find File")
   "fr" '(consult-recent-file :which-key "Recent files")
   "fs" '(save-buffer :which-key "Save file")
   "fl" '(consult-line :which-key "Search file")
   "q" '(:ignore t :which-key "session")
   "qq" '(kill-emacs :which-key "Quit Emacs")
   "h" '(:ignore t :which-key "help")
   "hf" '(describe-function :which-key "Describe function")
   "hv" '(describe-variable :which-key "Describe variable")))

(defun korv/org-font-setup ()

  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'regular :height (cdr face)))

  ;; Ensure fixed pitch appears as fixed pitch
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun korv/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . korv/org-mode-setup)
  :config
  (require 'org-tempo)

  ;; Add <el and <sh snippet for quicker code blocks
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (setq org-confirm-babel-evaluate nil
        org-ellipsis " ▾")

  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (korv/org-font-setup)

  ;; Enable emacs-lisp in the code blocks
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t))))

;; Nicer heading bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("" "○" "●" "○" "●" "○" "●")))

;; Center org buffer
(defun korv/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . korv/org-mode-visual-fill))

(defun korv/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.dotfiles/.config/emacs/Emacs.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'korv/org-babel-tangle-config)))

(setq gc-cons-threshold (* 2 1000 1000))
