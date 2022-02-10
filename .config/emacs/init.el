;;; init.el --- configure my emacs to my liking

;;; Commentary:
;; This package is not a package but a configuration file

;;; Code:

(defvar korv/default-font-size 145)
(defvar korv/default-variable-font-size 145)
(defvar korv/frame-transparency '(95 . 90))
(defvar korv/font-name "Google Sans Mono")
(defvar korv/org-headings-font "Google Sans Mono")

(setq gc-cons-threshold (* 50 1000 1000))

(setq native-comp-async-report-warnings-errors nil)

(defun korv/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))

;; Add the startup function to the emacs-startup-hook
(add-hook 'emacs-startup-hook #'korv/display-startup-time)

(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

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

(defun korv/set-font-faces ()
  (message "Setting font faces.")
  (set-face-attribute 'default nil :font korv/font-name :height korv/default-font-size :weight 'semi-bold)
  (set-face-attribute 'fixed-pitch nil :font korv/font-name :height korv/default-font-size :weight 'semi-bold)
  (set-face-attribute 'variable-pitch nil :font korv/font-name :height korv/default-variable-font-size :weight 'bold))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (korv/set-font-faces))))
  (korv/set-font-faces))

;; Set <ESC> to escape globally
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Change some global defaults
(column-number-mode)
(global-display-line-numbers-mode t)
(setq tab-always-indent t)
(defalias 'yes-or-no-p 'y-or-n-p)
(show-paren-mode 1)
(setq backup-direcotry-alist '(("." . "~/.cache/emacssaves"))
      create-lockfiles nil
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq-default
 auto-save-list-file-prefix nil
 cursor-in-non-selected-windows nil
 fill-column 80
 help-window-select t
 indent-tabs-mode nil
 select-enable-clipboard t
 x-stretch-cursor t
 tab-width 2)

(blink-cursor-mode 1)
(delete-selection-mode t)
;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

(set-default-coding-systems 'utf-8)

(setq split-width-threshold 0
      split-height-threshold nil)

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
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

(setq make-backup-files nil)

(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves" user-emacs-directory) t)))

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package modus-themes
  :config
  (setq modus-themes-hl-line '(intense)))

(use-package nano-theme)

(use-package doom-themes
  :config
  (load-theme 'doom-dark+ t)
  (setq doom-dark+-blue-modeline 1)
  (setq doom-dark+-padded-modeline 1))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.5))

(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)

  (smartparens-global-mode +1))

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
  (corfu-quit-at-boundary nil)
  (corfu-quit-no-match t)
  (corfu-preselect-first t)
  (corfu-echo-documentation nil)
  :init
  (corfu-global-mode)
  :config
  (setq corfu-auto-prefix 1))


;; TODO: Remap keybinds?

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package dabbrev)

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline--eglot t
        doom-modeline--project-root t))

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

(use-package popper
  :ensure t
  :bind (("C-`" . popper-toggle-latest)
         ("M-`" . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "\\*Backtrace\\*"
          "\\*Warnings\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   '("q f" . delete-frame)
   '("f l" . consult-line)
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . mode-line-other-buffer)))

(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1))

(defun korv/org-font-setup ()
  "Setup fonts for org mode"
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
    (set-face-attribute (car face) nil :font korv/org-headings-font :weight 'regular :height (cdr face)))

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
  :mode "\\.org\\'"
  :config
  (require 'org-tempo)

  ;; Add <el and <sh snippet for quicker code blocks
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (setq org-confirm-babel-evaluate nil
        org-ellipsis " ▾"
        org-agenda-files (list "~/Documents/org/Agenda.org")
        org-latex-pdf-process '("tectonic -X compile --outdir=%o %f"))

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

;; Toggle latex fragments in Org
(use-package org-fragtog
  :config
  (add-hook 'org-mode-hook 'org-fragtog-mode))

;; Center org buffer
(defun korv/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
         	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . korv/org-mode-visual-fill))

(defun korv/org-babel-tangle-config ()
  (when (member (buffer-file-name)
                (list
                 (expand-file-name "~/.dotfiles/.config/emacs/Emacs.org")
                 (expand-file-name "~/.dotfiles/.config/emacs/Development.org")))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'korv/org-babel-tangle-config)))

(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun korv/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun korv/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'kd/map-line-to-status-char status-lines)))))

(defun korv/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))


(defun korv/get-current-eshell-color ()
  (let ((cur-bg-mode (frame-parameter nil 'background-mode)))
    (if (string-equal cur-bg-mode "dark")
        "white"
      "black")))

(defun korv/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch))
        (current-color (korv/get-current-eshell-color)))
    (concat
     "\n"
     (propertize (korv/get-prompt-path) 'face `(:foreground "#2fafff"))
     (when current-branch
       (concat
        (propertize " on " 'face `(:foreground current-color))
        (propertize (concat " " current-branch) 'face `(:foreground "#f48cd4"))))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\n➜" 'face `(:foreground "#f48cd4")))
     (propertize " " 'face `(:foreground current-color)))))

(defun korv/eshell-configure ()

  (require 'magit)

  (use-package xterm-color)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  (eshell-hist-initialize)

  (setq eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-destroy-buffer-when-process-dies t
        eshell-highlight-prompt t
        eshell-prefer-lisp-functions nil
        eshell-scroll-to-bottom-on-output t
        eshell-prompt-function 'korv/eshell-prompt
        eshell-prompt-regexp "^➜ ")

  (setenv "PAGER" "cat"))

(use-package eshell
  :after magit
  :hook
  (eshell-first-time-mode . korv/eshell-configure)
  (eshell-mode . (lambda () (corfu-mode -1)))
  :init
  (setq eshell-directory-name "~/.config/emacs/eshell/"
        eshell-aliases-file (expand-file-name "~/.config/emacs/eshell/aliases")))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (exec-path-from-shell-initialize))

(load (expand-file-name "~/.config/emacs/dev.el"))

(setq gc-cons-threshold (* 2 1000 1000))
