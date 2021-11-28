;;; $doomdir/config.el -*- lexical-binding: t; -*-
(setq user-full-name "Karan Ahlawat"
      user-mail-address "ahlawatkaran12@gmail.com")

;; Font variable
(setq my-font-face "SFMono Nerd Font Mono")

(setq doom-font (font-spec :family my-font-face :size 19 :weight 'regular)
      doom-variable-pitch-font (font-spec :family my-font-face :size 19)
      doom-big-font (font-spec :family my-font-face :size 22)
      doom-unicode-font (font-spec :family "all-the-icons" :size 19))

(setq doom-theme 'doom-laserwave)

(setq doom-leader-key ",")
(setq doom-localleader-key ", m")

(setq org-directory "~/Documents/org/")

(setq display-line-numbers-type 'relative)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(setq projectile-project-search-path '("~/Secondary/dev/" "~/Secondary/dev/Coding/Elixir/"))

;; here are some additional functions/macros that could help you configure doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; to get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'k' (non-evil users must press 'c-c c k').
;; this will open documentation for it, including demos of how they are used.
;;
;; you can also try 'gd' (or 'c-c c d') to jump to their definition and see how
;; they are implemented.

(+global-word-wrap-mode +1)

(setq doom-quit-p 'nil)

;; documentation
;; turn off docs in popup buffer
(setq lsp-ui-doc-enable 'nil
      lsp-ui-sideline-enable 'nil
      lsp-signature-auto-activate 'nil)

(setq lsp-rust-analyzer-server-display-inlay-hints 't)

;; fix clipping of icons in the statusine by reducing scaling
(setq all-the-icons-scale-factor 1.0)

;; setup elcord (discord rpc)
(elcord-mode)

;; set treemacs icons
(setq doom-themes-treemacs-theme "vscode")

;; setup dired-sidebar
(use-package! dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-theme 'icons)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  (setq dired-sidebar-use-custom-modeline t)
  (setq dired-sidebar-use-magit-integration t))

(map! :leader
      :desc "Toggle dired-sidebar"
      "o p" #'dired-sidebar-toggle-sidebar)

;; Doom modeline
(use-package! doom-modeline
  :ensure t
  :custom
  (doom-modeline-major-mode-icon t))

;; Splash image
(setq fancy-splash-image "~/.doom.d/Icons/retro-icon-gnu-meditate-levitate.png")

;; Lang stuff
(dolist (mode '(rjsx-mode-hook
                js2-mode-hook
                typescript-mode-hook))
        (add-hook mode (lambda () (format-all-mode -1))))

(setq exec-path (append exec-path '("/home/karan/.deno/bin/")))

;; Setup org-present
(eval-after-load "org-present"
  '(progn
     (add-hook 'org-present-mode-hook
               (lambda ()
                 (org-present-big)
                 (org-present-hide-cursor)
                 (org-present-read-only)
                 (org-display-inline-images)))
     (add-hook 'org-present-mode-quit-hook
               (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write)))))

(defun kd/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package! visual-fill-column
  :hook (org-mode . kd/org-mode-visual-fill))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq indent-line-function 'insert-tab)

(setq org-latex-pdf-process '("tectonic -Z shell-escape --outdir=%o %f"))

;; Customssss
(setq vterm-always-compile-module t)
(setq vterm-kill-buffer-on-exit t)

;; Rest client setup
(add-to-list 'company-backends 'company-restclient)

;; Emacs browser
(setq browse-url-browser-function 'eww-browse-url)
(map! :leader
      :desc "Search web for text between BEG/END"
      "s w" #'eww-search-words
      (:prefix ("e" . "evaluate/EWW")
       :desc "Eww web browser" "w" #'eww
       :desc "Eww reload page" "R" #'eww-reload))

;; parinfer
(setq parinfer-extensions
      '(default
         pretty-parens
         evil
         smart-tab
         smart-yank))

;; Customize eshell
(defun read-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun kd/map-line-to-status-char (line)
  (cond ((string-match "^?\\? " line) "?")))

(defun kd/get-git-status-prompt ()
  (let ((status-lines (cdr (process-lines "git" "status" "--porcelain" "-b"))))
    (seq-uniq (seq-filter 'identity (mapcar 'kd/map-line-to-status-char status-lines)))))

(defun kd/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
        (string-remove-prefix (file-name-directory git-output) current-path))))

;; actual prompt function
(defun kd/eshell-prompt ()
  (let ((current-branch (magit-get-current-branch)))
    (concat
     "\n"
     (propertize (kd/get-prompt-path) 'face `(:foreground "#2fafff"))
     (when current-branch
       (concat
        (propertize " on " 'face `(:foreground "white"))
        (propertize (concat "" current-branch) 'face `(:foreground "#f48cd4"))))
     (if (= (user-uid) 0)
         (propertize "\n#" 'face `(:foreground "red2"))
       (propertize "\n➜" 'face `(:foreground "#f48cd4")))
     (propertize " " 'face `(:foreground "white")))))


(defun kd/eshell-configure ()

  (require 'magit)

  (require 'evil-collection-eshell)
  (evil-collection-eshell-setup)

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

  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'consult-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)


  (setq eshell-history-size 5000
        eshell-buffer-maximum-lines 5000
        eshell-hist-ignoredups t
        eshell-destroy-buffer-when-process-dies t
        eshell-highlight-prompt t
        eshell-prefer-lisp-functions nil
        eshell-scroll-to-bottom-on-output t
        eshell-prompt-function 'kd/eshell-prompt
        eshell-prompt-regexp "^➜ ")

  (setenv "PAGER" "cat"))

(use-package eshell
  :hook
  (eshell-first-time-mode . kd/eshell-configure)
  (eshell-mode . (lambda () (company-mode -1)))
  :init
  (setq eshell-directory-name "~/.doom.d/eshell/"
        eshell-aliases-file (expand-file-name "~/.doom.d/eshell/aliases")))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; setup stuff for transparency and such
(set-frame-parameter (selected-frame) 'alpha '(100 . 100))
(add-to-list 'default-frame-alist '(alpha . (100 . 100)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(set-company-backend! 'emacs-lisp-mode
  'company-capf 'company-yasnippet)
