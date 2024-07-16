;;; ui.el --- UI tweaks and customizations -*- lexical-binding: t; -*-
;;; Commentary:
;;; The file houses the UI changes I made to Emacs.
;;; Code:

(require 'cl-seq)
(require 'display-line-numbers)

(mapc #'disable-theme custom-enabled-themes)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; Relative numbers to move around quicker
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Hide the line numbering in certain modes as well
(defun conf/disable-line-numbers-in-mode ()
  "Disable `display-line-numbers-mode' in a major-mode."
  (display-line-numbers-mode -1))

(defmacro conf/daemon-frame-hook! (body)
  "Add BODY to `after-make-frame-functions' properly."
  `(if (daemonp)
       (add-to-list 'after-make-frame-functions
                    (lambda (frame)
                      (with-selected-frame
                          frame
                        ,body)))
     ,body))

(dolist (mode '(org-mode-hook eshell-mode-hook))
  (add-hook mode #'conf/disable-line-numbers-in-mode))

;; Switch off the visible bell, it's distracting to me. As well as the
;; blinking cursor
(setq visible-bell nil)
(blink-cursor-mode -1)

(use-package fontaine
  :straight t
  :init
  (setq x-underline-at-descent-line nil)
  (setq-default text-scale-remap-header-line t)
  (setq fontaine-latest-state-file (locate-user-emacs-file
                                    "fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular
           :default-weight semi-bold
           :default-height 150)
          (large
           :inherit medium
           :default-height 190)
          (presentation
           :inherit medium
           :default-height 190)
          (t
           :default-family "Sudo Var"
           :fixed-pitch-family "Sudo Var"
           :fixed-pitch-height 150
           :bold-weight ultra-bold
           :variable-pitch-family "Sudo UI Var"
           :variable-pitch-height 150
           :variable-pitch-weight semi-bold)))
  :config
  (conf/daemon-frame-hook!
   (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(use-package catppuccin-theme
  :straight t
  :demand
  :init
  (setq catppuccin-flavor 'mocha)
  (setq catppuccin-italic-comments t)
  (setq catppuccin-highlight-matches t))

(use-package modus-themes
  :straight (:type built-in)
  :demand
  :config
  (load-theme 'modus-operandi t))

;; Cuz I may have the memory of a fish
(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

;; A more minimal modeline. Maybe someday I'll actually customize the defualt in-built one.
(use-package mood-line
  :straight t
  :custom
  (mood-line-show-eol-style t)
  (mood-line-show-cursor-point t)
  :config
  (mood-line-mode))

(use-package spacious-padding
  :straight t
  :config
  (spacious-padding-mode))

(provide 'ui)
;;; ui.el ends here
