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
  :bind ("C-c f" . #'fontaine-set-preset)
  :init
  (setq x-underline-at-descent-line t)
  (setq-default text-scale-remap-header-line t)
  (setq fontaine-latest-state-file (locate-user-emacs-file
                                    "fontaine-latest-state.eld"))
  (setq fontaine-presets
        '((small
           :default-height 80)
          (regular)
          (thicker
           :inherit regular
           :default-weight medium)
          (large
           :inherit medium
           :default-height 190)
          (presentation
           :inherit medium
           :default-height 190)
          (t
           :default-family "Operator Mono"
           :fixed-pitch-family "Operator Mono"
           :fixed-pitch-serif-family "Operator Mono"
           :fixed-pitch-serif-weight medium
           :default-weight normal
           :default-height 130
           :variable-pitch-family "Lato"
           :variable-pitch-height 130
           :variable-pitch-weight normal)))
  :config
  (fontaine-set-preset 'regular)

  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(use-package modus-themes
  :straight nil
  :demand
  :init
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-italic-constructs t)
  (setq modus-themes-bold-constructs t)
  :config
  (load-theme 'modus-vivendi t))

(use-package autothemer
  :straight t)

(load-theme 'oxocarbon t)

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
