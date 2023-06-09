;;; ui.el --- UI tweaks and customizations -*- lexical-binding: t; -*-
;;; Commentary:
;;; The file houses the UI changes I made to Emacs.
;;; Code:

(require 'cl-seq)
(require 'display-line-numbers)

;; Disable all other themes first
(mapc #'disable-theme custom-enabled-themes)

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")

;; Relative numbers to move around quicker
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Hide the line numbering in certain modes as well
(defun conf/disable-line-numbers-in-mode ()
  "Disable `display-line-numbers-mode' in a major-mode."
  (display-line-numbers-mode -1))

(dolist (mode '(org-mode-hook eshell-mode-hook))
  (add-hook mode #'conf/disable-line-numbers-in-mode))

;; Switch off the visible bell, it's distracting to me. As well as the
;; blinking cursor
(setq visible-bell nil)
(blink-cursor-mode -1)

;; Define font families
(defvar conf/menlo-mono
  '(:family "Menlo" :height 130)
  "The Menlo font family.")

(defvar conf/twilio-mono
  '(:family "Twilio Sans Mono" :height 130)
  "The Twilio sans mono font family.")

(defvar conf/zed-mono
  '(:family "Zed Mono" :height 140 :width expanded :weight medium)
  "The Zed Mono font family.")

(defvar conf/courier-mono
  '(:family "Courier 10 Pitch" :height 140)
  "The Courier Mono font family.")

(defvar conf/source-sans
  '(:family "Source Sans 3" :height 150)
  "The Source Sans font family.")

;; Setup fonts (not using Fontaine anymore)
(custom-set-faces
 `(default ((t ,conf/courier-mono)))
 `(fixed-pitch ((t ,conf/courier-mono)))
 `(variable-pitch ((t ,conf/source-sans))))

;; And now for the themes
(defun conf/is-it-dark-yet? ()
  "Return t if the OS color theme is dark."
  (let
      ((color-scheme
        (shell-command-to-string
         "gsettings get org.gnome.desktop.interface color-scheme")))
    (cl-search "dark" (downcase color-scheme))))

(defun conf/switch-theme-to-os (light dark)
  "Switch the current Emacs theme according to either LIGHT or DARK."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (if (conf/is-it-dark-yet?)
      (load-theme dark :no-confirm)
    (load-theme light :no-confirm)))

(use-package catppuccin-theme
  :straight (catppuccin :local-repo "/home/karan/repos/catppuccin")
  :custom
  (catppuccin-italic-comments t)
  (catppuccin-italic-variables t))

(use-package modus-themes
  :straight t
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-slanted-constructs t)
  :init
  (setq modus-themes-common-palette-overrides
        '((comment fg-dim))))

(conf/switch-theme-to-os 'modus-operandi-tinted 'catppuccin)

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
  (mood-line-show-encoding-information t)
  :config
  (mood-line-mode))

(provide 'ui)
;;; ui.el ends here
