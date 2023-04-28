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
(global-hl-line-mode 1)
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
(defvar conf/source-sans
  '(:family "Source Sans 3" :height 160)
  "The Source Sans 3 font family.")

(defvar conf/source-serif
  '(:family "Source Serif 4" :height 150)
  "The Source Serif 4 font family.")

(defvar conf/lisa-mono
  '(:family "MonoLisa" :height 130)
  "The MonoLisa font family.")

;; Setup fonts (not using Fontaine anymore)
(custom-set-faces
 `(fixed-pitch ((t ,conf/lisa-mono)))
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

(load-theme 'catppuccin t)

(use-package ef-themes
  :straight t
  :config
  (setq
   ef-themes-mixed-fonts nil
   ef-themes-region '(intense extend)
   ef-themes-headings
   '((0 . (variable-pitch 1.5))
     (1 . (variable-pitch 1.4))
     (2 . (variable-pitch 1.3))
     (3 . (variable-pitch 1.2))
     (4 . (variable-pitch 1.1))
     (5 . (variable-pitch 1.1))
     (6 . (variable-pitch 1.1))
     (7 . (variable-pitch 1.1))
     (t . (variable-pitch 1.1)))))

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
