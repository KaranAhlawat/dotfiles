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

(use-package fontaine
  :straight t
  :init
  (setq x-underline-at-descent-line nil)
  (setq-default text-scale-remap-header-line t)
  (setq fontaine-latest-state-file (locate-user-emacs-file
                                    "fontaine-latest-state.eld"))

  (setq fontaine-presets
        '((small
           :default-family "monospace"
           :default-height 80
           :variable-pitch-family "sans-serif")
          (regular
           :default-weight medium)
          (medium
           :default-height 140)
          (large
           :inherit medium
           :default-height 190)
          (presentation
           :inherit medium
           :default-height 190)
          (t
           :default-family "monospace"
           :default-height 135
           :variable-pitch-family "monospace"
           :variable-pitch-height 135)))
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame
                      frame
                    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))))
    (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular)))

  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

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
  (defvar catppuccin-flavor)
  (mapc #'disable-theme custom-enabled-themes)
  (if (conf/is-it-dark-yet?)
      (progn
        (setq catppuccin-flavor 'mocha)
        (load-theme dark :no-confirm))
    (progn
      (setq catppuccin-flavor 'latte)
      (load-theme light :no-confirm))))

(use-package nord-theme
  :straight t
  :init
  (setq nord-uniform-mode-lines t)
  (setq nord-region-highlight "frost")
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame
                      frame
                    (load-theme 'nord t))))
    (load-theme 'nord t)))

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
