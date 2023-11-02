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
           :default-family "monospace"
           :default-height 80
           :variable-pitch-family "Liberation Serif")
          (regular
           :default-weight normal)
          (medium
           :default-height 130)
          (large
           :inherit medium
           :default-height 190)
          (presentation
           :inherit medium
           :default-height 190)
          (t
           :default-family "monospace"
           :default-height 130
           :fixed-pitch-family "monospace"
           :default-height 130
           :variable-pitch-family "Liberation Mono"
           :variable-pitch-height 130)))
  :config
  (conf/daemon-frame-hook!
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
  (mapc #'disable-theme custom-enabled-themes)
  (if (conf/is-it-dark-yet?)
      (load-theme dark :no-confirm)
    (load-theme light :no-confirm)))

(use-package jetbrains-darcula-theme
  :straight t
  :config
  (load-theme 'jetbrains-darcula t))

(use-package adwaita-dark-theme
  :straight t
  :custom
  (adwaita-dark-theme-bold-vertico-current t)
  (adwaita-dark-theme-modeline-padding 5)
  :config
  (eval-after-load 'flymake #'adwaita-dark-theme-flymake-fringe-bmp-enable))

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
