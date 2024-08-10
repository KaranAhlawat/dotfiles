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
          (monitor
           :default-height 140
           :fixed-pitch-height 140
           :variable-pitch-height 160)
          (large
           :inherit regular
           :default-height 160)
          (presentation
           :inherit regular
           :default-height 190)
          (t
           :default-family "Fira Code"
           :default-weight medium
           :default-height 120
           :bold-family nil
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil
           :fixed-pitch-family nil
           :fixed-pitch-weight nil
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil
           :variable-pitch-family "Fira Code"
           :variable-pitch-height 120
           :variable-pitch-weight medium)))
  :config
  (fontaine-set-preset 'regular)
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset))

(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-bold nil)
  (doom-oksolar-dark-brighter-comments t)
  (doom-oksolar-light-brighter-comments t)

  :config
  (with-eval-after-load 'org
    (doom-themes-org-config)
    (doom-themes-enable-org-fontification))
  (doom-themes-set-faces nil
    '(tooltip :inherit 'fixed-pitch)
    '(font-lock-comment-face :inherit 'italic))
  (load-theme 'doom-one t))

;; (use-package autothemer
;;   :straight t)
;; (load-theme 'oxocarbon t)

;; Cuz I may have the memory of a fish
(use-package which-key
  :straight t
  :custom
  (which-key-idle-delay 0.5)
  :config
  (which-key-setup-side-window-right)
  (which-key-mode))

;; A more minimal modeline. Maybe someday I'll actually customize the defualt in-built one.
(use-package doom-modeline
  :straight t
  :init
  (setq doom-modeline-icon nil)
  :config
  (doom-modeline-mode))

(provide 'ui)
;;; ui.el ends here
