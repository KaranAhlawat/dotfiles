;;; setup-org.el --- I write, I forget  -*- lexical-binding: t; -*-
;;; Commentary:
;;; A place to configure everything relating to Org
;;; Code:

(defun conf/org-font-setup ()
  "Setup fixed-pitch font for Org."
  (custom-set-faces
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-table ((t (:inherit fixed-pitch))))
   '(org-formula ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit fixed-pitch))))
   '(org-verbatim ((t (:inherit fixed-pitch))))
   '(org-special-keyword ((t (:inherit fixed-pitch))))
   '(org-meta-line ((t (:inherit fixed-pitch))))
   '(org-checkbox ((t (:inherit fixed-pitch))))
   '(line-number ((t (:inherit fixed-pitch))))
   '(line-number-current-line ((t (:inherit fixed-pitch))))
   '(org-block-begin-line ((t (:inherit fixed-pitch))))
   '(org-block-end-line ((t (:inherit fixed-pitch :height 0.9 :background "#303035" :foreground "##303035"))))))

;; ORG CONFIGURATION
(use-package org
  :straight (:type built-in)
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . turn-on-org-cdlatex)
   (org-mode . org-indent-mode)
   (org-mode . org-toggle-pretty-entities)
   (org-mode . variable-pitch-mode)
   (org-mode . conf/org-font-setup))
  :config
  (setq
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-agenda-tags-column 0
   org-agenda-block-separator ?-
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ "
     "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string "⭠ now ─────────────────────────────────────────────────"
   org-agenda-files (list (concat (getenv "HOME") "/Documents" "/agenda")))

  ;; Appearance settings
  (setq-default
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-hidden-keywords nil
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-startup-folded nil
   org-ellipsis "…"
   org-startup-with-inline-images nil
   org-highlight-latex-and-related '(native)
   org-indent-mode-turns-on-hiding-stars nil
   org-use-sub-superscripts '{}
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts nil))

(use-package org-appear
  :straight t
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq-default org-hide-emphasis-markers t)
  (setq
   org-appear-autoemphasis t
   org-appear-autosubmarkers t))

(use-package org-modern-indent
  :straight (:host github :repo "jdtsmith/org-modern-indent")
  :after org
  :config (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

(use-package org-modern
  :straight t
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda)
  (org-modern-mode . conf/org-modern-spacing)
  :config
  (defun conf/org-modern-spacing ()
    (setq-local line-spacing
                (if org-modern-mode
                    0.1
                  0.0)))
  (setq org-modern-todo nil))

;; Some minor org tweaks
(use-package visual-fill-column
  :straight t
  :after org
  :hook org-mode
  :custom
  (visual-fill-column-width 100)
  (visual-fill-column-center-text t))

(use-package denote
  :straight t
  :config
  (setq
   denote-allow-multi-word-keywords t
   denote-directory "/home/karan/Documents/denotes"
   denote-file-type 'org
   denote-known-keywords (list "emacs" "college" "major" "sem8")
   denote-modules '(project xref ffap)
   denote-sort-keywords t
   denote-dired-directories `(,denote-directory)
   denote-prompts '(title keywords subdirectory)
   denote-date-prompt-use-org-read-date t)

  (denote-modules-global-mode))

(use-package denote-menu
  :straight t)

(provide 'setup-org)
;;; setup-org.el ends here
