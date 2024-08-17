;;; setup-org.el --- I write, I forget  -*- lexical-binding: t; -*-
;;; Commentary:
;;; A place to configure everything relating to Org
;;; Code:

(defun conf/org-font-setup ()
  "Setup fixed-pitch font for Org."
  (custom-set-faces
   '(org-meta-line ((t :inherit (font-lock-comment-face fixed-pitch) :height 0.9)))
   '(org-document-info-keyword ((t :inherit org-meta-line :height 0.9)))
   '(org-block ((t (:inherit fixed-pitch :height 0.9))))
   '(org-table ((t (:inherit fixed-pitch :height 0.9))))
   '(org-formula ((t (:inherit fixed-pitch :height 0.9))))
   '(org-code ((t (:inherit fixed-pitch :height 0.9))))
   '(org-verbatim ((t (:inherit fixed-pitch :height 0.9))))
   '(org-special-keyword ((t (:inherit fixed-pitch :height 0.9))))
   '(org-checkbox ((t (:inherit fixed-pitch :height 0.9))))
   '(line-number ((t (:inherit fixed-pitch :height 0.9))))
   '(line-number-current-line ((t (:inherit fixed-pitch :height 0.9))))
   '(org-block-begin-line ((t (:inherit fixed-pitch :height 0.9))))
   '(org-block-end-line ((t (:inherit org-block-begin-line :height 0.9))))))

;; ORG CONFIGURATION
(use-package org
  :straight t
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . turn-on-org-cdlatex)
   (org-mode . org-toggle-pretty-entities)
   (org-mode . variable-pitch-mode)
   (org-mode . conf/org-font-setup)
   (org-mode . (lambda ()
                 (hl-line-mode -1))))
  :init
  (setq
   org-startup-folded 'fold
   org-startup-indented t
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
   org-agenda-current-time-string " now ─────────────────────────────────────────────────"
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
   org-edit-src-content-indentation 0
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts nil)
  :config
  ;; Org babel
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sql . t)))))

(use-package org-modern
  :straight t
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :custom
  (org-modern-tag nil)
  (org-modern-block-name t))

(provide 'setup-org)
;;; setup-org.el ends here
