;;; text.el -*- lexical-binding: t; -*-

(straight-use-package 'org-bullets)
(straight-use-package 'visual-fill-column)
(straight-use-package '(org :type built-in))
(straight-use-package 'org-fragtog)
(straight-use-package 'pdf-tools)
(straight-use-package 'nov)

(defun k/setup-org-face ()
  (set-face-attribute 'org-document-title nil :font "Iosevka Etoile" :weight 'bold :height 1.3)

  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Iosevka Etoile" :weight 'medium :height (cdr face)))

  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

  (set-face-attribute 'org-column nil :background nil)
  (set-face-attribute 'org-column-title nil :background nil))

(require 'org)
(add-hook 'org-mode-hook (lambda () (org-indent-mode)))
(setq org-ellipsis " ▾")
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))
(add-hook 'org-mode-hook 'k/setup-org-face)
(add-hook 'org-mode-hook 'variable-pitch-mode)

(require 'org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-format-latex-options '(:scale 1.75))

(setq visual-fill-column-width 100
      visual-fill-column-center-text t)
(add-hook 'org-mode-hook 'visual-fill-column-mode)

(require 'pdf-tools)
(pdf-tools-install)

(require 'nov)
(push '("\\.epub\\'" . nov-mode) auto-mode-alist)
(setq nov-text-width 100)
(add-hook 'nov-mode-hook 'visual-line-mode)
(add-hook 'nov-mode-hook 'visual-fill-column-mode)

;; Agenda setup
(setq org-directory "~/Documents/Org"
      org-agenda-files (list "TODO.org")
      org-agenda-start-with-log-mode t
      org-log-done 'time
      org-log-into-drawer t)

(provide 'text)
