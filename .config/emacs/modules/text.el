;;; text.el -*- lexical-binding: t; -*-

(straight-use-package 'org-bullets)
(straight-use-package 'visual-fill-column)
(straight-use-package '(org :type built-in))
(straight-use-package 'org-fragtog)
(straight-use-package 'pdf-tools)
(straight-use-package 'nov)

(require 'org)
(add-hook 'org-mode-hook (lambda () (org-indent-mode)))
(setq org-ellipsis " ▾")
(add-hook 'org-mode-hook 'org-bullets-mode)
(setq org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))

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
