;;; setup-latex.el --- For latex editing is better  -*- lexical-binding: t; -*-
;;; Commentary:
;;; A place to configure everything relating to LaTeX
;;; Code:

;; AUCTEX
(use-package cdlatex
  :straight t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :hook
  ((LaTeX-mode . prettify-symbols-mode)
   (LaTeX-mode . turn-on-auto-fill)
   (LaTeX-mode . (lambda () (setq corfu-mode -1))))
  :config
  (setq
   TeX-parse-self t
   TeX-auto-save t
   TeX-auto-untabify t
   TeX-engine 'xetex
   TeX-auto-local ".auctex-auto"
   TeX-style-local ".auctex-style"
   TeX-source-correlate-mode t
   TeX-source-correlate-method 'synctex
   TeX-source-correlate-start-server t
   TeX-electric-sub-and-superscript t
   TeX-save-query nil
   TeX-view-program-selection '((output-pdf "Evince"))
   TeX-region ".auctex-region")

  ;; Scale up previews
  (setq conf/org-latex-scale 1.75)
  (setq org-format-latex-options
        (plist-put
         org-format-latex-options
         :scale conf/org-latex-scale)))

;; Faster org-mode previews
(use-package org-auctex
  :straight (:type git :host github :repo "karthink/org-auctex")
  :after org
  :hook org-mode)

;; Faster-er math lol
(use-package laas
  :straight t
  :hook LaTeX-mode)

(provide 'setup-latex)
;;; setup-latex.el ends here
