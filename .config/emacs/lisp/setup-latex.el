;;; setup-latex.el --- For latex editing is better	-*- lexical-binding: t; -*-
;;; Commentary:
;;; A place to configure everything relating to LaTeX
;;; Code:

;; AUCTEX
(use-package auctex
	:straight t
	:defer t
	:mode ("\\.tex\\'" . TeX-latex-mode)
	:hook
	((LaTeX-mode . prettify-symbols-mode)
		(LaTeX-mode . turn-on-auto-fill)
		(LaTeX-mode . eglot-ensure))
	:init
	(setq
		TeX-parse-self t
		TeX-auto-save t
		TeX-auto-untabify t
		TeX-auto-local ".auctex-auto"
		TeX-style-local ".auctex-style"
		TeX-source-correlate-mode t
		TeX-source-correlate-method 'synctex
		TeX-source-correlate-start-server t
		TeX-electric-sub-and-superscript t
		TeX-save-query nil
		TeX-view-program-selection '((output-pdf "Evince"))
		TeX-region ".auctex-region")
	(setq-default
		TeX-output-dir "build"
		TeX-master nil))

(use-package cdlatex
	:straight t
	:after tex-site
	:hook (LaTeX-mode . turn-on-cdlatex)
	:bind (:map cdlatex-mode-map
			  ("<tab>" . cdlatex-tab))
	:init
	(setq cdlatex-takeover-dollar nil)
	(setq cdlatex-takeover-parenthesis nil))

;; Faster-er math lol
(use-package laas
	:straight t
	:hook LaTeX-mode
	:config
	(aas-set-snippets
		'laas-mode
		"dm" (lambda () (interactive)
				 (yas-expand-snippet "\\[\n	$0 \n\\]"))))


(provide 'setup-latex)
;;; setup-latex.el ends here
