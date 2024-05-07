;;; completion.el --- Completions for emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; This packages sets up various completion providers
;;; Code:

;; Learn from your history, so you are bound to repeat it?
(use-package savehist
	:straight t
	:config (savehist-mode))

;; Those quick annotations are really helpful
(use-package marginalia
	:straight t
	:custom
	(marginalia-annotators
	 '(marginalia-annotators-heavy marginalia-annotators-light nil))
	(marginalia-align 'right)
	:config
	(marginalia-mode))

(use-package vertico
	:straight (vertico :files (:defaults "extensions/*.el")
										 :includes (vertico-directory))
	:bind
	(:map vertico-map
				("TAB" . #'vertico-insert)
				("RET" . #'vertico-exit))
	:custom
	(vertico-resize t)
	(vertico-cycle t)
	(vertico-preselect 'first)
	:config (vertico-mode))

(use-package minibuffer
	:straight (:type built-in)
	:bind ( :map minibuffer-local-completion-map
					("<up>" . minibuffer-previous-line-completion)
					("<down>" . minibuffer-next-line-completion))
	:init
	(setq completions-format 'one-column)
	(setq completion-show-help nil)
	(setq completion-auto-help 'always)
	(setq completion-auto-select nil)
	(setq completions-detailed t)
	(setq completion-show-inline-help nil)
	(setq completions-max-height 6)
	(setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
	(setq completions-highlight-face 'completions-highlight)
	(setq minibuffer-completion-auto-choose t)
	;; (setq minibuffer-visible-completions t) ; Emacs 30
	(setq completions-sort nil))

;; A few more useful configurations
(require 'crm)

(defun crm-indicator (args)
	"Add prompt indicator to `completing-read-multiple' with ARGS.
We display [CRM<separator>], e.g., [CRM,] if the separator is a
comma."
	(cons
	 (format "[CRM%s] %s"
					 (replace-regexp-in-string
						"\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" "" crm-separator)
					 (car args))
	 (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq minibuffer-prompt-properties
			'(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(setq enable-recursive-minibuffers t)

;; Tempel for expansion - May remove this in the future as I don't use it alot.
(use-package tempel
	:straight t
	:bind (("M-+" . #'tempel-complete)
				 ("M-*" . #'tempel-insert)
				 :map tempel-map
				 ("TAB" . #'tempel-next))
	:config
	(defun conf/tempel-setup-capf ()
		"Setup tempel capf function."
		(setq-local completion-at-point-functions
								(cons #'tempel-expand completion-at-point-functions)))
	
	(add-hook 'org-mode-hook #'conf/tempel-setup-capf)
	(add-hook 'prog-mode-hook #'conf/tempel-setup-capf)

	(setq tempel-path (locate-user-emacs-file "templates/*.eld")))

(use-package cape
	:straight t)

(use-package fzf-native
	:straight '(fzf-native :repo "dangduc/fzf-native"
												 :host github
												 :files (:defaults "bin"))
	:config
	(fzf-native-load-dyn))

(use-package fussy
	:straight t
	:init
	(setq completion-styles '(fussy)
				completion-ignore-case t
				completion-category-defaults nil
				completion-category-overrides nil
				fussy-score-fn 'fussy-fzf-native-score
				fussy-filter-fn 'fussy-filter-default
				fussy-use-cache t
				fussy-compare-same-score-fn 'fussy-histlen->strlen<))

(use-package company-mode
	:straight t
	:demand t
	:hook ((prog-mode . company-mode)
				 (heex-ts-mode . company-mode))
	:custom
	(company-require-match nil)
	(company-eclim-auto-save nil)
	(company-dabbrev-down-case nil)
	(company-minimum-prefix-length 1)
	(company-tooltip-align-annotations t)
	(company-global-modes '(not shell-mode eat-mode eshell-mode))
	(company-idle-delay 0.1)
	(company-show-quick-access t)
	(company-format-margin-function #'company-text-icons-margin)
	(company-frontends '(company-pseudo-tooltip-frontend))
	(company-backends '( company-capf company-semantic
											 company-keywords
											 company-etags company-gtags ))
	:config
	(defun conf/company-capf (f &rest args)
		"Manage `completion-styles'."
		(if (length= company-prefix 0)
				;; Don't use `company' for 0 length prefixes.
				(let ((completion-styles (remq 'fussy completion-styles)))
					(apply f args))
			(let ((fussy-max-candidate-limit 5000)
						(fussy-default-regex-fn 'fussy-pattern-first-letter)
						(fussy-prefer-prefix nil))
				(apply f args))))

	(defun conf/company-transformers (f &rest args)
		"Manage `company-transformers'."
		(if (length= company-prefix 0)
				;; Don't use `company' for 0 length prefixes.
				(apply f args)
			(let ((company-transformers '(fussy-company-sort-by-completion-score)))
				(apply f args))))

	(advice-add 'company-auto-begin :before 'fussy-wipe-cache)
	(advice-add 'company--transform-candidates :around 'conf/company-transformers)
	(advice-add 'company-capf :around 'conf/company-capf))

(use-package consult-flycheck
	:straight t
	:bind ("M-g f" . consult-flycheck))

(use-package consult-lsp
	:straight t
	:config
	(define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
	(define-key lsp-mode-map [remap lsp-treemacs-errors-lisp] #'consult-lsp-diagnostics))

(use-package consult
	:straight t
	:demand t
	:bind (("M-g i" . consult-imenu)
				 ("C-x b" . consult-buffer)
				 ("C-x 4 b" . consult-buffer-other-window)
				 ("C-x 5 b" . consult-buffer-other-frame)
				 ("C-x p b" . consult-project-buffer)
				 ("C-x r b" . consult-bookmark)
				 ("M-y" . consult-yank-pop))
	:hook (completion-list-mode . consult-preview-at-point-mode)
	:init
	(setq
	 xref-show-xrefs-function #'consult-xref
	 xref-show-definitions-function #'consult-xref
	 consult--tofu-char #x100000
	 consult--tofu-range #xFFFE)

	:config
	(consult-customize
	 consult-buffer
	 consult-buffer-other-window
	 consult-buffer-other-frame
	 consult-project-buffer
	 :preview-key "M-."))

(provide 'completions)
;;; completions.el ends here
