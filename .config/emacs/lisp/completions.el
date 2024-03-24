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

;; Orderless - Chaos is nice
(use-package orderless
  :straight t
  :commands (orderless-filter)
  :custom
  (orderless-matching-styles
   '(orderless-literal
     orderless-regexp))
  (orderless-component-separator " +\\|[-/]")
  :init
  (setq
   completion-styles '(basic orderless)
   completion-ignore-case t
   completion-category-defaults nil
   completion-category-overrides nil)
  :config
  (let ((map minibuffer-local-completion-map))
    (keymap-set map "SPC" nil)
    (keymap-set map "?" nil)))

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
  (add-hook 'clojure-mode-hook #'conf/tempel-setup-capf)

  (setq tempel-path
        (directory-files (concat user-emacs-directory "templates")
                         t
                         directory-files-no-dot-files-regexp)))

(use-package cape
  :straight t)

(use-package company-mode
  :straight t
  :demand t
  :hook (prog-mode . company-mode)
  :custom
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-global-modes '(not shell-mode eat-mode eshell-mode))
  (company-idle-delay 0.1)
  (company-show-quick-access t)
  (company-format-margin-function #'company-text-icons-margin)
  (company-frontends '(company-pseudo-tooltip-frontend)))

(use-package consult
  :straight t
  :demand t
  :bind (("M-g f" . consult-flymake)
         ("M-g i" . consult-imenu)
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
   xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-buffer
   consult-buffer-other-window
   consult-buffer-other-frame
   consult-project-buffer
   :preview-key "M-."))

(use-package consult-eglot
  :straight t)

(provide 'completions)
;;; completions.el ends here
