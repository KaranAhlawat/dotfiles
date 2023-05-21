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
  :init (vertico-mode)
  :bind
  (:map vertico-map
        ("TAB" . #'vertico-insert)
        ("RET" . #'vertico-exit))
  :custom
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-preselect 'first))

;; (use-package vertico-directory
;;   :after vertico
;;   :ensure nil
;;   :bind
;;   (:map vertico-map
;;         ("RET" . vertico-directory-enter)
;;         ("DEL" . vertico-directory-delete-char)
;;         ("M-DEL" . vertico-directory-delete-word))
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))


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
  :custom
  (orderless-matching-styles
   '(orderless-regexp
     orderless-literal orderless-initialism orderless-prefixes))
  (orderless-component-separator " +\\|[-/]")
  :init
  (setq
   completion-styles '(orderless)
   completion-ignore-case t
   completion-category-defaults nil
   completion-category-overrides '((file (orderless styles partial-completion))))
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
;; Corfu for completion at point popup
(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.0)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)
  (corfu-on-exact-match nil)

  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . corfu-insert))

  :init (global-corfu-mode))

(use-package consult
  :straight t
  :demand t
  :bind (("M-g f" . consult-flymake)
         ("M-g i" . consult-imenu)
         ("C-x b" . consult-buffer)
         ("C-x 4 b" . consult-buffer-other-window)
         ("C-x 5 b" . consult-buffer-other-frame)
         ("C-x p b" . consult-project-buffer)
         ("C-x p g" . consult-ripgrep)
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

(use-package kind-icon
  :straight t
  :after corfu
  :custom
  (kind-icon-default-face nil)
  (kind-icon-extra-space t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(setq
 completions-max-height 10
 completions-format 'vertical)

(provide 'completions)
;;; completions.el ends here
