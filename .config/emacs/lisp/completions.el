;;; completion.el --- Completions for emacs -*- lexical-binding: t; -*-
;;; Commentary:
;;; This packages sets up various completion providers
;;; Code:

(use-package savehist
  :straight (:type built-in)
  :config
  (savehist-mode))

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
  (vertico-preselect 'directory)
  :config (vertico-mode))

(use-package vertico-posframe
  :straight t
  :after vertico
  :config
  (vertico-posframe-mode))

(use-package minibuffer
  :straight (:type built-in)
  :bind ( :map minibuffer-local-completion-map
          ("<up>" . minibuffer-previous-line-completion)
          ("<down>" . minibuffer-next-line-completion))
  :init
  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq completion-auto-help 'always)
  (setq completion-auto-select 'second-tab)
  (setq completions-detailed t)
  (setq completion-show-inline-help nil)
  (setq completions-max-height 6)
  (setq completions-header-format (propertize "%s candidates:\n" 'face 'bold-italic))
  (setq completions-highlight-face 'completions-highlight)
  (setq minibuffer-completion-auto-choose t)
  ;; (setq minibuffer-visible-completions t) ; Emacs 30
  (setq completions-sort nil)
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq enable-recursive-minibuffers t)
  :config
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

;; A few more useful configurations
(use-package crm
  :straight (:type built-in)
  :config
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
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;; Tempel for expansion - May remove this in the future as I don't use it alot.
(use-package tempel
  :straight t
  :bind (("M-+" . #'tempel-complete)
         ("M-*" . #'tempel-insert)
         :map tempel-map
         ("TAB" . #'tempel-next))
  :config
  (setq tempel-path (locate-user-emacs-file "templates/*.eld")))

(use-package orderless
  :after vertico
  :straight t
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides nil
        orderless-matching-styles '(orderless-literal orderless-initialism orderless-flex)))

(use-package cape
  :straight t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu
  :straight t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 0)
  (corfu-auto-delay 0)
  (corfu-preview-current 'insert)
  (corfu-preselct 'prompt)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match t)
  :bind (:map corfu-map
              ("s-SPC" . corfu-insert-separator)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous)
              ("RET" . corfu-insert))
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-quit-at-boundary t
                          corfu-quit-no-match t
                          corfu-auto nil)
              (corfu-mode)
              nil
              t)))

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
   consult-recent-file
   :preview-key "M-."))

(use-package consult-flycheck
  :straight t
  :after consult
  :bind ("M-g f" . consult-flycheck))

(use-package consult-lsp
  :straight t
  :after consult
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (define-key lsp-mode-map [remap lsp-treemacs-errors-lisp] #'consult-lsp-diagnostics))


(provide 'completions)
;;; completions.el ends here
