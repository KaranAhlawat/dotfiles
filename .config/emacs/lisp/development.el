;;; development.el --- Making the money -*- lexical-binding: t; -*-
;;; Commentary:
;;; Sets up a great dev exp inside Emacs
;;; Code:

;; Magit, the magical git interface
(use-package magit
  :straight t
  :custom
  (magit-display-buffer-function
   #'magit-display-buffer-same-window-except-diff-v1))

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((kotlin-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (astro-ts-mode . lsp-deferred)
         (scala-ts-mode . lsp-deferred)
         (lsp-completion-mode . conf/lsp-mode-completion-setup))
  :custom
  (lsp-completion-provider :none)
  :init
  (defun conf/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  
  (defun conf/lsp-mode-completion-setup ()
    (if (seq-contains-p '("clojure-mode" "clojurescript-mode" "clojurec-mode" "cider-mode")
                        major-mode)
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
              '(orderless cider))
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(orderless)))
    

    (add-hook 'orderless-style-dispatchers #'conf/orderless-dispatch-flex-first nil 'local)

    (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)

  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (append '("emacs-lsp-booster" "--json-false-value" ":json-false" "--") orig-result))
        orig-result)))

  (setq lsp-keymap-prefix nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-links t)
  (setq lsp-signature-doc-lines 2)

  (require 'lsp-astro)
  :config
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (lsp-enable-which-key-integration t))

(use-package dap-mode
  :straight t
  :after lsp-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package lsp-metals
  :straight t)

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :straight nil
  :hook lsp-mode
  :custom
  (eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0.15))

(use-package eldoc-box
  :straight t
  :after (eglot eldoc)
  :hook (eldoc-mode . eldoc-box-hover-mode)
  :init
  (setq eldoc-box-only-multi-line t)
  (setq eldoc-box-clear-with-C-g t)
  (setq eldoc-box-max-pixel-width 500))

(use-package smartparens
  :straight t
  :demand t
  :hook ((clojure-mode . smartparens-strict-mode)
         (clojurescript-mode . smartparens-strict-mode)
         (clojurec-mode . smartparens-strict-mode)
         (emacs-lisp-mode . smartparens-strict-mode))
  :config
  (sp-with-modes sp-lisp-modes
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "`" "'" :actions '(wrap insert autoskip) :when '(sp-in-string-p)))
  (eval-after-load 'cc-mode                  '(require 'smartparens-c))
  (eval-after-load 'elixir-ts-mode           '(require 'smartparens-elixir))
  (eval-after-load 'erlang-mode              '(require 'smartparens-erlang))
  (eval-after-load 'go-mode                  '(require 'smartparens-go))
  (eval-after-load 'haskell-interactive-mode '(require 'smartparens-haskell))
  (eval-after-load 'haskell-mode             '(require 'smartparens-haskell))
  (eval-after-load 'markdown-mode            '(require 'smartparens-markdown))
  (eval-after-load 'org                      '(require 'smartparens-org))
  (eval-after-load 'rust-mode                '(require 'smartparens-rust))
  (eval-after-load 'rustic                   '(require 'smartparens-rust))
  (eval-after-load 'scala-mode               '(require 'smartparens-scala))
  (eval-after-load 'scala-ts-mode            '(require 'smartparens-scala))
  (eval-after-load 'tex-mode                 '(require 'smartparens-latex))
  (eval-after-load 'text-mode                '(require 'smartparens-text))

  (seq-do (lambda (it)
            (eval-after-load it              '(require 'smartparens-clojure)))
          '(clojure-mode clojurescript-mode clojurec-mode))
  (seq-do (lambda (it)
            (eval-after-load it              '(require 'smartparens-html)))
          sp--html-modes)
  (seq-do (lambda (it)
            (eval-after-load it              '(require 'smartparens-latex)))
          '(latex-mode LaTeX-mode))
  (seq-do (lambda (it)
            (eval-after-load it              '(require 'smartparens-python)))
          '(python-mode python))
  (seq-do (lambda (it)
            (eval-after-load it              '(require 'smartparens-javascript)))
          '(js-ts-mode typescript-ts-mode))

  (smartparens-global-mode))

;; Tweak flymake just a little bit
(use-package flymake
  :ensure nil
  :straight nil
  :bind
  (:map
   flymake-mode-map
   ("M-g d" . #'flymake-show-buffer-diagnostics)
   ("M-g M-d" . #'flymake-show-project-diagnostics))
  :config
  (remove-hook
   'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package flycheck
  :straight t
  :hook ((prog-mode . flycheck-mode)
         (flycheck-error-list-mode . visual-line-mode))
  :bind
  (:map
   flycheck-mode-map
   ("M-g d" . #'flycheck-list-errors)))

(use-package markdown-mode
  :straight t
  :demand t
  :mode ("README\\.md\\'" . gfm-mode)
  :bind (:map markdown-mode-map ("C-c C-e" . markdown-do)))

(use-package yasnippet
  :straight t
  :config
  (yas-global-mode))

(use-package apheleia
  :straight t
  :config
  (--each '((zprint . ("zprint"))
            (refmt . ("refmt"))
            (ormolu . ("ormolu" filepath)))
    (push it apheleia-formatters))

  (--each '((clojure-mode . zprint)
            (clojurescript-mode . zprint)
            (clojurec-mode . zprint)
            (reason-mode . refmt)
            (haskell-mode . ormolu)
            (kotlin-ts-mode . ktlint))
    (push it apheleia-mode-alist)))

(use-package aggressive-indent-mode
  :straight t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package compile
  :straight (:type built-in)
  :init
  (setq compilation-scroll-output t)
  (--each '((sbt "^\\[error][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):" 1 2 3 2 1)
            (dotty "^\\[error][[:space:]]--[[:space:]].*Error: \\([^:]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 1 2 3 nil)
            (munit "^==> X .*: \\(.*\\):\\([[:digit:]]+\\)" 1 2 nil 2 1)
            (scalatest-info  "^\\[info][[:space:]]+\\(.*\\) (\\([^:[:space:]]+\\):\\([[:digit:]]+\\))" 2 3 nil 2 1)
            (scalatest-warn "^\\[warn][[:space:]][[:space:]]\\[[[:digit:]]+][[:space:]]\\([/[:word:]]:?[^:[:space:]]+\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\):" 1 2 3 1 1))
    (push it compilation-error-regexp-alist-alist))
  (--each '(sbt dotty munit scalatest-info scalatest-warn)
    (push it compilation-error-regexp-alist)))

;; ANSI color in compilation buffer
(use-package ansi-color
  :straight (:type built-in)
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter))

(use-package tramp
  :straight (:type built-in)
  :init
  (setq enable-remote-dir-locals t)
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-connection-properties
               (list nil "remote-shell" "/bin/bash")))

(provide 'development)
;;; development.el ends here
