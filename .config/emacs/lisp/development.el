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

(use-package git-gutter
  :straight t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :straight t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(use-package lsp-mode
  :straight (lsp-mode :type git :host github :repo "emacs-lsp/lsp-mode")
  :commands (lsp lsp-deferred)
  :hook ((typescript-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (scala-ts-mode . lsp-deferred)
         (elixir-ts-mode . lsp-deferred)
         (heex-ts-mode . lsp-deferred)
         (lsp-completion-mode . conf/lsp-mode-completion-setup))
  :init
  (defun conf/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  
  (defun conf/lsp-mode-completion-setup ()
    (if (seq-contains-p '("clojure-mode" "clojurescript-mode" "clojurec-mode" "cider-mode")
                        major-mode)
        (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
              '(cider fussy))
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            '(fussy))))

  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))

  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))   ;; native json-rpc
               (not (seq-contains-p orig-result "ocamllsp")) ;; OCAML LSP
               (executable-find "emacs-lsp-booster"))
          (progn
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (append '("emacs-lsp-booster" "--json-false-value" ":json-false" "--") orig-result))
        orig-result)))

  (setq lsp-keymap-prefix "C-c C-c")
  (setq lsp-completion-provider :capf)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-enable-links t)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-idle-delay 1.0)
  (setq lsp-semantic-tokens-enable t)
  (setq lsp-semantic-tokens-enable-multiline-token-support t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-modeline-code-actions-enable nil)

  (require 'lsp-javascript)
  (setq lsp-typescript-preferences-import-module-specifier "non-relative")

  (require 'lsp-elixir)
  (setq lsp-elixir-server-command '("/home/karan/repos/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))

  (require 'lsp-roslyn)
  (setq lsp-roslyn-server-dll-override-path "/home/karan/.local/bin/roslyn/Microsoft.CodeAnalysis.LanguageServer.dll")

  ;; Mine are better :)
  (setq lsp-disabled-clients '(emmet-ls eslint))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("emmet-language-server" "--stdio"))
    :activation-fn (lsp-activate-on "elixir" "eruby" "html" "css" "less" "javascriptreact" "typescriptreact" "phoenix-heex")
    :priority -1
    :add-on? t
    :multi-root t
    :server-id 'emmet))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/home/karan/repos/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))
                    :multi-root t
                    :activation-fn (lsp-activate-on "elixir" "phoenix-heex")
                    :server-id 'lexical))

  :config
  (push '(heex-ts-mode . "phoenix-heex") lsp-language-id-configuration)
  
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)

  (keymap-set lsp-mode-map "C-c C-c" lsp-command-map)

  (lsp-enable-which-key-integration t))

(use-package lsp-tailwindcss
  :straight t
  :init
  (setq lsp-tailwindcss-add-on-mode t)
  :config
  (push 'heex-ts-mode lsp-tailwindcss-major-modes))

(use-package dap-mode
  :straight t
  :after lsp-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package lsp-metals
  :straight t
  :custom
  (lsp-metals-fallback-scala-version "3.3.3")
  (lsp-metals-enable-indent-on-paste t)
  (lsp-metals-enable-semantic-highlighting t))

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :straight nil
  :hook lsp-mode
  :custom
  (eldoc-echo-area-use-multiline-p 1)
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0.15))

(use-package eldoc-box
  :straight t
  :after (eldoc)
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
  :init
  (setq sp-highlight-pair-overlay nil)
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

(use-package flycheck
  :straight t
  :hook ((prog-mode . flycheck-mode)
         (flycheck-error-list-mode . visual-line-mode))
  :bind
  (:map
   flycheck-mode-map
   ("M-g d" . #'flycheck-list-errors))

  :init
  (setq flycheck-javascript-eslint-executable "eslint_d")

  :config
  (with-eval-after-load 'doom-themes
    (custom-set-faces
     `(flycheck-error ((t :underline (:style line :color ,(doom-color 'red)))))
     `(flycheck-warning ((t :underline (:style line :color ,(doom-color 'yellow)))))
     `(flycheck-info ((t :underline (:style line :color ,(doom-color 'cyan))))))))

(use-package flycheck-deno
  :straight t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-deno-setup)))

(use-package flycheck-kotlin
  :straight t
  :config
  (flycheck-kotlin-setup)
  (flycheck-add-next-checker 'lsp 'kotlin-ktlint))

(use-package eslintd-fix
  :straight t
  :hook ((js-ts-mode . eslintd-fix-mode)
         (typescript-ts-mode . eslintd-fix-mode)
         (tsx-ts-mode . eslintd-fix-mode)))

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
            (scalafmt . ("scalafmt-native"
                         (when-let* ((project (project-current))
                                     (root (project-root project)))
                           (list "--config" (expand-file-name ".scalafmt.conf" root)))
                         "--stdin")))
    (push it apheleia-formatters))

  (--each '((clojure-mode . zprint)
            (clojurescript-mode . zprint)
            (clojurec-mode . zprint)
            (kotlin-ts-mode . ktlint)
            (php-ts-mode . phpcs)
            (scala-ts-mode . scalafmt))
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
            (scalatest-info   "^\\[info][[:space:]]+\\(.*\\) (\\([^:[:space:]]+\\):\\([[:digit:]]+\\))" 2 3 nil 2 1)
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

(use-package which-func
  :straight (:type built-in)
  :hook (prog-mode . which-function-mode))

(use-package opam-switch-mode
  :straight t)

(provide 'development)
;;; development.el ends here
