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
  :hook (((heex-ts-mode
           elixir-ts-mode
           scala-ts-mode
           java-ts-mode
           js-ts-mode
           tsx-ts-mode
           typescript-ts-mode
           smithy-mode) . lsp-deferred)
         (lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         (lsp-completion-mode . conf/lsp-mode-completion-setup))
  :preface
  (defun conf/lsp-mode-completion-setup ()
    (setf (caadr ;; Pad before lsp modeline error info
				   (assq 'global-mode-string mode-line-misc-info))
				  " ")
    ;; (if (seq-contains-p '("clojure-mode" "clojurescript-mode" "clojurec-mode" "cider-mode" "clojure-ts-mode")
    ;;                     major-mode)
    ;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
    ;;           '(cider orderless))
    ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
    ;;         '(orderless)))
    )

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
  :custom
  (lsp-completion-provider :none)
  (lsp-diagnostics-provider :flycheck)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-keep-workspace-alive t)
  (lsp-enable-links t)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-text-document-color nil)
  (lsp-idle-delay 0.5)
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-semantic-tokens-enable nil)
  (lsp-semantic-tokens-enable-multiline-token-support nil)
  (lsp-eldoc-render-all t)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-signature-doc-lines 2)
  ;; Mine are better :)
  (lsp-disabled-clients '(emmet-ls eslint))
  :init
  (use-package lsp-elixir
    :straight nil
    :after lsp-mode
    :custom
    (lsp-elixir-server-command '("/home/karan/repos/lexical/_build/dev/package/lexical/bin/start_lexical.sh")))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("/home/karan/repos/lexical/_build/dev/package/lexical/bin/start_lexical.sh"))
                    :multi-root t
                    :activation-fn (lsp-activate-on "elixir" "phoenix-heex")
                    :server-id 'lexical))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("cs"
                                                            "launch"
                                                            "software.amazon.smithy:smithy-language-server:0.4.0"
                                                            "--ttl"
                                                            "1h"
                                                            "--repository"
                                                            "m2Local"
                                                            "--main-class"
                                                            "software.amazon.smithy.lsp.Main"
                                                            "--"
                                                            "0"))
                    :multi-root nil
                    :activation-fn (lsp-activate-on "smithy")
                    :initialization-options '((statusBarProvider . "show-message")
                                              (isHttpEnabled . t))
                    :server-id 'smithy-ls))

  :config
  (push '("\\.smithy$" . "smithy") lsp-language-id-configuration)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)

  (use-package evil-core
    :straight nil
    :after evil
    :config
    (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
    (add-hook 'lsp-mode-hook #'evil-normalize-keymaps))

  (lsp-enable-which-key-integration t))

(use-package lsp-metals
  :straight t
  :custom
  (lsp-metals-server-args
   '("-Dmetals.allow-multiline-string-formatting=on"
     "-Dmetals.enable-best-effort=true"
     "-Dmetals.client=emacs"))
  (lsp-metals-fallback-scala-version "3.3.3")
  (lsp-metals-enable-indent-on-paste t)
  (lsp-metals-enable-semantic-highlighting nil))

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :straight nil
  :hook lsp-mode
  :custom
  (eldoc-echo-area-use-multiline-p 1)
  (eldoc-echo-area-display-truncation-message t)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
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
  :hook ((clojure-mode emacs-lisp-mode lisp-mode) . smartparens-strict-mode)
  :init
  (setq sp-highlight-pair-overlay nil)
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode)
  (sp-use-smartparens-bindings)
  (smartparens-global-mode))

(use-package flycheck
  :straight t
  :hook ((prog-mode . flycheck-mode)
         (flycheck-error-list-mode . visual-line-mode))
  :bind
  (:map
   flycheck-mode-map
   ("M-g d" . #'flycheck-list-errors))
  :custom
  (flycheck-javascript-eslint-executable "eslint_d"))

(use-package flycheck-deno
  :straight t
  :config
  (with-eval-after-load 'flycheck
    (flycheck-deno-setup)))

(use-package flycheck-kotlin
  :straight t
  :after 'lsp-mode
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

(use-package aggressive-indent
  :straight t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

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

(use-package emmet-mode
  :straight t
  :hook ((js-base-mode typescript-ts-base-mode) . emmet-mode))

(use-package editorconfig
  :straight t
  :hook (prog-mode . editorconfig-mode))

(provide 'development)
;;; development.el ends here
