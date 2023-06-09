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

;; Now that eglot is in Emacs itself, that is a huge incentive to move
;; away from lsp-mode, at least for me.
(use-package eglot
  :straight nil
  :ensure nil
  :custom
  (eglot-autoshutdown t)
  (eglot-send-changes-idle-time 0.2)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)

  :config
  (dolist (mode
           '(c-ts-mode-hook
             c++-ts-mode-hook
             typescript-ts-mode-hook
             tsx-ts-mode-hook
             js-ts-mode-hook
             python-ts-mode-hook
             java-ts-mode-hook
             csharp-ts-mode-hook
             elixir-ts-mode-hook
             heex-ts-mode-hook
             php-mode-hook
             dart-mode-hook
             clojure-mode-hook
             clojurescript-mode-hook
             clojurec-mode-hook))
    (add-hook mode 'eglot-ensure))

  (add-to-list
   'eglot-server-programs
   '(php-mode . ("intelephense" "--stdio")))

  (add-to-list
   'eglot-server-programs
   '(scala-ts-mode . ("metals" "-Dmetals.extensions=false"
                      :initializationOptions ( :compilerOptions ( :snippetAutoIndent :json-false
                                                                  :overrideDefFormat "unicode")
                                               :icons "unicode"
                                               :statusBarProvider "log-message"
                                               :isHttpEnabled t
                                               :treeViewProvider :json-false))))

  (dolist (mode '(js-ts-mode typescript-ts-mode tsx-ts-mode))
    (let ((lang-id
           (cond
            ((eq mode 'js-ts-mode)
             "javascript")
            ((eq mode 'typescript-ts-mode)
             "typescript")
            ((eq mode 'tsx-ts-mode)
             "typescriptreact"))))
      (add-to-list
       'eglot-server-programs
       `((,mode :language-id ,lang-id)
         .
         ,(eglot-alternatives
           '(("vtsls" "--stdio")
             ("typescript-language-server" "--stdio")))))))

  (setq-default eglot-workspace-configuration
                '( :vtsls ( :experimental ( :completion ( :enableServerSideFuzzyMatch t
                                                          :entriesLimit 200 )))
                   :pylsp ( :plugins ( :jedi_completion ( :include_params t
                                                          :fuzzy t)
                                       :mypy ( :live_mode :json-false
                                               :dmypy t)
                                       :ruff ( :enabled t
                                               :lineLength 100)
                                       :black ( :enabled t
                                                :line_length 100)))
                   :gopls ( :usePlaceholders t
                            :staticcheck t
                            :matcher "Fuzzy")
                   :dart ( :completeFunctionCalls t
                           :enableSnippets t)
                   :metals ( :superMethodLensesEnabled t
                             :showInferredType t
                             :showImplicitArguments t
                             :showImplicitConversionsAndClasses t
                             :bloopSbtAlreadyInsatlled t))))

;; Eldoc for documentation
(use-package eldoc
  :ensure nil
  :straight nil
  :hook eglot-ensure
  :custom
  (eldoc-echo-area-use-multiline-p 2)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0.25))

(use-package smartparens
  :straight t
  :hook ((clojure-mode . smartparens-strict-mode)
         (clojurescript-mode . smartparens-strict-mode)
         (clojurec-mode . smartparens-strict-mode))
  :init (smartparens-global-mode))

;; Tweak flymake just a little bit
(use-package flymake
  :ensure nil
  :straight nil
  :hook prog-mode
  :bind
  (:map
   flymake-mode-map
   ("M-g d" . #'flymake-show-buffer-diagnostics)
   ("M-g M-d" . #'flymake-show-project-diagnostics))
  :config
  (remove-hook
   'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

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
  :init
  (dolist (fmt '((scalafmt . ("scalafmt"
                              "--stdin"))
                 (zprint . ("zprint"))))
    (push fmt apheleia-formatters))

  (dolist (mapping '((scala-ts-mode . scalafmt)
                     (clojure-mode . zprint)
                     (clojurescript-mode . zprint)
                     (clojurec-mode . zprint)))
    (push mapping apheleia-mode-alist)))

(use-package aggressive-indent-mode
  :straight t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package sql
  :straight (:type built-in)
  :init
  (setq sql-postgres-login-params
        (append sql-postgres-login-params '(port))))

;; ANSI color in compilation buffer
(use-package ansi-color
  :straight (:type built-in)
  :config
  (defun colorize-compilation-buffer ()
    (read-only-mode 'toggle)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 'toggle))
  
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(provide 'development)
;;; development.el ends here
