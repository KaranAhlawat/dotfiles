(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(define-key magit-mode-map (kbd "s") 'magit-status)

(use-package project
  :config
  (defvar project-root-markers '("Cargo.toml" "mix.exs" ".project"))

  (defun korv/project-find-root (path)
    (let* ((this-dir (file-name-as-directory (file-truename path)))
           (parent-dir (expand-file-name (concat this-dir "../")))
           (system-root-dir (expand-file-name "/")))
      (cond
       ((korv/project-root-p this-dir) (cons 'transient this-dir))
       ((equal system-root-dir this-dir) nil)
       (t (korv/project-find-root parent-dir)))))

  (defun korv/project-root-p (path)
    (let ((results (mapcar (lambda (marker)
                             (file-exists-p (concat path marker)))
                           project-root-markers)))
      (eval `(or ,@ results))))

  (add-to-list 'project-find-functions #'korv/project-find-root))

(use-package flycheck
  :defer t
  :ensure t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "eslint_d"))

(use-package sly
    :defer t
    :config
    (setq inferior-lisp-program "sbcl"))

(use-package parinfer-rust-mode
  :hook
  (emacs-lisp-mode . parinfer-rust-mode)
  (lisp-mode . parinfer-rust-mode)
  (common-lisp-mode . parinfer-rust-mode)
  :init
  (setq parinfer-rust-auto-download t))

(use-package elixir-mode
  :defer t
  :config
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package flycheck-credo
  :after elixir-mode
  :config
  (flycheck-credo-setup))

(use-package alchemist
  :defer t
  :hook (elixir-mode . alchemist-mode))

(use-package js2-mode
  :mode ("\\.js\\'")
  :defer t
  :config
  (setq js2-basic-offset 2))

(use-package typescript-mode
  :mode ("\\.ts\\'")
  :defer t)

(use-package web-mode
  :mode "\\.html\\'"
  :init
  (add-to-list 'auto-mode-alist '("\\.heex\\'" . mhtml-mode))
  :config
  (setq web-mode-enable-html-entities-fontification t)
  (setq web-mode-auto-close-style 1)
  (setq web-mode-engines-alist '(("elixir" . "\\.ex\\'"))))


(use-package emmet-mode
  :hook ((web-mode . emmet-mode)
         (mhtml-mode . emmet-mode)
         (poly-elixir-web-mode . emmet-mode))

  :config
  (define-key emmet-mode-keymap (kbd "TAB") #'emmet-expand-line))

(use-package polymode
  :ensure t
  :mode ("\.ex$" . poly-elixir-web-mode)
  :config
  (define-hostmode poly-elixir-hostmode :mode 'elixir-mode)
  (define-innermode poly-live-view-expr-elixir-innermode
    :mode 'web-mode
    :head-matcher (rx line-start (* space) "~H" (= 3 (char "\"'")) line-end)
    :tail-matcher (rx line-start (* space) (= 3 (char "\"'")) line-end)
    :head-mode 'host
    :tail-mode 'host
    :allow-nested nil
    :keep-in-mode 'host
    :fallback-mode 'host)
  (define-polymode poly-elixir-web-mode
    :hostmode 'poly-elixir-hostmode
    :innermodes '(poly-live-view-expr-elixir-innermode)))

(use-package prettier
  :config
  (global-prettier-mode))

(use-package eglot
  :ensure t
  :defer t
  :init
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-auto-display-help-buffer nil)
  (setq eglot-stay-out-of '(flycheck))
  :hook
  ((elixir-mode js2-mode typescript-mode) . eglot-ensure)
  :bind
  (:map eglot-mode-map
        ("C-c l r"   . 'eglot-rename)
        ("C-c l a"   . 'eglot-code-actions)
        ("C-c l f t" . 'eglot-find-typeDefinition)
        ("C-c l f d" . 'eglot-find-declaration)
        ("C-c l f m" . 'eglot-find-implementation)
        ("C-c l q"   . 'eglot-code-action-quickfix)
        ("C-c l b"   . 'eglot-format-buffer)
        ("C-c l o"   . 'eglot-code-action-organize-imports)
        ("C-c l h"   . 'eldoc-box-eglot-help-at-point))
  :config
  (add-to-list 'eglot-ignored-server-capabilities :hoverProvider)

  (defconst eglot-eclipse-jdt-home "/home/karan/.java/eclipse.jdt.ls/org.eclipse.jdt.ls.product/target/repository/plugins/org.eclipse.equinox.launcher_1.5.700.v20200207-2156.jar")

  (defun eglot-eclipse-jdt-contact (interactive)
    (let ((cp (getenv "CLASSPATH")))
      (setenv "CLASSPATH" (concat cp ":" eglot-eclipse-jdt-home))
      (unwind-protect (eglot--eclipse-jdt-contact nil)
        (setenv "CLASSPATH" cp))))

  (setcdr (assq 'java-mode eglot-server-programs) #'eglot-eclipse-jdt-contact)

  (add-hook 'java-mode-hook 'eglot-ensure))

(use-package consult-eglot
  :defer t
  :config
  (define-key eglot-mode-map [remap xref-find-apropos] #'consult-eglot-symbols))

;; Defer Eglot shutting down servers for a few seconds

(defun eglot--defer-server-shutdown (fn &optional server)
  (cl-letf (((symbol-function #'eglot-shutdown)
             (lambda (server)
               (run-at-time 5
                            nil
                            (lambda (server)
                              (unless (eglot--managed-buffers server)
                                (eglot-shutdown server)))
                            server))))
    (funcall fn server)))

(advice-add #'eglot--managed-mode
            :around #'eglot--defer-server-shutdown)

(use-package highlight-numbers
  :defer t
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(use-package tree-sitter
  :ensure t
  :config
  (setq tree-sitter-debug-highlight-jump-region t)
  (setq tree-sitter-debug-jump-buttons t)
  (global-tree-sitter-mode +1))


(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package eldoc
  :hook
  (eglot-connect . eldoc-mode))

(use-package eldoc-box
  :commands (eldoc-box-hover-at-point-mode)
  :hook
  (eldoc-mode . eldoc-box-hover-at-point-mode))
