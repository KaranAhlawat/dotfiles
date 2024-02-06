;;; languages.el --- Setup programming specific stuff -*- lexical-binding: t; -*-
;;; Commentary:
;;; This is just mostly language related stuff.
;;; Code:

(require 'dash)

(use-package treesit
  :straight nil
  :demand t
  :custom
  (treesit-font-lock-level 4)
  :config
  (--each '((typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src" nil nil))
            (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src" nil nil))
            (css . ("https://github.com/tree-sitter/tree-sitter-css" nil nil nil nil))
            (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" nil nil nil nil))
            (json . ("https://github.com/tree-sitter/tree-sitter-json" nil nil nil nil))
            (python . ("https://github.com/tree-sitter/tree-sitter-python" nil nil nil nil))
            (go . ("https://github.com/tree-sitter/tree-sitter-go"  nil nil nil))
            (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" nil nil  nil))
            (java . ("https://github.com/tree-sitter/tree-sitter-java" nil nil nil))
            (kotlin . ("https://github.com/fwcd/tree-sitter-kotlin" nil nil nil))
            (scala . ("https://github.com/tree-sitter/tree-sitter-scala" nil nil nil)))
    (push it treesit-language-source-alist))

  (--each '((python-mode . python-ts-mode)
            (c-mode . c-ts-mode)
            (csharp-mode . csharp-ts-mode)
            (c++-mode . c++-ts-mode)
            (javascript-mode . js-ts-mode)
            (java-mode . java-ts-mode)
            (css-mode . css-ts-mode)
            (sh-mode . bash-ts-mode)
            (scala-mode . scala-ts-mode)
            (shell-script-mode . bash-ts-mode))
    (push it major-mode-remap-alist)))

(use-package ts-query-highlight
  :straight (:type git :host sourcehut :repo "meow_king/ts-query-highlight")
  :config
  ;; default is `dabbrev-expand`. For `cape-dabbrev`, take a look at https://github.com/minad/cape
  (setq ts-query-highlight-dabbrev-expand-function 'cape-dabbrev))

;; Cider for clojure
(use-package cider
  :straight t
  :custom
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-display-help-banner nil)
  (cider-shadow-cljs-command "npx shadow-cljs"))

;; PHP
(use-package php-mode
  :straight t
  :mode "\\.php\\'")

;; Web Stuff
(use-package web-mode
  :straight t
  :mode (("\\.blade\\.php\\'" . web-mode)
         ("\\.html?\\'" . web-mode))
  :custom
  (web-mode-enable-auto-pairing t)
  (web-mode-css-indent-offset 2)
  (web-mode-sql-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-markup-comment-indent-offset 2)
  (web-mode-attr-indent-offset 2)
  (web-mode-attr-value-indent-offset 2)
  :config
  (setq web-mode-engines-alist '(("blade" . "\\.blade\\.")))
  (add-hook 'web-mode-hook (lambda ()
                             (smartparens-mode -1))))

(use-package js
  :straight (:type built-in)
  :custom
  (js-indent-level 2))

(use-package python
  :straight (:type built-in)
  :config
  (setq
   python-indent-offset 4
   python-indent-guess-indent-offset nil
   python-indent-guess-indent-offset-verbose nil))

(use-package micromamba
  :straight t)

(use-package css-mode
  :straight (:type built-in)
  :custom
  (css-indent-offset 2))

(use-package scss-mode
  :straight (:type built-in)
  :mode "\\.scss\\'"
  :custom (css-indent-offset 2))

(use-package java-ts-mode
  :straight (:type built-in)
  :custom
  (java-ts-mode-indent-offset 2))

(use-package typescript-ts-mode
  :straight (:type built-in)
  :mode (("\\.jsx\\'" . tsx-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :init
  (setq typescript-ts-mode-indent-offset 4))

;; Scala TS (local)
(use-package scala-ts-mode
  :mode "\\.scala\\'"
  :straight (:local-repo "/home/karan/repos/scala-ts-mode"))

;; Haskell
(use-package haskell-mode
  :straight t)

;; Astro
(use-package astro-ts-mode
  :straight t
  :hook (astro-ts-mode . (lambda ()
                           (jinx-mode -1))))

(use-package kotlin-ts-mode
  :straight t
  :mode ("\\.kt\\'" "\\.kts\\'")
  :custom
  (kotlin-ts-mode-indent-offset 2))

(use-package yaml-ts-mode
  :straight (:type built-in))

(provide 'languages)
;;; languages.el ends here
